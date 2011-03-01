## BigQuerySupport.R       David Xiao      2011-1-24

## 
## This project is being developed as part of a UROP under the MIT CSAIL
## Advanced Network Architectures Group.
## Thanks to the authors of the RMySQL and RPostgreSQL packages, upon which 
## much of the code here is based. 
##

## Why on earth does R not have this built in?
trim <- function(x) gsub("^[[:space:]]+|[[:space:]]+$", "", x) 

generateBQId <- local(
{
    id <- 0
    function() {
        id <<- id + 1
        as.integer(id)
    }
})

getBQDriverConstructor <- function ()
{
    constructed <- FALSE
    driver <- 0
    function (max.con=1, fetch.default.rec = 10000, force.reload = FALSE)
    {
        if (constructed == FALSE || force.reload == TRUE)
        {
            env <- new.env(parent=emptyenv())
            env$connections = list()
            env$prc.con = as.integer(0)
            driver <<- new("BQDriver", 
                            Id = as.integer(0),
                            connections = list(),
                            fetch.default.rec = as.integer(fetch.default.rec),
                            max.con = as.integer(max.con),
                            state = env
                          )
            constructed <<- TRUE
        }
        driver
    }
}

# BQDriver is a singleton
getBQDriver <- getBQDriverConstructor()

bqDescribeDriver <- function(obj, verbose = FALSE, ...)
## Print out nicely a brief description of the connection Driver
{
    info <- dbGetInfo(obj)
    print(obj)
    cat("  Driver name: ", info$drvName, "\n")
    cat("  Default records per fetch:", info$fetch.default.rec, "\n")
    cat("  Max  connections:", info$max.con, "\n")
    cat("  Conn. processed :", info$prc.con, "\n")
    cat("  Open connections:", length(info$connections), "\n")
    if(verbose && !is.null(info$connections)){
        for(i in seq(along = info$connections)){
            cat("   ", i, " ")
            print(info$connections[[i]])
        }
    }
    if(verbose){
        cat("  DBI API version: ", dbGetDBIVersion(), "\n")
        cat("  BQ client version: ", info$clientVersion, "\n")
    }
    invisible(NULL)
}

bqDriverInfo <- function(dbObj, what="", ...)
{
    if(!isIdCurrent(dbObj))
        stop(paste("expired", class(dbObj)))
    info <- list()
    info$drvName <- .BQPkgName
    info$managerId <- dbObj@Id
    info$fetch.default.rec <- dbObj@fetch.default.rec
    info$max.con <- dbObj@max.con
    info$prc.con <- dbObj@state$prc.con
    info$connections <- dbObj@state$connections
    info$clientVersion <- .BQVersion
    if(!missing(what))
        info[what]
    else
        info
}

bqNewConnection <- function(drv, username=NULL, password=NULL, auth.token=NULL)
{
    if(!isIdCurrent(drv))
        stop("expired manager")

    if (!is.null(username) && !is.character(username))
        stop("Argument username must be a string")

    if (length(drv@state$connections) >= drv@max.con)
        stop("Driver has too many open connections")

    if (!is.null(auth.token) || !is.character(auth.token))
    {
        connection <- new("BQConnection", username=username,
                                password="", driver=drv,
                                auth.token=auth.token, Id=generateBQId(),
                                state=new.env(parent=globalenv()))

        drv@state$connections[connection@Id] <- connection
        connection
    }
    else
    {
        if (!is.null(password) && !is.character(password))
            stop("Argument password must be a string or NULL")

        result = postForm(.BQLoginURL,
            "accountType" = "HOSTED_OR_GOOGLE",
            "Email" = username,
            "Passwd" = password,
            service = .BQService,
            source = .BQSource)

        result.lines = strsplit(result, '\n')[[1]]

        if (grepl("Error=", result))
        {
            stop(paste("Login", result.lines[grep("Error=", result.lines)]))
        }
        else if (grepl("Auth=", result))
        {
            auth.token <- substring(result.lines[grep("Auth=", result.lines)],
                                    nchar("Auth=")+1)
            connection <- new("BQConnection", username=username, 
                                password=password, driver=drv, 
                                auth.token=auth.token, Id=generateBQId(),
                                state=new.env(parent=globalenv()))

            drv@state$connections[connection@Id] <- connection
            connection
        }
        else
        {
            stop("Unknown Error: Could not login")
        }
    }
}

bqDescribeConnection <- function(obj, verbose = FALSE, ...)
{
    info <- dbGetInfo(obj)
    print(obj)
    cat("  Driver: ")
    print(info$driver)
    cat("  Username:", info$username, "\n")
    cat("  Last Result: ")
    print(info$last.result)
    if(verbose){
       cat("  Password:", info$password, "\n")
       cat("  Auth Token:", info$auth.token, "\n")
       cat("  BigQuery client version: ", 
           dbGetInfo(info$driver, what="clientVersion")[[1]], "\n")
    }
    invisible(NULL)
}

bqConnectionInfo <- function(obj, what="", ...)
{
    if(!isIdCurrent(obj))
       stop(paste("expired", class(obj), deparse(substitute(obj))))
    info <- list()
    info$driver <- obj@driver
    info$username <- obj@username
    info$password <- length(obj@password)
    info$auth.token <- obj@auth.token
    info$last.result <- obj@state$last.result
    #rsId <- vector("list", length = length(info$rsId))
    #for(i in seq(along = info$rsId))
    #    rsId[[i]] <- new("MySQLResult", Id = c(id, info$rsId[i]))
    #info$rsId <- rsId
    if(!missing(what))
        info[what]
    else
        info
}

bqCloseConnection <- function(con, ...)
{
    if(!isIdCurrent(con))
       return(TRUE)
    driver <- con@driver
    driver@state$connections[con@Id] <- NULL
    driver@state$prc.con <- as.integer(driver@state$prc.con + 1)
    con@auth.token <- ""
    return(TRUE)
}

bqConvertFactorToType <- function(factor, type)
{
    # leaving characters as factors for now
    if (type == "integer")
    {
        as.integer(levels(factor)[factor])
    }
    else if (type == "numeric")
    {
        as.numeric(levels(factor)[factor])
    }
    else if (type == "logical")
    {
        as.logical(levels(factor)[factor])
    }
    else
    {
        factor
    }
}

bqExecStatement <- function(con, statement, verbose=FALSE, status=FALSE)
## submits the sql statement to BQ and creates a
## dbResult object
{
    if(!isIdCurrent(con))
        stop(paste("expired", class(con)))
    statement <- as(statement, "character")

    auth.text <- paste("GoogleLogin auth=", con@auth.token, sep="")
    json.in <- toJSON(list(params=list(q=statement),method="bigquery.query"))
    if (verbose)
        cat(" JSON Input: ", json.in, "\n")
    if (status)
        print("Sending query...")
    options <- list(httpheader=list(Authorization=auth.text,
                        "Content-type"="application/json"),
                        postfields=json.in)
    out.text <- postForm(.BQEndpoint, .opts=options)
    if (verbose)
        print(out.text)
    if (status)
        print("Received response, parsing...")

    if (typeof(out.text) == "raw")
        json.out <- rawToChar(out.text)
    else
        stop(paste("ERROR:\n", out.text))

    results <- fromJSON(json.out)
    if (verbose)
        cat(" JSON Output: ", json.out, "\n")
    if (status)
        print("Parsed response, arranging...")

    if (is.null(results$error))
    {
        success <- TRUE
        ## THIS COMMAND TAKES TOO LONG TO RUN
        rows <- ldply(results$result$rows, data.frame)
        if (status)
            print("Converting data types...")
        fields <- ldply(results$result$fields, data.frame)
        names(rows) <- as.character(fields[["id"]])
        types <- as.character(fields[["type"]])
        if (verbose)
            print(data.frame(fields))
        for (index in {1:length(rows)})
        {
            if (verbose)
            {
                print(index)
                cat(format(types[[index]]), " -> ", 
                    format(bq.map.type[types[[index]]]), "\n")
                print(rows[[index]])
            }

            rows[[index]] <- bqConvertFactorToType(rows[[index]],
                                bq.map.type[types[[index]]])

            if (verbose)
                print(rows[[index]])
        }
        if (status)
            print("Query Complete")
        result <- rows
    }
    else
    {
            if (status)
                print("Query Error")
        success <- FALSE
        fields <- data.frame()
        result <- data.frame(results$error$data)
    }
    
    bqresult <- new("BQResult", Id=generateBQId(), connection=con,
            statement=statement, success=success, fields=fields, result=result)
    con@state$last.result <- bqresult
    bqresult
}

## helper function: it exec's *and* retrieves a statement. It should
## be named something else.
bqQuickStatement <- function(con, statement)
{
    if(!isIdCurrent(con))
        stop(paste("expired", class(con)))
    result <- dbSendQuery(con, statement)
    if (result@success)
    {
         return(result@result)
    }
    else
    {
         return(FALSE)
    }
}

bqGetException <- function (conn, ...)
{
    last.result <- conn@state$last.result
    if (last.result$success)
    {
        list("no exceptions found")
    }
    else
    {
        list(last.result$result)
    }
}

bqResultInfo <- function(obj, what = "", ...)
{
    if(!isIdCurrent(obj))
       stop(paste("expired", class(obj), deparse(substitute(obj))))
    info <- list()
    info$connection = obj@connection
    info$statement = obj@statement
    info$success = obj@success
    info$fields = obj@fields
    if (info$success)
        info$result = obj@result
    else
        info$error = as.list(obj@result)
    if(!missing(what))
        info[what]
    else
        info
}

bqDescribeResult <- function(obj, verbose = FALSE, ...)
{

    if(!isIdCurrent(obj)){
       print(obj)
       invisible(return(NULL))
    }
    print(obj)
    info <- dbGetInfo(obj)
    cat("  Connection:", format(info$connection), "\n")
    print(info$connection)
    cat("  Statement:", info$statement, "\n")
    cat("  Success:", info$success, "\n")
    if (! info$success)
        cat("  Error:", format(as.list(info$error)), "\n")
    else if (verbose)
    {
        cat("  Result:\n")
        print(data.frame(info$result))
    }
    else
    {
        cat("  Fields:\n")
        print(info$fields)
    }
#   cat("  Statement:", dbGetStatement(obj), "\n")
#   cat("  Has completed?", if(dbHasCompleted(obj)) "yes" else "no", "\n")
#   cat("  Affected rows:", dbGetRowsAffected(obj), "\n")
#   cat("  Rows fetched:", dbGetRowCount(obj), "\n")
#   flds <- dbColumnInfo(obj)
#   if(verbose && !is.null(flds)){
#      cat("  Fields:\n")  
#      out <- print(dbColumnInfo(obj))
#   }
    invisible(NULL)
}

bqFetch <- function(res, n=0, ...)
## TODO: Make sure we don't exhaust all the memory, or generate
## an object whose size exceeds option("object.size").  Also,
## are we sure we want to return a data.frame?
{    
    res@result
}

bqDescribeTable <- function (con, table, what="", verbose=FALSE,...)
{
    auth.text <- paste("GoogleLogin auth=", con@auth.token, sep="")
    httpheader <-list("Authorization"=auth.text,
                        "Content-type"="application/json",
                        "Accept"="application/json")
    url <- paste(.BQGetpoint, 'tables/', curlEscape(table), sep="")
    json.out <- getURL(url, httpheader=httpheader, v=verbose)

    if (verbose)
        print(json.out)

    results <- fromJSON(json.out)
    if (verbose)
    {
        cat(" JSON Output: ", json.out, "\n")
        print(data.frame(results))
    }
    
    if (what)
    {
        results$data[what]
    }
    else
    {
        results$data
    }
}

bqListFields <- function (con, table, verbose=FALSE, ...)
{
    fields <- bqDescribeTable(con, table, verbose)$fields
    lst <- c()
    for (field in fields)
    {
        if (!is.null(field$fields))
        {
            lst <- c(lst, bqRecurseIds(field$fields, field$id))
        }
        else
        {
            lst <- c(lst, field$id)
        }
    }
    lst
}

bqRecurseIds <- function (fields, prefix)
{
    lst <- c()
    for (field in fields)
    {
        if (!is.null(field$fields))
        {
            lst <- c(lst, bqRecurseIds(field$fields, 
                                         paste(prefix, field$id, sep=".")))
        }
        else
        {
            lst <- c(lst, paste(prefix, field$id, sep="."))
        }
    }
    lst
}

bqReadTable <- function (conn, name, ...)
{
    columns <- dbListFields(conn, name)
    query <- paste("SELECT ", paste(columns, collapse=", "), 
                    " FROM [", name, "];", sep="")
    print(query)
    data <- dbGetQuery(conn, query) 

    data
}

bqCloseResult <- function(res, ...)
{
    if (res@connection@state$last.result@Id == res@Id)
    {
        res@connection@state$last.result <- NULL
    }
    res@fields <- data.frame()
    res@result <- data.frame()
    TRUE
}

if (FALSE) { ########################### END OF CODE

bqCloseDriver <- function(drv, ...)
{
    if (!isIdCurrent(drv))
        return(1)
    # close specified driver
    return(0)
}

"mysqlDBApply" <-
function(res, INDEX, FUN = stop("must specify FUN"), 
         begin = NULL, 
         group.begin =  NULL, 
         new.record = NULL, 
         end = NULL, 
         batchSize = 100, maxBatch = 1e6, 
         ..., simplify = TRUE)
## (Experimental)
## This function is meant to handle somewhat gracefully(?) large amounts 
## of data from the DBMS by bringing into R manageable chunks (about 
## batchSize records at a time, but not more than maxBatch); the idea
## is that the data from individual groups can be handled by R, but
## not all the groups at the same time.  
##
## dbApply apply functions to groups of rows coming from a remote
## database resultSet upon the following fetching events: 
##   begin         (prior to fetching the first record)
##   group.begin   (the record just fetched begins a new group)
##   new_record    (a new record just fetched)
##   group.end     (the record just fetched ends the current group)
##   end           (the record just fetched is the very last record)
##
## The "begin", "begin.group", etc., specify R functions to be
## invoked upon the corresponding events.  (For compatibility 
## with other apply functions the arg FUN is used to specify the
## most common case where we only specify the "group.end" event.)
## 
## The following describes the exact order and form of invocation for the
## various callbacks in the underlying  C code.  All callback functions
## (except FUN) are optional.
##  begin()
##    group.begin(group.name)   
##    new.record(df.record)
##    FUN(df.group, group.name, ...)   (aka group.end)
##  end()
##
## TODO: (1) add argument output=F/T to suppress the creation of
##           an expensive(?) output list.
##       (2) allow INDEX to be a list as in tapply()
##       (3) add a "counter" event, to callback every k rows
##       (3) should we implement a simplify argument, as in sapply()?
##       (4) should it report (instead of just warning) when we're forced
##           to handle partial groups (groups larger than maxBatch).
##       (5) extend to the case where even individual groups are too
##           big for R (as in incremental quantiles).
##       (6) Highly R-dependent, not sure yet how to port it to S-plus.
{
   if(dbHasCompleted(res))
      stop("result set has completed")
   if(is.character(INDEX)){
      flds <- tolower(as.character(dbColumnInfo(res)$name))
      INDEX <- match(tolower(INDEX[1]), flds, 0)
   }
   if(INDEX<1)
      stop(paste("INDEX field", INDEX, "not in result set"))

   "null.or.fun" <- function(fun) # get fun obj, but a NULL is ok 
   {
      if(is.null(fun)) 
         fun 
      else 
         match.fun(fun)
   }
   begin <- null.or.fun(begin)
   group.begin <- null.or.fun(group.begin)
   group.end <- null.or.fun(FUN)     ## probably this is the most important
   end <- null.or.fun(end)
   new.record <- null.or.fun(new.record)
   rsId <- as(res, "integer")
   con <- as(res, "MySQLConnection")
   on.exit({
      rc <- dbGetException(con)
      if(!is.null(rc$errorNum) && rc$errorNum!=0)
         cat("dbApply aborted with MySQL error ", rc$errorNum,
             " (", rc$errorMsg, ")\n", sep = "")

      })
   ## BEGIN event handler (re-entrant, only prior to reading first row)
   if(!is.null(begin) && dbGetRowCount(res)==0) 
      begin()
   rho <- environment()
   funs <- list(begin = begin, end = end,
                group.begin = group.begin,
                group.end = group.end, new.record = new.record)
   out <- .Call("RS_MySQL_dbApply",
            rs = rsId,
        INDEX = as.integer(INDEX-1),
        funs, rho, as.integer(batchSize), as.integer(maxBatch),
                PACKAGE = .MySQLPkgName)
   if(!is.null(end) && dbHasCompleted(res))
      end()
   out
}

"mysqlImportFile" <-
function(con, name, value, field.types = NULL, overwrite = FALSE, 
  append = FALSE, header, row.names, nrows = 50, sep = ",", 
  eol="\n", skip = 0, quote = '"', ...)
{
  if(overwrite && append)
    stop("overwrite and append cannot both be TRUE")

  ## Do we need to clone the connection (ie., if it is in use)?
  if(length(dbListResults(con))!=0){ 
    new.con <- dbConnect(con)              ## there's pending work, so clone
    on.exit(dbDisconnect(new.con))
  } 
  else 
    new.con <- con

  if(dbExistsTable(con,name)){
    if(overwrite){
      if(!dbRemoveTable(con, name)){
        warning(paste("table", name, "couldn't be overwritten"))
        return(FALSE)
      }
    }
    else if(!append){
      warning(paste("table", name, "exists in database: aborting dbWriteTable"))
      return(FALSE)
    }
  }

  ## compute full path name (have R expand ~, etc)
  fn <- file.path(dirname(value), basename(value))
  if(missing(header) || missing(row.names)){
    f <- file(fn, open="r")
    if(skip>0) 
      readLines(f, n=skip)
    txtcon <- textConnection(readLines(f, n=2))
    flds <- count.fields(txtcon, sep)
    close(txtcon)
    close(f)
    nf <- length(unique(flds))
  }
  if(missing(header)){
    header <- nf==2
  }
  if(missing(row.names)){
    if(header)
      row.names <- if(nf==2) TRUE else FALSE
    else
      row.names <- FALSE
  }

  new.table <- !dbExistsTable(con, name)
  if(new.table){
    ## need to init table, say, with the first nrows lines
    d <- read.table(fn, sep=sep, header=header, skip=skip, nrows=nrows, ...)
    sql <- 
      dbBuildTableDefinition(new.con, name, obj=d, field.types = field.types,
        row.names = row.names)
    rs <- try(dbSendQuery(new.con, sql))
    if(inherits(rs, ErrorClass)){
      warning("could not create table: aborting mysqlImportFile")
      return(FALSE)
    } 
    else 
      dbClearResult(rs)
  }
  else if(!append){
    warning(sprintf("table %s already exists -- use append=TRUE?", name))
  }

  fmt <- 
     paste("LOAD DATA LOCAL INFILE '%s' INTO TABLE  %s ",
           "FIELDS TERMINATED BY '%s' ",
           if(!is.null(quote)) "OPTIONALLY ENCLOSED BY '%s' " else "",
           "LINES TERMINATED BY '%s' ",
           "IGNORE %d LINES ", sep="")
  if(is.null(quote))
     sql <- sprintf(fmt, fn, name, sep, eol, skip + as.integer(header))
  else
     sql <- sprintf(fmt, fn, name, sep, quote, eol, skip + as.integer(header))

  rs <- try(dbSendQuery(new.con, sql))
  if(inherits(rs, ErrorClass)){
     warning("could not load data into table")
     return(FALSE)
  } 
  dbClearResult(rs)
  TRUE
}

"mysqlWriteTable" <-
function(con, name, value, field.types, row.names = TRUE, 
   overwrite = FALSE, append = FALSE, ..., allow.keywords = FALSE)
## Create table "name" (must be an SQL identifier) and populate
## it with the values of the data.frame "value"
## TODO: This function should execute its sql as a single transaction,
##       and allow converter functions.
## TODO: In the unlikely event that value has a field called "row_names"
##       we could inadvertently overwrite it (here the user should set 
##       row.names=F)  I'm (very) reluctantly adding the code re: row.names,
##       because I'm not 100% comfortable using data.frames as the basic 
##       data for relations.
{
   if(overwrite && append)
      stop("overwrite and append cannot both be TRUE")
   if(!is.data.frame(value))
      value <- as.data.frame(value)
   if(row.names){
      value <- cbind(row.names(value), value)  ## can't use row.names= here
      names(value)[1] <- "row.names"
   }
   if(missing(field.types) || is.null(field.types)){
      ## the following mapping should be coming from some kind of table
      ## also, need to use converter functions (for dates, etc.)
      field.types <- lapply(value, dbDataType, dbObj = con)
   } 

   ## Do we need to coerce any field prior to write it out?
   ## TODO: MySQL 4.1 introduces the boolean data type.  
   for(i in seq(along = value)){
      if(is(value[[i]], "logical"))
         value[[i]] <- as(value[[i]], "integer")
   }
   i <- match("row.names", names(field.types), nomatch=0)
   if(i>0) ## did we add a row.names value?  If so, it's a text field.
      field.types[i] <- dbDataType(dbObj=con, field.types$row.names)
   names(field.types) <- make.db.names(con, names(field.types), 
                             allow.keywords = allow.keywords)
   ## Do we need to clone the connection (ie., if it is in use)?
   if(length(dbListResults(con))!=0){ 
      new.con <- dbConnect(con)              ## there's pending work, so clone
      on.exit(dbDisconnect(new.con))
   } 
   else {
      new.con <- con
   }

   if(dbExistsTable(con,name)){
      if(overwrite){
         if(!dbRemoveTable(con, name)){
            warning(paste("table", name, "couldn't be overwritten"))
            return(FALSE)
         }
      }
      else if(!append){
         warning(paste("table",name,"exists in database: aborting mysqlWriteTable"))
         return(FALSE)
      }
   } 
   if(!dbExistsTable(con,name)){      ## need to re-test table for existence 
      ## need to create a new (empty) table
      sql1 <- paste("create table ", name, "\n(\n\t", sep="")
      sql2 <- paste(paste(names(field.types), field.types), collapse=",\n\t",
                          sep="")
      sql3 <- "\n)\n"
      sql <- paste(sql1, sql2, sql3, sep="")
      rs <- try(dbSendQuery(new.con, sql))
      if(inherits(rs, ErrorClass)){
         warning("could not create table: aborting mysqlWriteTable")
         return(FALSE)
      } 
      else 
         dbClearResult(rs)
   }

   ## TODO: here, we should query the MySQL to find out if it supports
   ## LOAD DATA thru pipes; if so, should open the pipe instead of a file.

   fn <- tempfile("rsdbi")
   fn <- gsub("\\\\", "/", fn)  # Since MySQL on Windows wants \ double (BDR)
   safe.write(value, file = fn)
   on.exit(unlink(fn), add = TRUE)
   sql4 <- paste("LOAD DATA LOCAL INFILE '", fn, "'",
                  " INTO TABLE ", name, 
                  " LINES TERMINATED BY '\n' ", 
                  "( ", paste(names(field.types), collapse=", "), ");",
               sep="")
   rs <- try(dbSendQuery(new.con, sql4))
   if(inherits(rs, ErrorClass)){
      warning("could not load data into table")
      return(FALSE)
   } 
   else 
      dbClearResult(rs)
   TRUE
}

"dbBuildTableDefinition" <-
function(dbObj, name, obj, field.types = NULL, row.names = TRUE, ...)
{
  if(!is.data.frame(obj))
    obj <- as.data.frame(obj)
  if(!is.null(row.names) && row.names){
    obj  <- cbind(row.names(obj), obj)  ## can't use row.names= here
    names(obj)[1] <- "row.names" 
  }
  if(is.null(field.types)){
    ## the following mapping should be coming from some kind of table
    ## also, need to use converter functions (for dates, etc.)
    field.types <- lapply(obj, dbDataType, dbObj = dbObj)
  } 
  i <- match("row.names", names(field.types), nomatch=0)
  if(i>0) ## did we add a row.names value?  If so, it's a text field.
    field.types[i] <- dbDataType(dbObj, field.types$row.names)
  names(field.types) <- 
    make.db.names(dbObj, names(field.types), allow.keywords = FALSE)

  ## need to create a new (empty) table
  flds <- paste(names(field.types), field.types)
  paste("CREATE TABLE", name, "\n(", paste(flds, collapse=",\n\t"), "\n)")
}

## the following is almost exactly from the ROracle driver 
"safe.write" <- 
function(value, file, batch, ...)
## safe.write makes sure write.table doesn't exceed available memory by batching
## at most batch rows (but it is still slowww)
{  
   N <- nrow(value)
   if(N<1){
      warning("no rows in data.frame")
      return(NULL)
   }
   digits <- options(digits = 17)
   on.exit(options(digits))
   if(missing(batch) || is.null(batch))
      batch <- 10000
   else if(batch<=0) 
      batch <- N
   from <- 1 
   to <- min(batch, N)
   conb <- file(file,open="wb")
   while(from<=N){
      write.table(value[from:to,, drop=FALSE], file = conb, append = TRUE, 
            quote = FALSE, sep="\t", na = .MySQL.NA.string,
            row.names=FALSE, col.names=FALSE, eol = '\n', ...)
      from <- to+1
      to <- min(to+batch, N)
   }
   close(conb)
   invisible(NULL)
}

"mysqlDataType" <-
function(obj, ...)
## find a suitable SQL data type for the R/S object obj
## TODO: Lots and lots!! (this is a very rough first draft)
## need to register converters, abstract out MySQL and generalize 
## to Oracle, Informix, etc.  Perhaps this should be table-driven.
## NOTE: MySQL data types differ from the SQL92 (e.g., varchar truncate
## trailing spaces).  MySQL enum() maps rather nicely to factors (with
## up to 65535 levels)
{
   rs.class <- data.class(obj)    ## this differs in R 1.4 from older vers
   rs.mode <- storage.mode(obj)
   if(rs.class=="numeric" || rs.class == "integer"){
      sql.type <- if(rs.mode=="integer") "bigint" else  "double"
   } 
   else {
      sql.type <- switch(rs.class,
                     character = "text",
                     logical = "tinyint",  ## but we need to coerce to int!!
                     factor = "text",      ## up to 65535 characters
                     ordered = "text",
                     "text")
   }
   sql.type
}

## the following code was kindly provided ny J. T. Lindgren.
"mysqlEscapeStrings" <-
function(con, strings)
{
  ## Escapes the given strings
  if(!isIdCurrent(con))
     stop(paste("expired", class(con)))
  strings <- as(strings, "character")
  conId <- as(con, "integer");
  out <- .Call("RS_MySQL_escapeStrings", conId, strings,
       PACKAGE = .MySQLPkgName)
  names(out) <- names(strings)
  out
}

} ### END OF OLD CODE
