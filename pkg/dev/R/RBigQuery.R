## RBigQuery.R      David Xiao      2010-12-14

## 
## This project is being developed as part of a UROP under the MIT CSAIL Advanced
## Network Architectures Group.
## Thanks to the authors of the RMySQL and RPostgreSQL packages, upon which much of
## the code here is based. 
##

##TODO: It may be useful at a later date to add custom summary methods to classes

########################################
## DBIObject Class

#setClass("BQObject", representation("DBIObject", "dbObjectId", "VIRTUAL"))
setClass("BQObject", representation("DBIObject", Id="integer", "VIRTUAL"))

########################################
## dbDriver Method

# At some point, may be useful to implement force.reload argument (flag which
# reloads driver and close all open connections)
#TODO: at some point, make the resulting class a singleton
BigQuery <- function(max.con=1, fetch.default.rec = 1000)
{
    if (fetch.default.rec<=0)
        stop("default number of records per fetch must be positive")
    new("BQDriver", Id = as.integer(0))
}

########################################
## DBIDriver Class

setClass("BQDriver", representation("DBIDriver", "BQObject"))

if (FALSE) {
setAs("BQObject", "BQDriver",
        def = function(from) new("BQDriver", Id = as(from, "integer")[1:2])
        )

setMethod("dbGetInfo", "BQDriver",
            def = function(dbObj, ...) bqDriverInfo(dbObj, ...)
            )

setMethod("dbListConnections", "BQDriver",
            def = function(driver, ...) dbGetInfo(driver, "connectionIds")[[1]]
            )

# not included: dbUnloadDriver, summary

########################################
## DBIConnection Class

## State of connection: authToken

setClass("BQConnection", representation("DBIConnection", "BQObject"))

setMethod("dbConnect", "BQDriver",
            def = function(driver, ...) bqNewConnection(driver, ...),
            valueClass = "BQConnection"
            )

setMethod("dbConnect", "character",
            def = function(driver, ...) bqNewConnection(dbDriver(driver), ...),
            valueClass = "BQConnection"
            )

setMethod("dbConnect", "BQConnection",
            def = function(driver, ...) bqCloneConnection(driver, ...),
            valueClass = "BQConnection"
            )

setMethod("dbDisconnect", "BQConnection",
            def = function(conn, ...) bqCloseConnection(conn, ...),
            valueClass = "logical"
            )

setMethod("dbSendQuery", 
            signature(conn = "BQConnection", statement = "character"),
            def = function(conn, statement, ...) bqExecStatement(conn, statement, ...),
            valueClass = "BQResult"
            )

setMethod("dbGetQuery",
            signature(conn = "BQConnection", statement = "character"),
            def = function(conn, statement, ...) bqQuickStatement(conn, statement, ...),
            )

setMethod("dbGetException", "BQConnection",
            def = function(conn, ...) {
                #if(!isIdCurrent(conn))
                #    stop(paste("expired", class(conn)))
                #.Call("RS_MySQL_getException", as(conn, "integer"),
                #    PACKAGE = .MySQLPkgName
            },
            valueClass = "list"
            )

setMethod("dbGetInfo", "BQConnection",
            def = function(dbObj, ...) bqConnectionInfo(dbObj, ...)
            )

setMethod("dbListResults", "BQConnection",
            def = function(conn, ...) dbGetInfo(conn, "rsId")[[1]]
            )

# not included: summary

###################
## Convenience methods

setMethod("dbListTables", "BQConnection",
            def = function(conn, ...) 
            {
                #output <- dbGetQuery(conn, "show talbes")
                #if (is.null(output) || nrow(out) == 0)
                #    output <- character(0)
                #else
                #    out <- out[, 1]
                #out

                ## BigQuery does not yet support a listing tables
                out <- NULL
                out
            },
            valueClass = "character"
            )

setMethod("dbReadTable",
            signature(conn="BQConnection", name="character"),
            def = function(conn, name, ...) bqReadTable(conn, name, ...),
            valueClass = "data.frame"
            )

setMethod("dbWriteTable",
            signature(conn="BQConnection", name="character"),
            def = function(conn, name, value, ...) bqWriteTalbe(conn, name, value, ...),
            valueClass = "logical"
            )

setMethod("dbExistsTable",
            signature(conn="BQConnection", name="character"),
            def = function(conn, name, ...)
            {
                #TODO: find appropirate query to BigQuery
                tables <- dbListTables(conn)
                if (length(tables) == 0)
                    tables <- ""
                math(tolower(name), tolower(tables), nomatch=0)>0
            },
            valueClass = "logical"
            )

setMethod("dbRemoveTable",
            signature(conn="BQConnection", name="character"),
            def = function(conn, name, ...) 
            {
                #TODO: understand just what the RMySQL folks are doing with this...
                if(dbExistsTable(conn, name))
                {
                   rc <- try(dbGetQuery(conn, paste("DROP TABLE", name)))
                   !inherits(rc, ErrorClass)
                } 
                else FALSE
            },
            valueClass = "logical"
)

setMethod("dbListFields",
            signature(conn="BQConnection", name="character"),
            def = function(conn, name, ...)
            {
                fields <- dbGetQuery(conn, paste("describe", name))[,1]
                if (length(fields) == 0)
                    fields <- character()
                fields
            },
            valueClass = "character"
        )

#######################################
## DBIResult Class

setClass("BQResult", representation("DBIResult", "BQObject"))

setAs("BQResult", "BQConnection",
        def = function(from) new("BQConnection", Id = as(from, "integer")[1:3])
        )
setAs("BQResult", "BQDriver",
        def = function(from) new ("BQDriver", Id = as(from, "integer")[1:2])
        )

setMethod("dbClearResult", "BQResult",
            def = function(res, ...) bqCloseResult(res, ...),
            valueClass = "logical"
            )

setMethod("fetch", signature(res="BQResult", n="numeric"),
            def = function(res, n, ...)
            {
                out <- bqFetch(res, n, ...)
                if(is.null(out))
                    out <- data.frame(out)
                out
            },
            valueClass = "data.frame"
            )

setMethod("fetch", signature(res="BQResult", n="missing"),
            def = function(res, n=0, ...)
            {
                out <- bqFetch(res, n, ...)
                if(is.null(out))
                    out <- data.frame(out)
                out
            },
            valueClass = "data.frame"
            )

setMethod("dbGetinfo", "BQResult",
            def = function(dbObj, ...) bqResultInfo(dbObj, ...),
            valueClass = "list"
            )

setMethod("dbColumnInfo", "BQResult",
            def = function(res, ...) bqDescribeFields(res, ...),
            valueClass = "data.frame"
            )

# Not included: dbGetStatement, dbGetRowsAffected, dbGetRowCount, dbHasCompleted,
# summary
}
