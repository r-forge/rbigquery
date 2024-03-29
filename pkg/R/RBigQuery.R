## RBigQuery.R      David Xiao      2011-1-24

## 
## This project is being developed as part of a UROP under the MIT CSAIL Advanced
## Network Architectures Group.
## Thanks to the authors of the RMySQL and RPostgreSQL packages, upon which much of
## the code here is based. 
##

########################################
## CONSTANTS

.BQPkgName <- "RBigQuery"
.BQVersion <- "0.5.3"
.BQLoginURL <- "https://www.google.com/accounts/ClientLogin"
.BQService <- "ndev"
.BQSource <- "RBigQuery"
.BQGetpoint <- "https://www.googleapis.com/bigquery/v1/"
.BQEndpoint <- "https://www.googleapis.com/rpc"

bq.map.type <- list(
                "string"="character",
                "character"="string",
                "integer"="integer",
                "float"="numeric",
                "numeric"="float",
                "boolean"="logical",
                "logical"="boolean")

########################################
## DBIObject Class

setClass("BQObject", representation("DBIObject", "dbObjectId", "VIRTUAL"))

########################################
## dbDriver Method

BigQuery <- function(max.con=1, fetch.default.rec = 10000, force.reload = FALSE)
{
    if (fetch.default.rec<=0)
        stop("default number of records per fetch must be positive")
    if (max.con<=0)
        stop("maxiumum open connections must be positive")

    getBQDriver(max.con, fetch.default.rec, force.reload)
}

########################################
## DBIDriver Class

setClass("BQDriver", representation("DBIDriver", "BQObject",
    connections="list", fetch.default.rec="integer", max.con="integer",
    state="environment"))


#setAs("BQObject", "BQDriver",
#    def = function(from) new("BQDriver", Id = as(from, "integer")[1:2])
#)

setMethod("dbGetInfo", "BQDriver",
    def = function(dbObj, ...) bqDriverInfo(dbObj, ...)
)

setMethod("dbListConnections", "BQDriver",
    def = function(drv, ...) dbGetInfo(drv, "connections")
)

setMethod("summary", "BQDriver",
    def = function(object, ...) bqDescribeDriver(object, ...)
)

# not included: dbUnloadDriver

########################################
## DBIConnection Class

## State of connection: authToken

setClass("BQConnection", representation("DBIConnection", "BQObject",
    username="character", password="character", driver="BQDriver",
    auth.token="character", state="environment")
)

setMethod("dbConnect", "BQDriver",
    def = function(drv, ...) bqNewConnection(drv, ...),
    valueClass = "BQConnection"
)

setMethod("dbConnect", "BQConnection",
    def = function(drv, ...) .NotYetImplemented(),
    valueClass = "BQConnection"
)

setMethod("dbGetInfo", "BQConnection",
    def = function(dbObj, ...) bqConnectionInfo(dbObj, ...)
)

setMethod("summary", "BQConnection",
    def = function(object, ...) bqDescribeConnection(object, ...)
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

setMethod("dbListFields",
    signature(conn="BQConnection", name="character"),
    def = function(conn, name, ...) bqListFields(conn, name, ...),
    valueClass = "character"
)

setMethod("dbReadTable",
    signature(conn="BQConnection", name="character"),
    def = function(conn, name, ...) bqReadTable(conn, name, ...),
    valueClass = "data.frame"
)

setMethod("dbGetException", "BQConnection",
    def = function(conn, ...) bqGetException(conn, ...),
    valueClass = "list"
)

setMethod("dbListResults", "BQConnection",
    def = function(conn, ...) dbGetInfo(conn, "lastresult")[[1]]
)

#######################################
## DBIResult Class

setClass("BQResult", representation("DBIResult", "BQObject",
    connection="BQConnection", statement="character", 
    success="logical", fields="data.frame", result="data.frame")
)

setMethod("dbGetInfo", "BQResult",
    def = function(dbObj, ...) bqResultInfo(dbObj, ...),
    valueClass = "list"
)

setMethod("summary", "BQResult",
    def = function(object, ...) bqDescribeResult(object, ...)
)

setMethod("dbColumnInfo", "BQResult",
    def = function(res, ...) dbGetInfo(res, "fields"),
    valueClass = "data.frame"
)

setMethod("dbGetStatement", "BQResult",
    def = function(res, ...) dbGetInfo(res, "statement"),
    valueClass = "character"
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

setMethod("dbClearResult", "BQResult",
    def = function(res, ...) bqCloseResult(res, ...),
    valueClass = "logical"
)

if (FALSE) { ########################### END OF CODE

########################################
## DBIConnection Class

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

setMethod("dbWriteTable",
            signature(conn="BQConnection", name="character"),
            def = function(conn, name, value, ...) bqWriteTable(conn, name, value, ...),
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

#######################################
## DBIResult Class

setAs("BQResult", "BQConnection",
        def = function(from) new("BQConnection", Id = as(from, "integer")[1:3])
        )
setAs("BQResult", "BQDriver",
        def = function(from) new ("BQDriver", Id = as(from, "integer")[1:2])
        )

# Not included: dbGetRowsAffected, dbGetRowCount, dbHasCompleted,
} ### END OF OLD CODE
