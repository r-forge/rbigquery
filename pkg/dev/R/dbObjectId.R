## dbObjectId.R       David Xiao      2010-12-29

## 
## This project is being developed as part of a UROP under the MIT CSAIL Advanced
## Network Architectures Group.
## Thanks to the authors of the RMySQL and RPostgreSQL packages, upon which much of
## the code here is based. 
##

##
## This mixin helper class is NOT part of the database interface definition,
## but it is extended by the Oracle, MySQL, and SQLite implementations to
## MySQLObject and OracleObject to allow us to conviniently (and portably) 
## implement all database foreign objects methods (i.e., methods for show(), 
## print() format() the dbManger, dbConnection, dbResultSet, etc.) 
## A dbObjectId is an  identifier into an actual remote database objects.  
## This class and its derived classes <driver-manager>Object need to 
## be VIRTUAL to avoid coercion (green book, p.293) during method dispatching.
##
## TODO: Convert the Id slot to be an external object (as per Luke Tierney's
## implementation), even at the expense of S-plus compatibility?

setClass("dbObjectId", representation(Id = "integer", "VIRTUAL"))

## coercion methods 
setAs("dbObjectId", "integer", 
   def = function(from) as(slot(from,"Id"), "integer")
)
setAs("dbObjectId", "numeric",
   def = function(from) as(slot(from, "Id"), "integer")
)
setAs("dbObjectId", "character",
   def = function(from) as(slot(from, "Id"), "character")
)   

## formating, showing, printing,...
setMethod("format", "dbObjectId", 
   def = function(x, ...) {
      paste("(", paste(as(x, "integer"), collapse=","), ")", sep="")
   },
   valueClass = "character"
)

setMethod("show", "dbObjectId", def = function(object) print(object))

setMethod("print", "dbObjectId",
   def = function(x, ...){
      expired <- if(isIdCurrent(x)) "" else "Expired "
      str <- paste("<", expired, class(x), ":", format(x), ">", sep="")
      cat(str, "\n")
      invisible(NULL)
   }
)

isIdCurrent <- function(obj)
## verify that obj refers to a currently open/loaded database
{ 
   ## This method should be converted into a generic as soon as possible
   #obj <- as(obj, "integer")
   #.Call("RS_DBI_validHandle", obj, PACKAGE = .MySQLPkgName)
   TRUE
}

