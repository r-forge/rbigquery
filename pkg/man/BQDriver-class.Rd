\name{BQDriver-class}
\docType{class}
\alias{BQDriver-class}
\alias{dbDisconnect,BQConnection-method}
\alias{dbGetException,BQConnection-method}
\alias{dbListFields,BQConnection-method}
\alias{dbListResults,BQConnection-method}

\title{Class "BQDriver"}
\description{
    Singleton holding some configuration options and open connections.
    For RBigQuery, not extremely important other than to conform to DBI.
}
\section{Objects from the Class}{
Objects can be created by calls of the form 
    \code{BigQuery()} or \code{dbDriver{"BigQuery"}}
}
\section{Extends}{
Class \code{"\linkS4class{DBIDriver}"}, directly.
Class \code{"\linkS4class{BQObject}"}, directly.
Class \code{"\linkS4class{DBIObject}"}, by class "DBIDriver", distance 2.
Class \code{"\linkS4class{dbObjectId}"}, by class "BQObject", distance 2.
}
\section{Methods}{
  \describe{
    \item{dbConnect}{\code{
        signature(drv = "BQDriver", username = "character", 
            password="character"): ... }}
    \item{dbGetInfo}{\code{
        signature(dbObj = "BQDriver", what="character")}: ... }
    \item{dbListConnections}{\code{
        signature(drv = "BQDriver"): ... }}
    \item{dbListFields}{\code{
        signature(drv = "BQDriver", table="character"): ... }}
    \item{summary}{\code{
        signature(object = "BQDriver", verbose="logical"): ... }}
	 }
}
\seealso{
    \code{\link{BQObject-class}}
    \code{\link[DBI]{DBIDriver-class}}
    \code{\link{BQConnection-class}}
    \code{\link{dbConnect-methods}}
    \code{\link{dbGetInfo-methods}}
    \code{\link{dbListConnections}}
    \code{\link{summary}}
}
\examples{ \dontrun{
    drv <- dbDriver("BigQuery")
    conn <- dbConnect(drv, "username", "password")
}}
\keyword{classes}
