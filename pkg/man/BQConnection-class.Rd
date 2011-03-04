\name{BQConnection-class}
\docType{class}
\alias{BQConnection-class}

\title{Class "BQConnection"}
\description{
    Holds auth information to connect to BigQuery.
}
\section{Objects from the Class}{
Objects can be created by calls of the form 
    \code{dbConnect(BigQuery(), "username@gmail.com", "password")}
}
\section{Extends}{
Class \code{"\linkS4class{DBIConnection}"}, directly.
Class \code{"\linkS4class{BQObject}"}, directly.
Class \code{"\linkS4class{DBIObject}"}, by class "DBIConnection", distance 2.
Class \code{"\linkS4class{dbObjectId}"}, by class "BQObject", distance 2.
}
\section{Methods}{
  \describe{
    \item{dbConnect}{\code{
        signature(drv = "BQConnection", username="character",
            password="character"): ... }}
    \item{dbDisconnect}{\code{
        signature(conn = "BQConnection"): ... }}
    \item{dbGetException}{\code{
        signature(conn = "BQConnection"): ... }}
    \item{dbGetInfo}{\code{
        signature(dbObj = "BQConnection"): ... }}
    \item{dbSendQuery}{\code{
        signature(conn = "BQConnection", statement = "character",
            verbose="logical", status="logical"): ... }}
    \item{dbGetQuery}{\code{
        signature(conn = "BQConnection", statement = "character"): ... }}
    \item{dbListFields}{\code{
        signature(conn = "BQConnection", name = "character"): ... }}
    \item{dbListResults}{\code{
        signature(conn = "BQConnection"): ... }}
    \item{dbReadTable}{\code{
        signature(conn = "BQConnection", name = "character"): ... }}
    \item{summary}{\code{
        signature(object = "BQConnection"): ... }}
	 }
}
\examples{ \dontrun{
    conn <- dbConnect(drv, username="username", password="password")
    result <- dbGetQueyr(conn, "SELECT word FROM [bigquery/samples/shakespeare]
                                WHERE word CONTAINS 'raisin'")
}}
\keyword{classes}
