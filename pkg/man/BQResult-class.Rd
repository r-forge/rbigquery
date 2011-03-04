\name{BQResult-class}
\Rdversion{1.1}
\docType{class}
\alias{BQResult-class}
\alias{dbClearResult,BQResult-method}
\alias{dbColumnInfo,BQResult-method}
\alias{dbGetStatement,BQResult-method}

\title{Class "BQResult"}
\description{
    Incapsulates information regarding a specific query, including query
    statement, result, and result metadata.
}
\section{Objects from the Class}{
Results can be created by querying the BigQuery database, either with
    \code{dbSendQuery(conn, ...)} or \code{dbGetQuery(conn, ...)}
}
\section{Extends}{
Class \code{"\linkS4class{DBIResult}"}, directly.
Class \code{"\linkS4class{BQObject}"}, directly.
Class \code{"\linkS4class{DBIObject}"}, by class "DBIResult", distance 2.
Class \code{"\linkS4class{dbObjectId}"}, by class "BQObject", distance 2.
}
\section{Methods}{
  \describe{
    \item{dbClearResult}{\code{signature(res = "BQResult"): ... }}
    \item{dbColumnInfo}{\code{signature(res = "BQResult"): ... }}
    \item{dbGetInfo}{\code{signature(dbObj = "BQResult", what="character"): ... }}
    \item{dbGetStatement}{\code{signature(res = "BQResult"): ... }}
    \item{fetch}{\code{signature(res = "BQResult"): ... }}
    \item{summary}{\code{signature(object = "BQResult", what="character"): ... }}
	 }
}
\examples{ \dontrun{
    result <- dbSendQuery(conn, "SELECT word FROM [bigquery/samples/shakespeare]
                                 WHERE word CONTAINS 'raisin'")
    summary(result)
    data <- fetch(result)
    
    ## or to get data directly, use
    data <- dbGetQuery(conn, "SELECT word FROM [bigquery/samples/shakespeare]
                              WHERE word CONTAINS 'raisin'")
}}
\keyword{classes}
