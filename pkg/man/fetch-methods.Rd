\name{fetch-methods}
\docType{methods}
\alias{fetch-methods}
\alias{fetch,BQResult,missing-method}
\alias{fetch,BQResult,numeric-method}
\title{ Fetch data out of an BQResult object }
\description{
    Retrive data from a BQResult in the form of a data.frame
}
\section{Methods}{
\describe{

\item{\code{signature(res = "BQResult", n = "missing")}}{
    Retrieves as many rows of the data as possible.
}

\item{\code{signature(res = "BQResult", n = "numeric")}}{
    Retrieves the first n rows of data. 
}
}}

\examples{ \dontrun{
    result <- dbSendQuery(conn, "SELECT word FROM [bigquery/samples/shakespeare]
                                 WHERE word CONTAINS 'raisin'")
    data <- fetch(result)
}}

\seealso{
    \code{\link{BQResult-class}}
    \code{\link{dbSendQuery-methods}}
}

\keyword{methods}
