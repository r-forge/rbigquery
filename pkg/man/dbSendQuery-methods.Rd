\name{dbSendQuery-methods}
\docType{methods}
\alias{dbSendQuery-methods}
\alias{dbSendQuery,BQConnection,character-method}
\title{ Query the database }
\description{
    Sends a query to the BigQuery database and returns a BQResult object
    describing it.
}
\section{Methods}{
\describe{

\item{\code{signature(conn = "BQConnection", statement = "character"), ...}}{
    Returns a BQResult. Use fetch() to retrieve full results.
}
}}

\arguments{
    \item{statement}{character string of query. See BigQuery query reference for syntax}
    \item{status}{(Optional) flag to print status messages about the state of execution - recommended}
    \item{verbose}{(Optional) flag to print all debugging information during the query - do not set unless you know what you're doing}
}

\examples{ \dontrun{
    conn <- dbConnect(driver, "username", "password")
    result <- dbSendQuery(conn, "SELECT word FROM [bigquery/samples/shakespeare]
                                 WHERE word CONTAINS 'raisin'", status=TRUE)
    data <- fetch(result)
}}

\seealso{
    \code{\link{BQConnection-class}}
    \code{\link{BQResult-class}}
    \code{\link{dbGetQuery-methods}}
    \code{\link{fetch-methods}}
    \code{\link{dbReadTable-methods}}
    \code{\link{dbGetInfo-methods}}
}
\keyword{methods}
