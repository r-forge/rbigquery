\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,BQConnection-method}
\alias{summary,BQDriver-method}
\alias{summary,BQResult-method}
\title{ pretty-print object information }
\description{
    prints a human-readable string describing the object and its data
}
\section{Methods}{
\describe{

\item{\code{signature(object = "BQDriver")}}{
    Will give driver name, records per fetch, max allowed connections, number
    of processed connections, number of open connections. If verbose is set,
    the connection ids, and DBI and BQ versions.
}

\item{\code{signature(object = "BQConnection")}}{
    Will give driver, username, and object id of last result returned from
    connection. If verbose is set, will give the length of password, the
    stored authorization token, and the BQ client version.
}

\item{\code{signature(object = "BQResult")}}{
    Will give connection, statement queried, a flag indicating success or
    failure, and if success, the field names returned, if failure, the
    error message. If verbose is set, it will return the data as well.
}
}}

\arguments{
    \item{verbose}{(Optional) flag which will print additional information
    about the object}
}

\examples{ \dontrun{
    summary(driver)
    summary(conn, TRUE)
    summary(result, verbose=TRUE)
}}

\seealso{
    \code{\link{BQDriver-class}}
    \code{\link{BQConnection-class}}
    \code{\link{BQResult-class}}
}

\keyword{methods}
