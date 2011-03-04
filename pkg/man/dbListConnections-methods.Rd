\name{dbListConnections-methods}
\docType{methods}
\alias{dbListConnections-methods}
\alias{dbListConnections,BQDriver-method}
\title{ List open connections}
\description{
    Lists all open and query-able connections that are associated with a 
    provided driver.

    Synonomous to \code{dbGetInfo(driver, what="connections")}
}
\section{Methods}{
\describe{

\item{\code{signature(drv = "BQDriver")}}{
    Returns a list of BQConnections
}
}}
\seealso{
\code{\link{BQDriver-class}}
\code{\link{BQConnection-class}}
\code{\link{dbGetInfo-methods}}
}

\keyword{methods}
