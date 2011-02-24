\name{dbDriver-methods}
\docType{methods}
\alias{dbDriver-methods}
\alias{dbDriver,character-method}
\title{ constructs the DBI driver }
\description{
    initializes the appropriate driver from the driver string given.
}
\section{Methods}{
\describe{

\item{\code{signature(drvName = "character"), ...}}{
    calls the initialization function appropriate to driver name specified.
    Syntactically equal to \code{drvName(...)}
}
}}

\arguments{
    \item{drvName}{
        should be the character name of the driver to initiate. in this
        package, "BigQuery"}
}
\value{
    a DBIDriver object for the specified database. In BigQuery's case, a
    BQDriver. Note that BQDriver is a singleton; only one object of its
    class should be instantiated at a time.
}

\seealso{
    \code{\link{BQDriver-class}}
    \code{\link{BigQuery}}
}
\keyword{methods}
