\name{BigQuery}
\alias{BigQuery}
\title{
    Creates a BigQuery driver.
}
\description{
    Creates a BigQuery driver, or if the driver is already created and the
    \code{force.reload} flag is not set, returns the current BigQuery driver.
    (drivers are singletons)
}
\usage{
BigQuery(max.con = 1, fetch.default.rec = 10000, force.reload = FALSE)
}
\arguments{
  \item{max.con}{
      Maximum number of connections BigQuery can drive at once.
}
  \item{fetch.default.rec}{
      Default number of result rows to fetch at a time
}
  \item{force.reload}{
      Force the driver to restart itself.
}
}
\details{
    the current arguments to BigQuery() are not yet set to do anything. Don't
    expect them to do _anything_ to the way RBigQuery operates.
}
\value{
    Returns a driver object, which can then be used to create a connection.
}
\seealso{
    \code{\link[DBI]{DBIDriver-class}}
}
\examples{\dontrun{
    driver <- BigQuery()
    ## Alternatively, use
    driver <- dbDriver("BigQuery")
}}
\keyword{BigQuery }
