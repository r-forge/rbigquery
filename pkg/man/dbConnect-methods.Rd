\name{dbConnect-methods}
\docType{methods}
\alias{dbConnect-methods}
\alias{dbConnect,BQConnection-method}
\alias{dbConnect,BQDriver-method}
\title{ Connect to the BigQuery server}
\description{
    Authenticates with the BigQuery server and creates a BQConnection
}
\section{Methods}{
\describe{

\item{\code{signature(drv = "BQDriver", username="character", password="character")}}{
    Connects to and authenticates with the Google servers, then creates a
    BQConnection object which can then be used to query the BigQuery servers.
}

\item{\code{signature(drv = "BQDriver", username="character", auth.token="character")}}{
    Takes the authentication token provided as an argument and creates a
    BQConnection object with can then be used to query to BigQuery servers.
    N.B.: RBigQuery does not actually verify to see if the token is correct -
    you'll have to query BigQuery yourself to find out.
}

\item{\code{signature(drv = "BQConnection")}}{
    Should clone the provided BQConnection object, not implemented for now.
}
}}

\arguments{
    \item{username}{Your google username: e.g. john.doe@gmail.com or just john.doe}
    \item{password}{Your google password.}
}

\value{
    A BQConnection object which can then be used to query BigQuery.
}

\examples{ \dontrun{
    driver <- BigQuery()
    conn <- dbConnect(driver, username="username", password="password")
    data <- dbGetQuery(conn, "insert query here")
}}

\seealso{
    \code{\link{BQConnection-class}}
}
\keyword{methods}
