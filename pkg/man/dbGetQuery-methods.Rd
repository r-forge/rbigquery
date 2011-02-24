\name{dbGetQuery-methods}
\docType{methods}
\alias{dbGetQuery-methods}
\alias{dbGetQuery,BQConnection,character-method}
\title{ query and return }
\description{
    Executes a query, parses the result. Returns data.frame if successful,
    FALSE if not.
}
\section{Methods}{
\describe{

\item{\code{signature(conn = "BQConnection", statement = "character")}}{
}
}}
\arguments{
    \item{conn}{BQConnection object to send query through}
    \item{statement}{character string of statement to execute}
}

\examples{ \dontrun{
    data <- dbGetQuery(conn, "SELECT word FROM [bigquery/samples/shakespeare] WHERE word CONTAINS 'raisin'")
}}

\seealso{
    \code{\link{dbSendQuery-methods}}
    \code{\link{fetch}}
    \code{\link{BQConnection-class}}
}
\keyword{methods}
