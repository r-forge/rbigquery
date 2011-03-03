\name{dbListFields-methods}
\docType{methods}
\alias{dbListFields-methods}
\alias{dbListFields,BQConnection,character-method}
\title{ Lists table column names }
\description{
    Returns a list of the column names for a given table. Uses bqDescribeTable
    to get metadata
}
\section{Methods}{
\describe{

\item{\code{signature(conn = "BQConnection", name = "character")}}{
    name is the character name of the table, with without square brackets.
}
}}

\examples{ \dontrun{
    dbListFields(conn, "bigquery/samples/shakespeare")
}}

\seealso{
    \code{\link{bqDescribeTable}}
    \code{\link{BQConnection-class}}
}
\keyword{methods}
