\name{bqDescribeTable}
\alias{bqDescribeTable}
\title{
    Get table metadata from remote server.
}
\description{
    Equivalent to the describe command in bq, asks for metadata regarding
    a BigQuery table - primarily column name and type
}
\usage{
bqDescribeTable(con, table, what = "", verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{con}{
      Connection to BigQuery to use.
}
  \item{table}{
      The name of the table to describe
}
  \item{what}{
      Optional: A specific tag whose value to return
}
  \item{verbose}{
      Print extra debugging output
}
  \item{\dots}{
      Currently unimplemented
}
}
\value{
    \item{kind}{Type of query - should be bigquery#table}
    \item{name}{name of the table}
    \item{fields}{List of fields in the table and their types.
        Note: this can produce very unhelpful output - for column names, using
        bqListFields is recommended.}
}
\seealso{
    \code{\link{BQConnection-class}}
}
\examples{ \dontrun{
    metadata <- bqDescribeTable(conn, "bigquery/samples/shakespeare")
}}
\keyword{table}
