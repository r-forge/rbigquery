\name{dbReadTable-methods}
\docType{methods}
\alias{dbReadTable-methods}
\alias{dbReadTable,BQConnection,character-method}
\title{ Query all columns of a table }
\description{
    Queries over all columns of a table. 
}
\section{Methods}{
\describe{

\item{\code{signature(conn = "BQConnection", name = "character"), ...}}{
    returns a data.field of the results of the query "SELECT * FROM [name] <addend>;".
}
}}

\arguments{
    \item{name}{name of the table to query - do not enclose in sqaure brackets}
    \item{addend}{(Optional) argument for adding additional clauses to the query string.}
}

\examples{ \dontrun{
    dbReadTable(conn, "bigquery/samples/shakespeare")

    dbReadTable(conn, "bigquery/samples/shakespeare", "WHERE word CONTAINS 'th'")
}}

\seealso{
    \code{\link{dbListFields-methods}}
    \code{\link{dbGetQuery-methods}}
    \code{\link{dbSendQuery-methods}}
    \code{\link{BQConnection-class}}
}
\keyword{methods}
