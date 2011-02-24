\name{RBigQuery-package}
\alias{RBigQuery-package}
\alias{RBigQuery}
\docType{package}
\title{
BigQuery for R
}
\description{
A DBI extension for querying and parsing results from Google's BigQuery 
database.
}
\section{Overview}{
    \enumerate{
    \item Connect and authenticate with Google:
        \preformatted{
conn <- dbConnect(BigQuery(), username="bquser@gmail.com",
                  password="pass")
        }
    \item Get table fields:
        \preformatted{
info <- dbListFields(conn, "bigquery/samples/shakespeare")
        }
    \item Send a query and if successful get results as data.frame:
        \preformatted{
result <- dbSendQuery(conn, "SELECT word FROM 
    [bigquery/samples/shakespeare] WHERE word CONTAINS 'raisin'")
if (result@success)
    fetch(result)
else
    dbGetInfo(result, "error")
        }
    \item Get information about connection, results, etc.:
        \preformatted{
summary(BigQuery(), verbose=TRUE)
summary(conn, verbose=TRUE)
summary(result, verbose=TRUE)
dbGetException(conn)
        }
    }
}

\section{Note}{
This package does not yet support data import or table deletion.
}
\author{
David Xiao <dxiao@csail.mit.edu>
}
\keyword{ package }
\seealso{

\code{\link[DBI]{dbDriver}}
\code{\link[DBI]{dbConnect}}
\code{\link[DBI]{dbDisconnect}}
\code{\link[DBI]{dbSendQuery}}
\code{\link[DBI]{dbGetQuery}}
\code{\link[DBI]{fetch}}
\code{\link[DBI]{dbClearResult}}
\code{\link{summary}}
\code{\link[DBI]{dbGetInfo}}
\code{\link[DBI]{dbColumnInfo}}
\code{\link[DBI]{dbGetException}}
\code{\link[DBI]{dbGetStatement}}

}
