\name{dbGetInfo}
\alias{dbGetInfo}
\title{Retrieve BQ object information as list}
\description{
    Retrieves information about a BQObject (or subclass) and presents it in the form of a list.
}
\usage{
    dbGetInfo( dbObj, ... )
}
\arguments{
    \item{dbObj}{Either BQDriver, BQConnection, or BQResult}
    \item{...}{Accepts a \code{what} argument, requesting only that point of information.}
}
\seealso{
    \code{\link{summary}}
}
