\name{dbGetInfo-methods}
\docType{methods}
\alias{dbGetInfo-methods}
\alias{dbGetInfo,BQConnection-method}
\alias{dbGetInfo,BQDriver-method}
\alias{dbGetInfo,BQResult-method}
\title{ Information about a BQObject }
\description{
    Get information about a BQObject, either general list or a specific
    element of that list.
}
\section{Methods}{
\describe{

\item{\code{signature(dbObj = "BQDriver")}}{
    Returns:
    \describe{
        \item{drvName}{Name of Package}
        \item{managerID}{ID number of object}
        \item{fetch.default.rec}{default number of rows per fetch (unused)}
        \item{max.con}{maximum number of connections allows (unused)}
        \item{prc.con}{number of processed connections (unused)}
        \item{connections}{list of active connections under driver}
        \item{clientVersion}{version of package}
    }
}

\item{\code{signature(dbObj = "BQConnection")}}{
    Returns:
    \describe{
        \item{driver}{Driver the connection is using}
        \item{username}{username of google account}
        \item{password}{length of google account password (as opposed to password itself)}
        \item{auth.token}{authorization token used when querying BigQuery}
        \item{last.result}{last BQResult returned by connection}
    }
    
}

\item{\code{signature(dbObj = "BQResult")}}{
    Returns:
    \describe{
        \item{connection}{Connection the result came from}
        \item{statement}{statement used in query}
        \item{success}{logical indicating success of query}
        \item{fields}{result table metadata}
        \item{result}{if success is TRUE, the data returned by query}
        \item{error}{if success is FALSE, the error returned by query}
    }
}
}}

\arguments{
    \item{dbObj}{BQObject to get information about}
    \item{what}{Optional: character name of specific element of list to return.}
}

\examples{ \dontrun{
    dbGetInfo(driver) #returns list

    dbGetInfo(driver, what="drvName") #returns name of package
}}
\seealso{
    \code{\link{summary}}
    \code{\link{BQDriver-class}}
    \code{\link{BQConnection-class}}
    \code{\link{BQResult-class}}
}
\keyword{methods}
