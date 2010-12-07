\name{testFunction}
\alias{testFunction}
\alias{testObject}
\title{A test function}
\description{
    Something I wrote just to try the function
    \code{test}
}
\usage{
    testFunction( arg1, arg2 )
}
\arguments{
    \item{arg1}{Anything}
    \item{arg2}{Anything - as long as it supports addition with object1}
}
\value{
    The sum of the two objects given as input.
    \item{testObject}{A list containing:
        - pointer to the testFunction function
        - a list (1,2,3)
    }
}
\author{David Xiao}
\seealso{
    \code{\link{testObject}}
}
\examples{
   ## try things out
   testFunction( 1:5, 3)

   \dontrun{testObject}
   \dontshow{testObject}
}
\keyword{test}
