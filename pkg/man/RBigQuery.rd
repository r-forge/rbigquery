\name{RBigQuery}
\title{BigQuery for R}
\description{
    Currently still a work in progress: basic workflow example below:
}
\examples{
    \dontrun{
        driver <- BigQuery()
        conn <- dbConnect(driver, username="user@gmail.com", password="password")
        result <- dbSendQuery(conn, "SELECT count(*) from [examplebucket/tables/ponylist] WHERE is_magic = true;")
        summary(result)
        data.frame(result@result)
    }
}
    
