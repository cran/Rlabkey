\name{labkey.executeSql}
\alias{labkey.executeSql}
\title{Retrieve data from a labkey database using SQL commands}
\description{
Use Sql commands to specify data to be imported into R. Prior to import, data can
be manipulated through standard SQL commands supported in labkey SQL.
}
\usage{
labkey.executeSql(baseUrl, folderPath, schemaName, sql, maxRows = NULL, 
rowOffset = NULL, stripAllHidden = TRUE)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{schemaName}{a string specifying the  \code{schemaName} for the query}
  \item{sql}{a string containing the \code{sql} commands to be executed}
  \item{maxRows}{(optional) an integer specifying the maximum number of rows to return. If no value is specified, all rows are returned.}
  \item{rowOffset}{(optional) an integer specifying which row of data should be the first row in the retrieval. 
If no value is specified, rows will begin at the start of the result set.}
  \item{stripAllHidden}{(optional) a logical value indicating whether or not to return data columns that would 
normally be hidden from user veiw. If no value is specified, no hidden columns are returned.}
}
\details{
A full dataset or any portion of a dataset can be imported into an R
data frame using the \code{labkey.executeSql}
function. Function arguments are components of the url that identify the location of the
data and the SQL actions that should be taken on the data prior to import.


NOTE: Each variable in a dataset has both a column label and a column name. The column label is visible at the top
of each column on the web page and is longer and more descriptive. The column name is shorter and is
used \dQuote{behind the scenes} for database manipulation. It is the column name that must be used in
the Rlabkey functions when a column name is expected. To identify a particular column name in a dataset on
a web site, use the \dQuote{export to R script} option available as a drop down option under the \dQuote{views}
tab for each dataset.

}
\value{
The requested data are returned in a data frame with column names as they appear on the website.
}
\references{http://www.omegahat.org/RCurl/,\cr 
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}

\author{Valerie Obenchain}
\seealso{
\code{\link{labkey.selectRows}}, \code{\link{makeFilter}}, \code{\link{labkey.insertRows}}, 
\code{\link{labkey.updateRows}}, \cr
\code{\link{labkey.deleteRows}}
}

\examples{
## These example datasets are located at 
## https://www.labkey.org/project/home/Study/demo/begin.view?


## Select participants who meet acute status requirements
getacute <- labkey.executeSql(
baseUrl="https://www.labkey.org",
folderPath="/home/Study/demo",
schemaName="study",
sql = 'select "Status Assessment".ParticipantId, "Status Assessment".StatusMeetCriteria 
from "Status Assessment" where "Status Assessment".StatusMeetCriteria=\'Yes\'')


## Compute average ages over different gender groups, 
## use column alias "Number" to rename the column
getage <- labkey.executeSql(
baseUrl="https://www.labkey.org",
folderPath="/home/Study/demo",
schemaName="study",
sql = "select Demographics.Gender, avg(Demographics.Age) as Number from Demographics 
group by Demographics.Gender")


## Get a list of participants with partner information 
getpartners <- labkey.executeSql(
baseUrl="https://www.labkey.org",
folderPath="/home/Study/demo",
schemaName="study",
sql = 'select "Status Assessment".ParticipantID, "Status Assessment".StatusPartner1 
from "Status Assessment" where "Status Assessment".StatusPartner1 is not null')

}
\keyword{IO}
