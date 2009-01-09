\name{labkey.updateRows}
\alias{labkey.updateRows}
\title{Update existing rows of data in a labkey database}
\description{
Send data from an R session to update existing rows of data in the database.
}
\usage{
labkey.updateRows(baseUrl, folderPath, schemaName, queryName, toUpdate, 
stripAllHidden = TRUE)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{schemaName}{a string specifying the  \code{schemaName}for the query}
  \item{queryName}{a string specifying the  \code{queryName}} 
  \item{toUpdate}{a data frame containing the row(s) of data to be updated}
  \item{stripAllHidden}{(optional) a logical value indicating whether or not to return data columns that would 
normally be hidden from user veiw. If no value is specified, no hidden columns are returned.}
}
\details{
A single row or multiple rows of data can be updated.  The \code{toUpdate} data frame should contain 
the rows of data to be updated and must be created with the \code{stringsAsFactors} option
set to FALSE. The names of the data in the data frame must be the column names from the
labkey database. 

NOTE: Each variable in a dataset has both a column label and a column name. The column label is visible at the top
of each column on the web page and is longer and more descriptive. The column name is shorter and is
used \dQuote{behind the scenes} for database manipulation. It is the column name that must be used in
the Rlabkey functions when a column name is expected. To identify a particular column name in a dataset on
a web site, use the \dQuote{export to R script} option available as a drop down option under the \dQuote{views}
tab for each dataset.

}
\value{
A list is returned with named categories of \bold{command}, \bold{rowsAffected}, \bold{rows}, \bold{queryName}, \bold{containerPath} and \bold{schemaName}.
The \bold{schemaName}, \bold{queryName} and \bold{containerPath} properties contain the same schema, query 
and folder path used in the request.  The
\bold{rowsAffected} property indicates he number of rows affected by the API action. This will typically be the same
number as passed in the request. The \bold{rows} property contains a list of row objects corresponding to the rows
updated.
}
\references{http://www.omegahat.org/RCurl/, \cr
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}

\author{Valerie Obenchain}
\seealso{
\code{\link{labkey.selectRows}}, \code{\link{labkey.executeSql}}, \code{\link{makeFilter}}, 
\code{\link{labkey.insertRows}}, \cr
\code{\link{labkey.deleteRows}}

}
\examples{
## Insert, update and delete
# Note that users must have the necessary permissions in the database
# to be able to modify data through the use of these functions

### Not run
#newrow <- data.frame(name="Frank", age=11, stringsAsFactors=FALSE)
#labkey.insertRows(
#baseUrl="https://atlas-test.scharp.org/cpas", 
#folderPath="/CHAVI/Analysis/vobencha", 
#schemaName="lists", 
#queryName="testlist", 
#toInsert=newrow)
#
#updaterow=data.frame(Key=4,name="Patty",age=11, stringsAsFactors=FALSE)
#mydata <- labkey.updateRows(
#baseUrl="https://atlas-test.scharp.org/cpas", 
#folderPath="/CHAVI/Analysis/vobencha", 
#schemaName="lists", 
#queryName="testlist", 
#toUpdate=updaterow)
#
#deleterow <- data.frame(Key=1, stringsAsFactors=FALSE)
#mydata <- labkey.deleteRows(
#baseUrl="https://atlas-test.scharp.org/cpas", 
#folderPath="/CHAVI/Analysis/vobencha", 
#schemaName="lists", 
#queryName="testlist", 
#toDelete=deleterow)

}
\keyword{IO}
