\name{labkey.deleteRows}
\alias{labkey.deleteRows}
\title{Delete rows of data from a labkey database}
\description{
Specify rows of data to be deleted from the database.
}
\usage{
labkey.deleteRows(baseUrl, folderPath, schemaName, queryName, toDelete, 
stripAllHidden = TRUE)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for labkey server}
  \item{folderPath}{a string specifying the \code{folderPath}}
  \item{schemaName}{a string specifying the  \code{schemaName} for the query}
  \item{queryName}{a string specifying the  \code{queryName}}
  \item{toDelete}{a data frame containing a single column of data containing the data identifiers of the rows to be deleted} 
  \item{stripAllHidden}{(optional) a logical value indicating whether or not to return data columns that would 
normally be hidden from user veiw. If no value is specified, no hidden columns are returned.}
}
\details{
A single row or multiple rows of data can be deleted.  The \code{toDelete} data frame should consist of a single 
column of data containing the data identifiers of the rows to be deleted (e.g., key or lsid).
The name of the data in the data frame must be the column name from the labkey database.
The data frame must be created with the \code{stringsAsFactors} set to FALSE.

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
number as passed in the request. The \bold{rows} property contains a list of rows corresponding to the rows
deleted.
}
\references{http://www.omegahat.org/RCurl/, \cr
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}

\author{Valerie Obenchain}
\seealso{
\code{\link{labkey.selectRows}}, \code{\link{labkey.executeSql}}, \code{\link{makeFilter}}, 
\code{\link{labkey.insertRows}}, \code{\link{labkey.updateRows}}
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
