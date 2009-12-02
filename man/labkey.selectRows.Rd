\name{labkey.selectRows}
\alias{labkey.selectRows}
\title{Retrieve data from a labkey database}
\description{
Import full datasets or selected rows into R. The data can be sorted and filtered prior to import.
}
\usage{
labkey.selectRows(baseUrl, folderPath, schemaName, queryName, 
viewName = NULL, colSelect = NULL, maxRows = NULL, 
rowOffset = NULL, colSort = NULL, colFilter = NULL, showHidden = FALSE)
}
\arguments{
  \item{baseUrl}{a string specifying the \code{baseUrl}for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{schemaName}{a string specifying the  \code{schemaName} for the query}
  \item{queryName}{a string specifying the \code{queryName}}
  \item{viewName}{(optional) a string specifying the \code{viewName}}
  \item{colSelect}{(optional) a vector of comma separated strings specifying which columns of a dataset or view to import}
  \item{maxRows}{(optional) an integer specifying how many rows of data to return. If no value is specified, all rows are returned.}
  \item{colSort}{(optional) a string including the name of the column to sort preceeded by a \dQuote{+} or
\dQuote{-} to indicate sort direction}
  \item{rowOffset}{(optional) an integer specifying which row of data should be the first row in the retrieval. If no
value is specified, the retrieval starts with the first row.}
  \item{colFilter}{(optional) a vector or array object created by the \code{makeFilter} function which contains the
column name, operator and value of the filter(s) to be applied to the retrieved data.}
  \item{showHidden}{(optional) a logical value indicating whether or not to return data columns that would normally be hidden from user view. If no value is specified, the hidden columns are not returned. In versions 0.0.5 and earlier, this argument was called \code{stripAllHidden}. The \code{stripAllHidden} argument performed the same function as \code{showHidden} but with different logic. The change was made for clarity.}
}
\details{
A full dataset or any portion of a dataset can be imported into an R data frame using the\cr 
\code{labkey.selectRows} function. Function arguments are the components of the url that identify
the location of the data and what actions should be taken on the data prior to import
(ie, sorting, selecting particular columns or maximum number of rows, etc.).

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
\code{\link{labkey.executeSql}}, \code{\link{makeFilter}}, \code{\link{labkey.insertRows}}, 
\code{\link{labkey.updateRows}}, \cr
\code{\link{labkey.deleteRows}}

}
\examples{
## These example datasets are located at 
## https://www.labkey.org/project/home/Study/demo/begin.view?


## Retrieve full HIV Test Results dataset
## Not run
#fulldata <- labkey.selectRows(
#baseUrl="https://www.labkey.org",
#folderPath="/home/Study/demo",
#schemaName="study",
#queryName="HIV Test Results")


## Specifying filters, max rows and selecting columns
## Not run
#myfilters<- makeFilter(c("HIVLoadQuant","GREATER_THAN",500),
#c("HIVRapidTest","EQUAL","Positive"))
#smalldata <- labkey.selectRows(
#baseUrl="https://www.labkey.org",
#folderPath="/home/Study/demo",
#schemaName="study",
#queryName="HIV Test Results",
#colSelect=c("ParticipantId","HIVDate","HIVLoadQuant","HIVRapidTest"),
#maxRows=20,
#colFilter=myfilters)



}
\keyword{IO}
