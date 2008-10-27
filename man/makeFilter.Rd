\name{makeFilter}
\alias{makeFilter}
\title{Builds filters to be used in labkey.selectRows}
\description{
This function takes inputs of column name, filter value and filter operator and 
returns an array of filters to be used in \code{labkey.selectRows}.
}
\usage{
makeFilter(...)
}
\arguments{
\item{...}{Arguments in c("colname","operator","value") form, used to create a filter.}
}
\details{
These filters are applied to the data prior to import into R. The user can specify as many 
filters as desired. The format for specifying a filter is a vector of characters including
the column name, operator and value.
  \item{colname}{a string specifying the name of the column to be filtered} 
  \item{operator}{a string specifying what operator should be used in the filter (see options below)}
  \item{value}{an integer or string specifying the value the columns should be filtered on}

Possible operator values are as follows:
"EQUALS", "EQUALS\_ONE\_OF", "NOT\_EQUALS", "GREATER\_THAN", "GREATER\_THAN\_OR\_EQUAL\_TO", 
"LESS\_THAN", "LESS\_THAN\_OR\_EQUAL\_TO", "DATE\_EQUAL", "DATE\_NOT\_EQUAL", 
"NOT\_EQUAL\_OR\_NULL", "IS\_MISSING", "IS\_NOT\_MISSING", "CONTAINS", "DOES\_NOT\_CONTAIN", 
"STARTS\_WITH", and "DOES\_NOT\_START\_WITH".

When using the "IS\_MISSING" or "IS\_NOT\_MISSING" operators, an empty string should be supplied as the value.
See example below.
}


\value{
The function returns either a single string or an array of strings to be use in the
\code{colFilter} argument of the \code{labkey.selectRows} function.
}
\references{http://www.omegahat.org/RCurl/, \cr
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\author{Valerie Obenchain}
\seealso{\code{\link{labkey.selectRows}}}
\examples{
## These example datasets are located at https://www.labkey.org/project/home/Study/demo/begin.view?


## Two filters:
filter1<- makeFilter(c("HIVLoadQuant","GREATER_THAN",500), c("HIVRapidTest","EQUALS","Positive"))

## Using "equals one of" operator:
filter2 <- makeFilter(c("HIVLoadIneq","EQUALS_ONE_OF","Equals ; Less than"))

## Using "is not missing" operator:
filter3 <- makeFilter(c("HIVRapidTest","IS_NOT_MISSING","")) 

## Apply a filter in labkey.selectRows function
getdata <- labkey.selectRows(
baseUrl="https://www.labkey.org", 
folderPath="/home/Study/demo", 
schemaName="study", 
queryName="HIV Test Results", 
colSelect=c("ParticipantId","HIVDate","HIVLoadQuant","HIVRapidTest"), 
colFilter=filter3)



}
\keyword{file}
