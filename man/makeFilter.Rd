\name{makeFilter}
\alias{makeFilter}
\title{Builds filters to be used in labkey.selectRows and getRows}
\description{
This function takes inputs of column name, filter value and filter operator and 
returns an array of filters to be used in \code{labkey.selectRows} and \code{getRows}.
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
\describe{
\item{colname}{a string specifying the name of the column to be filtered} 
\item{operator}{a string specifying what operator should be used in the filter (see options below)}
\item{value}{an integer or string specifying the value the columns should be filtered on}
}

Operator values:\cr
EQUAL\cr
NOT_EQUAL\cr 
NOT_EQUAL_OR_MISSING\cr
DATE_EQUAL\cr
DATE_NOT_EQUAL\cr
MISSING\cr
NOT_MISSING\cr
GREATER_THAN\cr
GREATER_THAN_OR_EQUAL\cr
LESS_THAN\cr
LESS_THAN_OR_EQUAL\cr
CONTAINS\cr
DOES_NOT_CONTAIN\cr
STARTS_WITH\cr
DOES_NOT_START_WITH\cr
EQUALS_ONE_OF\cr
QC_VALUE\cr
NOT_QC_VALUE

When using the MISSING or NOT_MISSING operators, an empty string should be supplied as the value.
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
\dontrun{

# library(Rlabkey)

## Two filters, ANDed together
filter1<- makeFilter(c("TextFld","CONTAINS","h"), c("BooleanFld","EQUAL","TRUE"))

## Apply a filter in labkey.selectRows function
rows <- labkey.selectRows(
	baseUrl="http://localhost:8080/labkey",
	folderPath="/apisamples",
	schemaName="lists", 
	queryName="AllTypes",
	colFilter=filter1
)
rows

## Using "equals one of" operator:
filter2 <- makeFilter(c("RowId","EQUALS_ONE_OF","2;3;6"))
rows <- labkey.selectRows(
	baseUrl="http://localhost:8080/labkey",
	folderPath="/apisamples",
	schemaName="lists", 
	queryName="AllTypes",
	colFilter=filter2
)
rows

## Using "not missing" operator:
filter3 <- makeFilter(c("IntFld","MISSING","")) 

rows <- labkey.selectRows(
	baseUrl="http://localhost:8080/labkey",
	folderPath="/apisamples",
	schemaName="lists", 
	queryName="AllTypes",
	colFilter=filter3
)
rows

}
}
\keyword{file}
