##
#  Copyright (c) 2008-2010 Fred Hutchinson Cancer Research Center
# 
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
# 
#      http://www.apache.org/licenses/LICENSE-2.0
# 
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
##

labkey.selectRows <- function(baseUrl, folderPath, schemaName, queryName, viewName=NULL, colSelect=NULL,
        maxRows=NULL, rowOffset=NULL, colSort=NULL, colFilter=NULL, showHidden=FALSE, colNameOpt='caption', containerFilter=NULL)
{
## Empty string/NULL checking
if(is.null(viewName)==FALSE) {char <- nchar(viewName); if(char<1){viewName<-NULL}}
if(is.null(colSelect)==FALSE) {char <- nchar(colSelect[1]); if(char<1){colSelect<-NULL}}
if(is.null(maxRows)==FALSE) {char <- nchar(maxRows); if(char<1){maxRows<-NULL}}
if(is.null(rowOffset)==FALSE) {char <- nchar(rowOffset); if(char<1){rowOffset<-NULL}}
if(is.null(colSort)==FALSE) {char <- nchar(colSort); if(char<1){colSort<-NULL}}
if(is.null(colFilter)==FALSE) {char <- nchar(colFilter[1]); if(char<1){colFilter<-NULL}}
if(is.null(showHidden)==FALSE) {char <- nchar(showHidden); if(char<1){showHidden<-FALSE}}
if(is.null(containerFilter)==FALSE) {char <- nchar(containerFilter[1]); if(char<1){containerFilter<-NULL}}


## Error if any of baseUrl, folderPath, schemName or queryName are missing
if(exists("baseUrl")==FALSE || exists("folderPath")==FALSE || exists("schemaName")==FALSE || exists("queryName")==FALSE)
stop (paste("A value must be specified for each of baseUrl, folderPath, schemaName and queryName."))

## URL encoding of schema, query, view and folder path
if(length(grep("%",schemaName))<1) {schemaName <- URLencode(schemaName)}
if(length(grep("%",queryName))<1) {queryName <- URLencode(queryName)}
if(length(grep("%",folderPath))<1) {folderPath <- URLencode(folderPath)}
if(is.null(viewName)==FALSE) {if(length(grep("%",viewName))<1) viewName <- URLencode(viewName)}
if(is.null(containerFilter)==FALSE) {if(length(grep("%",containerFilter))<1) containerFilter<- URLencode(containerFilter)}

## Format colSelect
if(is.null(colSelect)==FALSE)
  {   lencolSel <- length(colSelect)
    holder <- NULL
      for(i in 1:length(colSelect)) holder <-paste(holder,URLencode(colSelect[i]),",",sep="")
      colSelect <- substr(holder, 1, nchar(holder)-1)}

## Formatting
baseUrl <- gsub("[\\]", "/", baseUrl)
folderPath <- gsub("[\\]", "/", folderPath)
if(substr(baseUrl, nchar(baseUrl), nchar(baseUrl))!="/"){baseUrl <- paste(baseUrl,"/",sep="")}
if(substr(folderPath, nchar(folderPath), nchar(folderPath))!="/"){folderPath <- paste(folderPath,"/",sep="")}
if(substr(folderPath, 1, 1)!="/"){folderPath <- paste("/",folderPath,sep="")}

## Construct url
myurl <- paste(baseUrl,"query",folderPath,"selectRows.api?schemaName=",schemaName,"&query.queryName=",queryName,sep="","&apiVersion=8.3")
if(is.null(viewName)==FALSE) {myurl <- paste(myurl,"&query.viewName=",viewName,sep="")}
if(is.null(colSelect)==FALSE) {myurl <- paste(myurl,"&query.columns=",colSelect,sep="")}
if(is.null(maxRows)==FALSE) {myurl <- paste(myurl,"&query.maxRows=",maxRows,sep="")}
if(is.null(rowOffset)==FALSE) {myurl <- paste(myurl,"&query.offset=",rowOffset,sep="")}
if(is.null(colSort)==FALSE) {myurl <- paste(myurl,"&query.sort=",colSort,sep="")}
if(is.null(colFilter)==FALSE) {for(j in 1:length(colFilter)) myurl <- paste(myurl,"&query.",colFilter[j],sep="")}
if(is.null(maxRows)==TRUE) {myurl <- paste(myurl,"&query.showRows=all",sep="")}
if(is.null(containerFilter)==FALSE) {myurl <- paste(myurl,"&containerFilter=",containerFilter,sep="")}

## Set options
reader <- basicTextGatherer()
header <- basicTextGatherer()
myopts <- curlOptions(writefunction=reader$update, headerfunction=header$update, netrc=1, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE, followlocation=TRUE)

## Support user-settable options for debuggin and setting proxies etc
if(exists(".lksession"))
{
	userOpt <- .lksession[["curlOptions"]] 
	if (!is.null(userOpt))
		{myopts<- curlOptions(.opts=c(myopts, userOpt))}
}

## Http get
handle <- getCurlHandle()
clist <- ifcookie()
if(clist$Cvalue==1) 
{	
	mydata <- getURI(myurl, .opts=myopts, cookie=paste(clist$Cname,"=",clist$Ccont,sep=""))
} 
else 
{
	myopts <-curlOptions(.opts=c(myopts, httpauth=1L))
	mydata <- getURI(myurl, .opts=myopts, curl=handle)
}


## Error checking, decode data and return data frame
h <- parseHeader(header$value())
status <- getCurlInfo(handle)$response.code
message <- h$statusMessage

if(status==500)
{decode <- fromJSON2(mydata); message <- decode$exception; stop(paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,sep=""))}
if(status>=400)
  {contTypes <- which(names(h)=='Content-Type')
  if(length(contTypes)>1 & h[contTypes[2]]=="application/json;charset=utf-8")
      {decode <- fromJSON2(mydata); message<-decode$exception; stop (paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,sep=""))} else
  {stop(paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,sep=""))}}

newdata <- makeDF(mydata, colSelect, showHidden, colNameOpt)


## Check for less columns returned than requested
if(is.null(colSelect)==FALSE){if(ncol(newdata)<lencolSel)warning("Fewer columns are returned than were requested in the colSelect variable. The column names may be invalid. Be sure to use the column name and not the column caption. See the documentation for further explaination.")}


return(newdata)
}

