labkey.executeSql <- function(baseUrl, folderPath, schemaName, sql, maxRows=NULL, 
rowOffset=NULL, showHidden=FALSE)
{  
## If maxRows and/or rowOffset are specified, set showAllRows=FALSE
showAllRows=TRUE
if(is.null(maxRows)==FALSE || is.null(rowOffset)==FALSE){showAllRows=FALSE}

## Error if any of baseUrl, folderPath, schemaName or sql are missing
if(exists("baseUrl")==FALSE || exists("folderPath")==FALSE || exists("schemaName")==FALSE || exists("sql")==FALSE)
stop (paste("A value must be specified for each of baseUrl, folderPath, schemaName and sql."))

## URL encoding of schema and folder path
if(length(grep("%",schemaName))<1) {schemaName <- URLencode(schemaName)}
if(length(grep("%",folderPath))<1) {folderPath <- URLencode(folderPath)}

## Formatting
baseUrl <- gsub("[\\]", "/", baseUrl)
folderPath <- gsub("[\\]", "/", folderPath)
if(substr(baseUrl, nchar(baseUrl), nchar(baseUrl))!="/"){baseUrl <- paste(baseUrl,"/",sep="")}
if(substr(folderPath, nchar(folderPath), nchar(folderPath))!="/"){folderPath <- paste(folderPath,"/",sep="")}
if(substr(folderPath, 1, 1)!="/"){folderPath <- paste("/",folderPath,sep="")}

## Construct url 
myurl <- paste(baseUrl,"query",folderPath,"executeSql.api",sep="")

## Set options
reader <- basicTextGatherer()
header <- basicTextGatherer()
handle <- getCurlHandle()
clist <- ifcookie()
if(clist$Cvalue==1) {myopts <- curlOptions(cookie=paste(clist$Cname,"=",clist$Ccont,sep=""),
                        writefunction=reader$update, headerfunction=header$update, ssl.verifyhost=FALSE,
                        ssl.verifypeer=FALSE, followlocation=TRUE)} else
{myopts <- curlOptions(netrc=1, writefunction=reader$update, headerfunction=header$update, ssl.verifyhost=FALSE,
                        ssl.verifypeer=FALSE, followlocation=TRUE)}


## Post form
postForm(uri=myurl, "schemaName"=schemaName, "sql"=sql, "apiVersion"="8.3", .opts=myopts, curl=handle)



## Error checking for incoming file
h <- parseHeader(header$value())
status <- getCurlInfo(handle)$response.code
message <- h$statusMessage
if(status==500)
{decode <- fromJSON(reader$value()); message <- decode$exception; stop(paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,sep=""))}
if(status>=400)
    {contTypes <- which(names(h)=='Content-Type')
    if(length(contTypes)>1 & h[contTypes[2]]=="application/json;charset=utf-8")
        {decode <- fromJSON(reader$value()); message<-decode$exception; stop (paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,sep=""))} else
    {stop(paste("HTTP request was unsuccessful. Status code = ",status,", Error message = ",message,sep=""))}}


newdata <- makeDF(rawdata=reader$value(), showHidden=showHidden)

return(newdata)
}
                                                              
