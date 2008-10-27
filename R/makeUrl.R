
makeUrl <- function(baseUrl, folderPath, schemaName, queryName, viewName, colSelect, maxRows, rowOffset, colSort, colFilter, showAllRows)
{	
	## URL encoding of schema, query, view and folder path
	if(length(grep("%",schemaName))<1) {schemaName <- URLencode(schemaName)}
	if(length(grep("%",queryName))<1) {queryName <- URLencode(queryName)}
	if(length(grep("%",folderPath))<1) {folderPath <- URLencode(folderPath)}
	if(is.null(viewName)==FALSE) {if(length(grep("%",viewName))<1) viewName <- URLencode(viewName)}


	## Format colSelect
	if(is.null(colSelect)==FALSE)
		{	holder <- NULL
			for(i in 1:length(colSelect)) holder <-paste(holder,colSelect[i],",",sep="")
			colSelect <- substr(holder, 1, nchar(holder)-1)}


	## Replace backslashes if present and add forward slashes where needed
	baseUrl <- gsub("[\\]", "/", baseUrl)
	folderPath <- gsub("[\\]", "/", folderPath)
	if(substr(baseUrl, nchar(baseUrl), nchar(baseUrl))!="/"){baseUrl <- paste(baseUrl,"/",sep="")}
	if(substr(folderPath, nchar(folderPath), nchar(folderPath))!="/"){folderPath <- paste(folderPath,"/",sep="")}
    if(substr(folderPath, 1, 1)!="/"){folderPath <- paste("/",folderPath,sep="")}
    

	## Construct url 
	res <- paste(baseUrl,"query",folderPath,"selectRows.api?schemaName=",schemaName,"&query.queryName=",queryName,sep="")
	if(is.null(viewName)==FALSE) {res <- paste(res,"&query.viewName=",viewName,sep="")}
	if(is.null(colSelect)==FALSE) {res <- paste(res,"&query.columns=",colSelect,sep="")}
	if(is.null(maxRows)==FALSE) {res <- paste(res,"&query.maxRows=",maxRows,sep="")}
	if(is.null(rowOffset)==FALSE) {res <- paste(res,"&query.offset=",rowOffset,sep="")}
	if(is.null(colSort)==FALSE) {res <- paste(res,"&query.sort=",colSort,sep="")}
	if(is.null(colFilter)==FALSE) {for(j in 1:length(colFilter)) res <- paste(res,"&query.",colFilter[j],sep="")}
	if(showAllRows==TRUE) {res <- paste(res,"&query.showAllRows=",showAllRows,sep="")}

	return(res)
} 
