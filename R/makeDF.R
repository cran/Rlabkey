 makeDF <- function(rawdata, colSelect=NULL, showHidden)
{	decode <- fromJSON(rawdata)

	## Check for invalid colSelect name (with labkey 8.3 this returns lsid column only)
	if(is.null(colSelect)==FALSE){
	if(length(decode$columnModel)==1 & decode$columnModel[[1]]$header!=colSelect) 
		{stop(paste('The column names in the query "',decode$queryName,'" do not match one or more of the names specified in the colSelect variable. Be sure you are using the column name and not the column label. See the documentation for more details.',sep=''))}}


  	## Get column names in proper order, associated header index, hidden tag, and data type
  	cnames <- NULL
  	hindex <- NULL
  	hide <- NULL
  	for(j in 1:length(decode$columnModel))
  	    {   cnames <- c(cnames, decode$columnModel[[j]]$header)
  	        hindex <- c(hindex, decode$columnModel[[j]]$dataIndex)
  	        hide <- c(hide, decode$columnModel[[j]]$hidden)}
  	refdf <- data.frame(cnames,hindex,hide)
  	
	## Check for no rows returned, put data in data frame 
  	if(length(decode$rows)<1)
		{tohide <- length(which(refdf$hide==TRUE))
		totalcol <- length(refdf$cnames)
		if(showHidden==FALSE)
			{emptydf <- as.data.frame(rep(list(num=double(0)), each=(totalcol-tohide)))
			colnames(emptydf) <- refdf$cnames[refdf$hide==FALSE]
			warning("Empty data frame was returned. Query may be too restrictive.", call.=FALSE)
			return(emptydf)}else
		{emptydf <- as.data.frame(rep(list(num=double(0)), each=(totalcol)))
		colnames(emptydf) <- refdf$cnames
		warning("Empty data frame was returned. Query may be too restrictive.", call.=FALSE)}
		return(emptydf)}
		
	if(length(decode$rows)>0)
		{hold.dat <- NULL
    	hold.dat <- matrix(sapply(decode$rows,rbind), nrow=length(decode$rows), byrow=TRUE)
    	hold.dat <- as.data.frame(hold.dat)
    	names(hold.dat) <- names(decode$rows[[1]])}

	## Order data
	oindex <- NULL
  	for(k in 1:length(names(hold.dat))){oindex <- rbind(oindex, which(names(hold.dat)==refdf$hindex[k]))}
  	refdf$oindex <- oindex
  	refdf$type <- NULL
  	for(p in 1:dim(refdf)[1])
  	    {   ind <- which(refdf$hindex==decode$metaData$fields[[p]]$name)
  	        refdf$type[ind] <- decode$metaData$fields[[p]]$type}

	newdat <- NULL  	
	for(i in 1:length(cnames)){ newdat <- cbind(newdat, hold.dat[,refdf$oindex[i]])}
	newdat <- as.data.frame(newdat)

  	## Delete hidden column(s) unless showHidden=TRUE
  	if(showHidden==TRUE)   {} else
  	{if(is.null(decode$metaData$id)) {} else
  		{hide.ind <- which(refdf$hide==TRUE)
  		newdat <- newdat[,-hide.ind]
		refdf <- refdf[-hide.ind,]
		cnames <- cnames[-hide.ind]}}

	## Set mode for multiple columns of data (this also removes list factor)
	if(is.null(dim(newdat))==FALSE) 
  	{for(j in 1:ncol(newdat))
  	    {mod <- refdf$type[j]
  	    if(mod=="date"){ newdat[,j] <- as.Date(as.character(newdat[,j]), "%d %b %Y %H:%M:%S %Z")}else
	    if(mod=="string"){	mode(newdat[,j]) <- "character"} else
  	    if(mod=="int"){ mode(newdat[,j]) <- "numeric"} else
  	    if(mod=="boolean"){mode(newdat[,j]) <- "logical"} else
  	    if(mod=="float"){mode(newdat[,j]) <- "numeric"} else
  	    {print("MetaData field type not recognized.")}}
	newdat <- as.data.frame(newdat, stringsAsFactors=FALSE); colnames(newdat)<-cnames}
	## Set mode for single column of data
	if(is.null(dim(newdat))==TRUE & length(newdat)>1) 
	{mod <- refdf$type
  	if(mod=="date"){ newdat <- as.Date(as.character(newdat), "%d %b %Y %H:%M:%S %Z")}else
  	if(mod=="string"){mode(newdat) <- "character"} else
  	if(mod=="int"){ mode(newdat) <- "numeric"} else
  	if(mod=="boolean"){mode(newdat) <- "logical"} else
  	if(mod=="float"){mode(newdat) <- "numeric"} else
  	{print("MetaData field type not recognized.")}
	newdat <- as.data.frame(newdat, stringsAsFactors=FALSE); colnames(newdat)<-cnames[1]}


	## Replace string "NA" with NA in character columns
	for(k in 1:ncol(newdat))
		{if(mode(newdat[,k])=="character") {na.ind <- which(newdat[,k]=="NA")
											ifelse(na.ind>0, newdat[na.ind,k] <- NA, )}}    

  	
return(newdat)
}

