

makeFilter <- function(...)
{	
	
fargs <- list(...)
flen <- lapply(fargs, function(x) {len <- length(x); if(len<3){stop ("each filter must be of length 3")}})

#if(is.null(filter1)==TRUE) stop("At least one filter must be specified.")
# 	# For each filter that exists, confirm length=3
#	if(length(filter1)<3) stop("each filter must be of length 3.")
#    if(is.null(filter2)==FALSE) if(length(filter2)<3) stop("each filter must be of length 3.")
#    if(is.null(filter3)==FALSE) if(length(filter3)<3) stop("each filter must be of length 3.")
#    if(is.null(filter4)==FALSE) if(length(filter4)<3) stop("each filter must be of length 3.")
#    if(is.null(filter5)==FALSE) if(length(filter5)<3) stop("each filter must be of length 3.")
	
	# Determine number of filters 
	fmat <- rbind(...)
	fcount <- dim(fmat)[1]
	# Construct filters
	filters <- array(0, dim=c(fcount,1))
	for(i in 1:fcount)
			{	# Match the operator
				fop  <- switch(EXPR=fmat[i,2], 	
								"EQUALS"="eq",
								"EQUALS_ONE_OF"="in",
								"NOT_EQUALS"="neq", 	
								"GREATER_THAN"="gt",
								"GREATER_THAN_OR_EQUAL_TO"="gte",
								"LESS_THAN"="lt",
								"LESS_THAN_OR_EQUAL_TO"="lte",
								"DATE_EQUAL"="dateeq",
								"DATE_NOT_EQUAL"="dateneq",
								"NOT_EQUAL_OR_NULL"="neqornull",
								"IS_MISSING"="isblank",
								"IS_NOT_MISSING"="isnonblank",
								"CONTAINS"="contains",
								"DOES_NOT_CONTAIN"="doesnotcontain",
								"DOES_NOT_START_WITH"="doesnotstartwith",
								"STARTS_WITH"="startswith")
				
				if(is.null(fop)==TRUE) stop ("Invalid operator name.")
				# url encode column name and value	
				colnam <- URLencode(fmat[i,1])
				fvalue <- URLencode(fmat[i,3])
				filters[i] <- paste(colnam,"~",fop,"=",fvalue,sep="")
			}
		 
	return(filters)
}
	
 
