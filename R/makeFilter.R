

makeFilter <- function(...)
{	
	
fargs <- list(...)
flen <- lapply(fargs, function(x) {len <- length(x); if(len<3){stop ("each filter must be of length 3")}})

	# Determine number of filters 
	fmat <- rbind(...)
	fcount <- dim(fmat)[1]
	# Construct filters
	filters <- array(0, dim=c(fcount,1))
	for(i in 1:fcount)
			{	# Match the operator
				fop  <- switch(EXPR=fmat[i,2], 	
								"EQUALS"="eq",
								"EQUAL"="eq",
								"NOT_EQUALS"="neq", 	
								"NOT_EQUAL"="neq", 	
								"NOT_EQUAL_OR_NULL"="neqornull",
								"NOT_EQUAL_OR_MISSING"="neqornull",
								"DATE_EQUAL"="dateeq",
								"DATE_NOT_EQUAL"="dateneq",
								"IS_MISSING"="isblank",
								"MISSING"="isblank",
								"IS_NOT_MISSING"="isnonblank",
								"NOT_MISSING"="isnonblank",
								"GREATER_THAN"="gt",
								"GREATER_THAN_OR_EQUAL_TO"="gte",
								"GREATER_THAN_OR_EQUAL"="gte",
								"LESS_THAN"="lt",
								"LESS_THAN_OR_EQUAL_TO"="lte",
								"CONTAINS"="contains",
								"DOES_NOT_CONTAIN"="doesnotcontain",
								"STARTS_WITH"="startswith",
								"DOES_NOT_START_WITH"="doesnotstartwith",
								"EQUALS_ONE_OF"="in")
				
				if(is.null(fop)==TRUE) stop ("Invalid operator name.")
				# url encode column name and value	
				colnam <- URLencode(fmat[i,1])
				fvalue <- URLencode(fmat[i,3])
				filters[i] <- paste(colnam,"~",fop,"=",fvalue,sep="")
			}
		 
	return(filters)
}
	
 
