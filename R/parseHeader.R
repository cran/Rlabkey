parseHeader <- function (lines)
{
    if (length(lines) < 1)
        return(NULL)
    if (length(lines) == 1)
        lines = strsplit(lines, "\r\n")[[1]]
    status = lines[1]
    lines = lines[-c(1, length(lines))]
    lines = gsub("\r\n", "", lines)
    if (FALSE) {
        header = lines[-1]
        header <- read.dcf(textConnection(header))
    }
    else {
        els <- sapply(lines, function(x) strsplit(x, ":[ ]*"))
        header <- lapply(els, function(x) x[2])
        names(header) <- sapply(els, function(x) x[1])
    }
    els <- strsplit(status, " ")[[1]]
    header[["status"]] <- as.integer(els[2])
	hstring <- NULL
	for(i in 3:length(els)) hstring <- paste(hstring," ",els[i],sep="")
	hstring <- substr(hstring,2,nchar(hstring))
    header[["statusMessage"]] <- hstring
    header
}

