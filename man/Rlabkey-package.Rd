\name{Rlabkey-package}
\alias{Rlabkey-package}
\alias{Rlabkey}
\docType{package}
\title{
Import/export data between a labkey database and R
}
\description{
This package allows the transfer of data between a labkey database and an R session. Data can be imported from 
a labkey database into R by specifying the query schema information (\code{labkey.selectRows}) 
or by using sql commands (\code{labkey.executeSql}). From an R session, existing data can be updated
 (\code{labkey.updateRows}), new data can be inserted (\code{labkey.insertRows}) or  
data can be deleted from the labkey database (\code{labkey.deleteRows}). 

The user must have the appropriate authorization on the labkey
server in order to modify the database through the use of
these functions.
}
\details{
\tabular{ll}{
Package: \tab Rlabkey\cr
Type: \tab Package\cr
Version: \tab 0.0.8\cr
Date: \tab 2009-07-01\cr
License: \tab Apache License 2.0\cr
LazyLoad: \tab yes\cr
}
Using this package to access a password protected labkey data base requires that the user
has their login information in a netrc file. The netrc file
contains configuration and autologin information for the File Transfer Protocol client (ftp) and
other programs such as CURL.

On a UNIX system this file should be named .netrc (dot netrc) and on windows it sould be 
named \_netrc (underscore netrc). The file should be located in the users home directory and the 
permissions on the file should be unreadable for everybody except the owner.  

To create the \_netrc on a windows machine, first create an environment variable called 'HOME' that 
is set to your home directory (c:/Users/<User-Name> on Vista) or any directory you want to use. 
In that directory, create a text file named \_netrc (note that it's underscore netrc, not dot 
netrc like it is on UNIX). 

The following three lines must be included in the .netrc or \_netrc file either separated by white space
(spaces, tabs, or newlines) or commas.

machine <remote-machine-name>\cr
login <user-email>\cr
password <user-password>


One example would be:\cr
machine atlas.scharp.org\cr
login vobencha@fhcrc.org\cr
password mypassword\cr

Another example would be:\cr
machine atlas.scharp.org login vobencha@fhcrc.org password mypassword
}

\author{



Valerie Obenchain
}
\references{http://www.omegahat.org/RCurl/,\cr
http://dssm.unipa.it/CRAN/web/packages/rjson/rjson.pdf,\cr
https://www.labkey.org/project/home/begin.view}
\keyword{ package }
\seealso{
\code{\link{labkey.selectRows}}, \code{\link{labkey.executeSql}}, \code{\link{makeFilter}}, 
\code{\link{labkey.insertRows}}, \cr
\code{\link{labkey.updateRows}}, \code{\link{labkey.deleteRows}}
}
\examples{
}

