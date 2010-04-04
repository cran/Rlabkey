\name{getFolderPath}
\alias{getFolderPath}
\title{Reutrns the folder path associated with a session}
\description{ Returns the current folder path for a Labkey session }

\usage{
getFolderPath(session) 
}
\arguments{
  \item{session}{the session key returned from getSession}
}

\details{  Returns a string containing the current folder path for the passed in Labkey session 
}

\value{ A character array containing the folder path, relative to the root. 
}
\references{https://www.labkey.org/wiki/home/Documentation/page.view?name=projects}
\author{Peter Hussey}
\seealso{\code{\link{getSession}}  \code{\link{lsFolders}}}
\examples{
# library(Rlabkey)

lks<- getSession(baseUrl="http://localhost:8080/labkey", folderPath="/apisamples")
getFolderPath(lks)  #returns "/apisamples"

}
\keyword{file}
