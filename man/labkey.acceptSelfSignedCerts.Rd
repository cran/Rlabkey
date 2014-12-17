\name{labkey.acceptSelfSignedCerts}
\alias{labkey.acceptSelfSignedCerts}
\title{Convenience method to configure Rlabkey connections to accept self-signed certificates}
\description{
Rlabkey uses the package RCurl to connect to the LabKey Server. This is equivalent to
executing the function:  labkey.setCurlOptions(ssl.verifyhost=0, ssl.verifypeer=FALSE)
}
\usage{
labkey.acceptSelfSignedCerts()
}
