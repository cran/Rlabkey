\name{labkey.experiment.saveBatch}
\alias{labkey.experiment.saveBatch}
\title{Saves a modified experiment batch}
\description{
Saves a modified experiment batch.
}
\usage{
labkey.experiment.saveBatch(baseUrl=NULL, folderPath,
    assayConfig = NULL, protocolName = NULL,
    batchPropertyList = NULL, runList)
}
\arguments{
  \item{baseUrl}{(optional) a string specifying the \code{baseUrl} for the labkey server}
  \item{folderPath}{a string specifying the \code{folderPath} }
  \item{assayConfig}{(optional) a list specifying assay configuration information}
  \item{protocolName}{(optional) a string specifying the protocol name of the protocol to use}
  \item{batchPropertyList}{(optional) a list of batch properties }
  \item{runList}{a list of experiment run objects}
}
\details{
Saves a modified batch. Runs within the batch may refer to existing data and material objects, either inputs or outputs,
by ID or LSID. Runs may also define new data and materials objects by not specifying an ID or LSID in their properties.

Runs can be created for either assay or non-assay backed protocols. For an assay backed protocol, either the assayId or
the assayName and providerName name must be specified in the assayConfig parameter. If a non-assay backed protocol is to be used,
specify the protocolName string value, note that currently only the simple : labkey.experiment.SAMPLE_DERIVATION_PROTOCOL is supported.

Refer to the labkey.experiment.createData, labkey.experiment.createMaterial, and labkey.experiment.createRun helper functions
to assemble the data structure that saveBatch expects.
}

\value{
 Returns the object representation of the experiment batch.
}

\author{Karl Lum}
\seealso{
\code{\link{labkey.experiment.createData}},
\code{\link{labkey.experiment.createMaterial}},
\code{\link{labkey.experiment.createRun}}
}
\examples{
\dontrun{

library(Rlabkey)

## uploads data to an existing assay

df <- data.frame(participantId=c(1:3), visitId = c(10,20,30), sex = c("f", "m", "f"))
bprops <- list(LabNotes="this is a simple demo")
bpl <- list(name=paste("Batch ", as.character(date())),properties=bprops)
run <- labkey.experiment.createRun(list(name="new assay run"), dataRows = df)
labkey.experiment.saveBatch(baseUrl="http://labkey/", folderPath="home",
    assayConfig=list(assayName="GPAT", providerName="General"),
    batchPropertyList=bpl, runList=run)

## create a non-assay backed run with samples as material inputs and outputs

m1 <- labkey.experiment.createMaterial(
        list(name = "87444063.2604.626"), sampleSetName = "Study Specimens")
m2 <- labkey.experiment.createMaterial(
        list(name = "87444063.2604.625"), sampleSetName = "Study Specimens")
run <- labkey.experiment.createRun(
        list(name="new run"), materialInputs = m1, materialOutputs = m2)
labkey.experiment.saveBatch(baseUrl="http://labkey/", folderPath="home",
        protocolName=labkey.experiment.SAMPLE_DERIVATION_PROTOCOL, runList=run)

}
}
\keyword{IO}
