\name{labkey.provenance.createProvenanceParams}
\alias{labkey.provenance.createProvenanceParams}
\title{Create provenance parameter object}
\description{
Helper function to create the data structure that can be used in provenance related APIs.
Note: this function is in beta and not yet final, changes should be expected so exercise caution when using it.
}
\usage{
labkey.provenance.createProvenanceParams(recordingId=NULL, name=NULL, description=NULL,
        runName=NULL, materialInputs=NULL, materialOutputs=NULL, dataInputs=NULL,
        dataOutputs=NULL, inputObjectUriProperty=NULL, outputObjectUriProperty=NULL,
        objectInputs=NULL, objectOutputs=NULL, provenanceMap=NULL,
        params=NULL, properties=NULL)
}
\arguments{
  \item{recordingId}{(optional) the recording ID to associate with other steps using the same ID}
  \item{name}{(optional) the name of this provenance step}
  \item{description}{(optional) the description of this provenance step}
  \item{runName}{(optional) the name of the provenance run, if none specified a default run name will be created}
  \item{materialInputs}{(optional) the list of materials (samples) to be used as the provenance run input. The data structure should be a dataframe with the column name describing the data type (lsid, id)}
  \item{materialOutputs}{(optional) the list of materials (samples) to be used as the provenance run output. The data structure should be a dataframe with the column name describing the data type (lsid, id)}
  \item{dataInputs}{(optional) the list of data inputs to be used for the run provenance map}
  \item{dataOutputs}{(optional) the list of data outputs to be used for the run provenance map}
  \item{inputObjectUriProperty}{(optional) for incoming data rows, the column name to interpret as the input to the provenance map. Defaults to : 'prov:objectInputs'}
  \item{outputObjectUriProperty}{(optional) for provenance mapping, the column name to interpret as the output to the provenance map. Defaults to : 'lsid'}
  \item{objectInputs}{(optional) the list of object inputs to be used for the run provenance map}
  \item{objectOutputs}{(optional) the list of object outputs to be used for the run provenance map}
  \item{provenanceMap}{(optional) the provenance map to be used directly for the run step. The data structure should be a dataframe with the column names of 'from' and 'to' to indicate which sides of the mapping the identifiers refer to}
  \item{params}{(optional) the list of initial run step parameters. Parameters supported in the parameter list such as name, description, runName, can be specified in this data structure as well as other run step parameters not available in the parameter list}
  \item{properties}{(optional) custom property values to associate with the run step. The data structure should be a dataframe with the property URIs as column names and the column value to associate with the property. The Vocabulary domain and fields must have been created prior to using}
}
\details{
This function can be used to generate a provenance parameter object which can then be used as an argument
in the other provenance related functions to assemble provenance runs.
This is a premium feature and requires the Provenance LabKey module to function correctly.
}
\value{
A list containing elements describing the passed in provenance parameters.
}

\author{Karl Lum}
\seealso{
\code{\link{labkey.provenance.startRecording}},
\code{\link{labkey.provenance.addRecordingStep}},
\code{\link{labkey.provenance.stopRecording}}
}
\examples{
\dontrun{

## create provenance params with material inputs and data outputs
library(Rlabkey)

mi <- data.frame(lsid=c("urn:lsid:labkey.com:Sample.251.MySamples:sample1",
        "urn:lsid:labkey.com:Sample.251.MySamples:sample2"))
do <- data.frame(
        lsid="urn:lsid:labkey.com:AssayRunTSVData.Folder-251:12c70994-7ce5-1038-82f0-9c1487dbd334")

p <- labkey.provenance.createProvenanceParams(name="step1", description="initial step",
        materialInputs=mi, dataOutputs=do)

## create provenance params with object inputs (from an assay run)
oi <- labkey.selectRows(baseUrl="https://labkey.org/labkey/", folderPath = "Provenance",
        schemaName="assay.General.titer",
        queryName="Data",
        colSelect= c("LSID"),
        colFilter=makeFilter(c("Run/RowId","EQUAL","253")))
mi <- data.frame(lsid=c("urn:lsid:labkey.com:Sample.251.MySamples:sample1",
        "urn:lsid:labkey.com:Sample.251.MySamples:sample2"))

p <- labkey.provenance.createProvenanceParams(name="step1", description="initial step",
        objectInputs=oi[["LSID"]], materialInputs=mi)

## add run step properties and custom properties to the provenance params
props <- data.frame(
    "urn:lsid:labkey.com:Vocabulary.Folder-996:ProvenanceDomain#version"=c(22.3),
    "urn:lsid:labkey.com:Vocabulary.Folder-996:ProvenanceDomain#instrumentName"=c("NAb reader"),
    check.names=FALSE)
params <- list()
params$comments <- "adding additional step properties"
params$activityDate <- "2022-3-21"
params$startTime <- "2022-3-21 12:35:00"
params$endTime <- "2022-3-22 02:15:30"
params$recordCount <- 2
p <- labkey.provenance.createProvenanceParams(recordingId=ra$recordingId, name="step2",
    properties=props, params=params)

}
}
\keyword{IO}
