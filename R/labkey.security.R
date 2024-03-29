##
#  Copyright (c) 2018 LabKey Corporation
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
##

## Returns information about the specified container, including the user's current permissions within that container.
##
labkey.security.getContainers <- function(baseUrl=NULL, folderPath, includeEffectivePermissions=TRUE, includeSubfolders=FALSE, depth=50,
    includeChildWorkbooks=TRUE, includeStandardProperties=TRUE)
{
    results = labkey.getFolders(baseUrl, folderPath, includeEffectivePermissions, includeSubfolders, depth,
                                includeChildWorkbooks, includeStandardProperties)
    return (results)
}

## Creates a new container, which may be a project, folder, or workbook, on the server.
##
labkey.security.createContainer <- function(baseUrl=NULL, parentPath, name = NULL, title = NULL, description = NULL, folderType = NULL, isWorkbook = FALSE)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(parentPath))
        stop (paste("A value must be specified for both baseUrl and parentPath."))

    ## normalize the folder path
    parentPath <- encodeFolderPath(parentPath)

    params <- list(isWorkbook = isWorkbook)
    if(is.null(name)==FALSE) {params <- c(params, list(name=name))}
    if(is.null(title)==FALSE) {params <- c(params, list(title=title))}
    if(is.null(description)==FALSE) {params <- c(params, list(description=description))}
    if(is.null(folderType)==FALSE) {params <- c(params, list(folderType=folderType))}

    url <- paste(baseUrl, "core", parentPath, "createContainer.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response))
}

## Deletes an existing container, which may be a project, folder, or workbook, and all of its children from the server.
##
labkey.security.deleteContainer <- function(baseUrl=NULL, folderPath)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath))
        stop (paste("A value must be specified for both baseUrl and folderPath."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    params <- list(folderPath = folderPath) # no params for this action but need an object for the post body

    url <- paste(baseUrl, "core", folderPath, "deleteContainer.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response))
}

## Moves an existing container, which may be a folder or workbook, to be the subfolder of another folder and/or project.
##
labkey.security.moveContainer <- function(baseUrl=NULL, folderPath, destinationParent, addAlias = TRUE)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath) || missing(destinationParent))
        stop (paste("A value must be specified for each of baseUrl, folderPath, and destinationParent."))

    params <- list(container = folderPath, parent = destinationParent)
    if(is.null(addAlias)==FALSE) {params <- c(params, list(addAlias=addAlias))}

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    url <- paste(baseUrl, "core", folderPath, "moveContainer.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response))
}

## Renames an existing container at the given container path. This action allows for updating the container
## name, title, or both.
##
labkey.security.renameContainer <- function(baseUrl=NULL, folderPath, name=NULL, title=NULL, addAlias=TRUE)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath))
        stop (paste("A value must be specified for both baseUrl and folderPath."))
    if (missing(name) && missing(title))
            stop (paste("A value must be specified for either name or title."))

    params <- list()
    if(is.null(name)==FALSE) {params <- c(params, list(name=name))}
    if(is.null(title)==FALSE) {params <- c(params, list(title=title))}
    if(is.null(addAlias)==FALSE) {params <- c(params, list(addAlias=addAlias))}

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    url <- paste(baseUrl, "admin", folderPath, "renameContainer.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (fromJSON(response))
}

labkey.security.impersonateUser <- function(baseUrl = NULL, folderPath, userId = NULL, email = NULL)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl) || missing(folderPath))
        stop (paste("A value must be specified for both baseUrl and folderPath."))
    if (missing(userId) && missing(email))
        stop (paste("A value must be specified for either userId or email."))

    ## normalize the folder path
    folderPath <- encodeFolderPath(folderPath)

    params <- list()
    if(!missing(userId)) {params <- c(params, list(userId = userId))}
    if(!missing(email)) {params <- c(params, list(email = email))}

    url <- paste(baseUrl, "user", folderPath, "impersonateUser.api", sep="")
    response <- labkey.post(url, toJSON(params, auto_unbox=TRUE))

    return (labkey.whoAmI())
}

labkey.security.stopImpersonating <- function(baseUrl = NULL)
{
    baseUrl=labkey.getBaseUrl(baseUrl)

    ## check required parameters
    if (missing(baseUrl) || is.null(baseUrl))
        stop (paste("A value must be specified for baseUrl."))

    url <- paste(baseUrl, "login/", "stopImpersonating.api", sep="")
    labkey.post(url, toJSON(list()))

    return (labkey.whoAmI())
}