#' @title Merge multiple column variable and create indicator variable
#' @description  Merge multiple column variable and create indicator variable
#' @param data a data.frame containing the column variable to merge
#' @param listVarToMerge a character vector, containing the colname of the column to merge
#' @param nonAppChar a character vector of length one, containing the colname of the column to merge
#' @param varOutputLabel a character vector of length one, containing the prefix of the columns to create
#' @return a data.frame
#' @noRd
int_manageMultipleColumnVariable <- function(data, listVarToMerge, nonAppChar = "NonApp", varOutputLabel) {

  if(!is.data.frame(data)){
    stop("data must be a data.frame")
  }
  if(!is.vector(listVarToMerge) || !is.character(listVarToMerge)) {
    stop("listVarToMerge must be a character vector")
  }
  if(!is.vector(nonAppChar) || !is.character(nonAppChar) || length(nonAppChar) != 1) {
    stop("nonAppChar must be a character vector of length one")
  }
  if(!is.vector(nonAppChar) || !is.character(nonAppChar) || length(nonAppChar) != 1) {
    stop("nonAppChar must be a character vector of length one")
  }
  if(!is.vector(varOutputLabel) || !is.character(varOutputLabel) || length(varOutputLabel) != 1) {
    stop("varOutputLabel must be a character vector of length one")
  }
  if(!all(listVarToMerge %in% colnames(data))) {
    stop("listVarToMerge must be colnames of data")
  }

  ## get listVarToMerge position in data
  columnPos <- which(colnames(data) %in% listVarToMerge)
  minColumnPos <- min(columnPos)

  ## get listModality to manage (exclude NA and nonAppChar)
  listModality <- unique(unlist(data[listVarToMerge]))
  listModality <- listModality[!is.na(listModality)]
  if(!is.null(nonAppChar)){
    listModality <- listModality[!listModality %in% nonAppChar]
  }
  listModality <- listModality[order(listModality)]

  ## get listColumnToCreate and check if in data yet
  listColumnToCreate <- paste0(varOutputLabel, "_", listModality, collapse = ", ")
  if(listColumnToCreate %in% colnames(data)[!colnames(data) %in% listVarToMerge]) {
    bool <- listColumnToCreate %in% colnames(data)[!colnames(data) %in% listVarToMerge]
    stop(paste("Column name collision :", paste0(listColumnToCreate[bool], collapse = ", "), "will be created whereas in data yet"))
  }

  ## set message column creation
  # message(paste(length(listModality), "modalities will be treated."))
  # message(paste("Creation of ", listColumnToCreate, "columns."))

  ## create new column
  result <- do.call("cbind", lapply(listModality, function(currentModality){
    currentModalityData <- data.frame(apply(data[listVarToMerge], 1, function(currentRow){
      if(all(is.na(currentRow))){
        return(NA)
      } else if(!is.null(nonAppChar) && all(currentRow[!is.na(currentRow)] == nonAppChar)) {
        return(nonAppChar)
      } else {
        result <- ifelse(currentModality %in% currentRow == TRUE, 1, 0)
        return(result)
      }
    }))
    colnames(currentModalityData) <- paste0(varOutputLabel, "_", currentModality)
    return(currentModalityData)
  }))

  ## remove listVarToMerge from data
  data <- data[-columnPos]

  ## add result into data in right place
  dataStart <- data[1:minColumnPos-1]
  if(ncol(data) >= minColumnPos){
    dataEnd <- data[(minColumnPos):ncol(data)]
  } else {
    dataEnd <- data[0]
  }
  data <- data.frame(dataStart, result, dataEnd)

  return(data)
}

#' @title A conveinient method to modify labelVar in case of multiple column variable management
#' @description A conveinient method to modify labelVar in case of multiple column variable management
#' @param data a data.frame, containing the data with the indicator variable created by manageMultipleColumnVariable
#' @param labelVar a data.frame with two columns : 1) the variable colname, 2) the clean label to display
#' @param labelQL a data.frame containing three columns : 1) the variable names, 2) one modality as digit, 3) the label of the modality.
#' @param varOutputLabel a character vector of length one, containing the prefix of the columns to create
#' @return a data.frame, the modified labelVar
#' @noRd
int_manageLabelVarMultipleColumnVariable <- function(data, labelVar, labelQL, varOutputLabel) {

  modalityToManage <- labelQL[labelQL[, 1] == varOutputLabel, ]
  variableToManage <- paste0(modalityToManage[, 1], "_", modalityToManage[, 2])
  labelVarMain <- as.character(labelVar[labelVar[, 1] == varOutputLabel, ][, 2])
  variableToAdd <- paste0(trimws(labelVarMain), ' (', trimws(labelQL[labelQL[, 1] == varOutputLabel, 3]), ')')

  bool <- variableToManage %in% colnames(data)
  variableToManage <- variableToManage[bool]
  variableToAdd <- variableToAdd[bool]

  labelVar <- labelVar[-which(labelVar[, 1] == varOutputLabel), ]

  resultToAdd <- data.frame(
    variableToManage,
    variableToAdd)
  colnames(resultToAdd) <- colnames(labelVar)

  labelVar <- rbind(labelVar, resultToAdd)
  return(labelVar)

}

#' @title A conveinient method to modify labelQL in case of multiple column variable management
#' @description A conveinient method to modify labelQL in case of multiple column variable management
#' @param data a data.frame, containing the data with the indicator variable created by manageMultipleColumnVariable
#' @param labelQL a data.frame containing three columns : 1) the variable names, 2) one modality as digit, 3) the label of the modality.
#' @param varOutputLabel a character vector of length one, containing the prefix of the columns to create
#' @return a data.frame, the modified labelVar
#' @noRd
int_manageLabelQLMultipleColumnVariable <- function(data, labelQL, varOutputLabel) {
  modalityToManage <- labelQL[labelQL[, 1] == varOutputLabel, ]
  variableToAdd <- paste0(modalityToManage[, 1], "_", modalityToManage[, 2])
  variableToAdd <- variableToAdd[variableToAdd %in% colnames(data)]
  result <- data.frame(
    Variable = rep(variableToAdd, 2, each = 2),
    Modality = rep(c(0, 1), length(variableToAdd)),
    Label = rep(c('Non', 'Oui'), length(variableToAdd))
  )
  colnames(result) <- colnames(labelQL)
  result <- rbind(labelQL[labelQL[, 1] != varOutputLabel, ], result)
  if(is.factor(result[, 1])){
    result[, 1] <- as.factor(as.character(result[, 1]))
  }
  return(result)
}

#' @title A conveinient method to modify label dictionnary in case of multiple column variable management
#' @description  A conveinient method to modify label dictionnary in case of multiple column variable management
#' @param data a data.frame, containing the data with the indicator variable created by manageMultipleColumnVariable
#' @param labelVar a data.frame with two columns : 1) the variable colname, 2) the clean label to display
#' @param labelQL a data.frame containing three columns : 1) the variable names, 2) one modality as digit, 3) the label of the modality.
#' @param varOutputLabel a character vector of length one, containing the prefix of the columns to create
#' @return a list of two data.frame : 1) labelvar, 2) labelQL
#' @noRd
int_manageLabelDictionnaryMultipleColumnVariable <- function(data, labelVar, labelQL, varOutputLabel){

  if(!is.data.frame(data)){
    stop("data must be a data.frame")
  }
  if (!is.data.frame(labelVar) | ncol(labelVar) != 2) {
    stop("labelVar must be a data.frame of 2 column")
  }
  if (!is.data.frame(labelQL)) {
    stop("labelQL must be a data.frame")
  }
  if (!varOutputLabel %in% labelVar[, 1]){
    stop("varOutputLabel must be a variable described in labelVar")
  }
  if (!varOutputLabel %in% labelQL[, 1]){
    stop("varOutputLabel must be a variable described in labelQL")
  }

  labelVar <- int_manageLabelVarMultipleColumnVariable(data = data, labelQL = labelQL, labelVar = labelVar, varOutputLabel = varOutputLabel)
  labelQL <- int_manageLabelQLMultipleColumnVariable(data = data, labelQL = labelQL, varOutputLabel = varOutputLabel)
  return(list(labelVar, labelQL))
}
