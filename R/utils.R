#' @title A conveinient method to manage multiple column data and label dictionnary
#' @description  A conveinient method to manage multiple column data and label dictionnary
#' @param data a data.frame, containing the data with the indicator variable created by manageMultipleColumnVariable
#' @param labelVar a data.frame with two columns : 1) the variable colname, 2) the clean label to display
#' @param labelQL a data.frame containing three columns : 1) the variable names, 2) one modality as digit, 3) the label of the modality.
#' @param listVarToMerge a character vector, containing the colname of the column to merge
#' @param nonAppChar a character vector of length one, containing the colname of the column to merge
#' @param varOutputLabel a character vector of length one, containing the prefix of the columns to create
#' @return a list of three data.frame : 1) data, 2) labelVar, 3) labelQL
#' @export
manageMultipleColumnVariableAndDictionnary <- function(data, labelVar, labelQL, listVarToMerge, nonAppChar = "NonApp", varOutputLabel) {
  data <- int_manageMultipleColumnVariable(data = data, listVarToMerge = listVarToMerge, nonAppChar = nonAppChar, varOutputLabel = varOutputLabel)
  manageDictionnary <- int_manageLabelDictionnaryMultipleColumnVariable(data = data, labelVar = labelVar, labelQL = labelQL, varOutputLabel = varOutputLabel)
  return(list(data, manageDictionnary[[1]], manageDictionnary[[2]]))
}
