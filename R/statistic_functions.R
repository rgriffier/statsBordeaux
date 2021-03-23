#' @title Check if data are only digits
#' @description Check if the input data.frale are only composed of digits
#' @param returnError boolean. If TRUE, return a data.frame with the coordinate of the cell
#' @param data a data.frame containing the data to check
#' @param excelFormat a boolean vector of length one. If TRUE, data.frame containing position of none
#' digit cell is in excel format. Default to TRUE.
#' @return TRUE if the data.frame only contain digit, else return a data.frame
#' with the coordinate of the cell containing some wrond format data in excel style. FALSE by default.
#' @export
#' @examples
#' data(mtcars)
#' checkNotDigitInDataframe(data = mtcars, returnError = FALSE)
#' mtcars$NOT_DIGIT <- rep("A", nrow(mtcars))
#' errorPosition <- checkNotDigitInDataframe(data = mtcars, returnError = TRUE)
checkNotDigitInDataframe <- function (data, returnError = FALSE, excelFormat = TRUE){
  if(!is.data.frame(data)) {
    stop("data must be a data.frame")
  }

  ## remove full space element
  data <- data.frame(lapply(data, function(x) {
    gsub("^[ \t\r\n]+$", NA, x)
  }))

  ## data as.character
  data[] <- lapply(data, as.character)

  ## get non num element
  notNum <- sapply(data, function(x) {
    !grepl("^-*[0-9]+(\\.[0-9]+)?$", x) & !is.na(x)
  })

  if(any(notNum)){
    ## get position of error
    if(returnError){
      ## get position of non digit as vector
      notNumPos <- which(sapply(notNum, isTRUE))
      ## compute the modulo depend on the number of data's rows to get the row index
      numRow <- notNumPos %% nrow(data)
      ## compute the division by the number of row and add one to get the col index (begining to one)
      numCol <- floor(notNumPos/nrow(data)) + 1
      ## manage case where row index equal to 0 (last row)
      numCol[numRow == 0] <- numCol[numRow == 0]-1
      numRow[numRow == 0] <- nrow(data)
      ## get the leter representation of the col index
      letterCol <- paste0(do.call("c", lapply(floor(ifelse(numCol%%26 == 0, numCol-1, numCol)/26), function(x){ifelse(length(LETTERS[x]) == 0, "", LETTERS[x])})), ifelse(numCol%%26 == 0, 'Z', LETTERS[numCol%%26]))
      ## save result in a data.frame
      error <- data.frame(Lignes = numRow, Colonnes = numCol, Contenu = NA, stringsAsFactors = FALSE)
      ## for each row of error, get the text element corresponding in data
      error$Contenu <- apply(error, 1, function(x){ data[x["Lignes"], x["Colonnes"]]})
      if(excelFormat){
        ## add one to each index row to take care of header
        error$Lignes <- error$Lignes + 1
        ## replace col index by col letter
        error$Colonnes <- letterCol
      }
      return(error)
    } else{
      return(FALSE)
    }
  } else{
    return(TRUE)
  }
}



#' @title Get a summary for the given data.frame
#' @description Get a summary for the given data.frame
#' @param data a data.frame or tbl_df
#' @return a data.frame containing the metada description
#' @export
#' @examples
#' data(mtcars)
#' describeMetadata(mtcars)
describeMetadata <- function(data){

  if(!is.data.frame(data)){
    stop("data must be a data.frame")
  }

  # convert tbl_df as data.frame
  if(any(class(data) %in% "tbl_df")){
    data <- as.data.frame(data)
  }

  # for each columns
  metadataDescription <- do.call(rbind, (lapply(colnames(data), function(x){

    # get var label if exist
    varLabel <- attributes(data[, x])$var_label
    if(is.null(varLabel)){
      varLabel <- attributes(data[, x])$label
      if(is.null(varLabel)){
        varLabel <- NA
      }
    }

    # compute N_AVAILABLE, N_MISSING DATA and DATA_TYPE
    N_AVAILABLE <- sum(!is.na(data[, x]))
    N_MISSING <- sum(is.na(data[, x]))
    DATA_TYPE <- class(data[, x])
    LEVELS_NB <- length(levels(data[, x]))
    if(LEVELS_NB <= 20){
      LEVELS_CHAR <- paste0(levels(data[, x]), collapse = ", ")
    } else {
      LIST_LEVELS <- levels(data[, x])
      LIST_LEVELS <- LIST_LEVELS[1:20]
      LEVELS_CHAR <- paste0("** ", (LEVELS_NB-20), " levels more ** : ", paste0(LIST_LEVELS, collapse = ", "))
    }
    # if numeric DATA_TYPE, compute N_INF
    if(DATA_TYPE %in% c("numeric", "integer", "double")){
      N_INF <- sum(is.infinite(data[, x]))
    } else {
      N_INF <- NA
    }


    # arrange metadata description
    metadataDescription <- data.frame(VAR = x,
                                      VAR_LABEL = varLabel,
                                      DATA_TYPE = class(data[, x]),
                                      LEVELS_NB = ifelse(test = DATA_TYPE == 'factor', yes = LEVELS_NB, no = NA),
                                      LEVELS = ifelse(test = DATA_TYPE == 'factor',
                                                      yes = LEVELS_CHAR,
                                                      no = NA),
                                      N_AVAILABLE = N_AVAILABLE,
                                      PROP_AVAILABLE = round(N_AVAILABLE/nrow(data), 2),
                                      N_MISSING = N_MISSING,
                                      PROP_MISSING = round(N_MISSING/nrow(data), 2),
                                      N_INF = N_INF,
                                      PROP_INF = round(N_INF/nrow(data), 2),
                                      UNIQUE = length(unique(data[, x])))

    return(metadataDescription)
  })))
  return(metadataDescription)
}


#' @title Format some float in the french p-value format for display usage
#' @description Manage the display of p-value format in french format
#' @param pvalues a numeric, witch need to be formated
#' @param round an integer, the number of digit after the decimal separator. Default to 3
#' @return a formated character
#' @export
#' @examples
#' pvalue <- 0.00028
#' pvalue_f <- setFormatToPvalue(pvalue)
#' pvalue_f
setFormatToPvalue <- function(pvalue, round = 3){
  if(!is.vector(pvalue) | !is.numeric(pvalue)){
    stop("p-value must be a numeric vector")
  }
  if(!is.vector(round) | !is.numeric(round) | length(round) !=1){
    stop("round must be a integer vector of length 1")
  }
  pv <- format.pval(
    pv = round(pvalue, round),
    eps = 0.001,
    nsmall = round,
    digits = round,
    na.form = "-"
  )

  if(!is.null(getOption('lang.value')) && getOption('lang.value') == 'FR'){
    pv <- trimws(gsub('\\.', ',', pv))
  } else {
    pv <- trimws(pv)
  }

  return(pv)
}


#' @title Format some digit in the french format for display usage
#' @description Manage the display digit format in french format
#' @param pvalues a numeric, witch need to be formated
#' @param round an integer, the number of digit after the decimal separator. Default to 3
#' @return a formated character
#' @export
#' @examples
#' digit <- 12.359
#' digit_f <- setFormatToNumber(digit)
#' digit_f
setFormatToNumber <- function(num, round = 3){
  if(!is.numeric(num)){
    stop("num must be a numeric vector")
  }
  if(!is.vector(round) | !is.numeric(round) | length(round) !=1){
    stop("round must be a integer vector of length 1")
  }
  num <- round(num, round)
  if(!is.null(getOption('lang.value')) && getOption('lang.value') == 'FR'){
    num <- gsub('\\.', ',', num)
  }
  return(num)
}


#' @title Test normality of numeric variable based on graphic and statistc methods
#' @description Test normality of numeric variable based on graphic and statistc methods
#' @param data a data.frace conteting the numeric data witch normality need to be evaluate
#' @param variable a character vector. The name of the numeric columns to describe.
#' @param group a character vector of length 1. The name of the factor column to use as commparaison group. Default to NULL.
#' @param p_value a boolean. If TRUE, comparaison test to normality are performed. Default to FALSE.
#' @param method a character vector of length 1. The name og the statistic method to use : 'Kolmogorov' or 'Shapiro'
#' @return a plot
#' @importFrom ggpubr ggtexttable ggparagraph ggarrange
#' @import ggplot2
#' @export
#' @examples
#' data(mtcars)
#' checkNormality(mtcars, "wt", p_value = TRUE)
checkNormality <- function(data, variable = colnames(data), group = NULL, p_value = FALSE, method = 'Kolmogorov'){
  listPlot <- lapply(variable, function(currentVar){
    if(is.numeric(data[, currentVar])){
      plot <- checkNormalityInternal(data = data,
                                     variable = currentVar,
                                     group = group,
                                     p_value = p_value,
                                     method = method)
    } else {
      plot <- NULL
    }
  })
  listPlot[sapply(listPlot, is.null)] <- NULL
  return(listPlot)
}


#' @title set label to qualitative variable into a data.frame based on dictionnary.
#' @description manage the conversion and sat label to qualitative into a data.frame based
#' on dictionnary.
#' @param data a data.frame containing qualitative variable witch need to be labelised.
#' @param factorLevelsLabel a data.frame containing variable names / modality as digit / label of the modality.
#' @param varIndex a numeric vector of length one, containing the index of the variable column.
#' @param levelsIndex a numeric vector of length one, dontaining the index of the levels column.
#' @param levelsThreesholdCheck a numeric vector of length one, containing the disctinct
#' number value threeshold to check to evaluate if all factor variable are listed in the
#' factorLevelsLabel table
#' @param labelIndex a numeric vector of length one, containing the index of the labels column.
#' @return a data.frame with qualitative variable in the factorLevelsLabel convert as factor with clean label
#' @importFrom plyr revalue
#' @export
#' @examples
#' data <- data.frame(X1 = c(0, 1, 2, 1, 2, 1, 2),
#' X2 = c(0, 1, 0, 1, 1, 0, 0))
#' labelTable <- data.frame(VARIABLE = c("X2", "X2"),
#'                          MODALITY = c(0, 1),
#'                          LABEL = c("Male", "Female"))
#' data <- setLabelToFactorLevels(data, labelTable)
setLabelToFactorLevels <- function(data, factorLevelsLabel, varIndex = 1, levelsIndex = 2, labelIndex = 3, levelsThreesholdCheck = 5){
  if(!is.data.frame(data)){
    stop("data must be a data.frame")
  }
  if(!is.data.frame(factorLevelsLabel)){
    stop("factorLevelsLabel must be a data.frame")
  }

  ## case of duplicated in factorLevelsLabel
  factorLevelsLabel <- unique(factorLevelsLabel)

  listeVariableQL <- unique(as.character(factorLevelsLabel[, varIndex]))
  diff <- setdiff(listeVariableQL, colnames(data))
  if(length(diff) > 0){
    stop(paste0("\nVariable '", diff, "' is in levels table and not in data."))
  }

  # convert all variable as numeric if possible
  data[] <- lapply(data, function(currentColumn){
    if(isCastableAsNumeric(currentColumn)){
      return(as.numeric(as.character(currentColumn)))
    } else {
      return(currentColumn)
    }
  })


  # all variable in the factorLevelsLabel data.frame are converted as factor
  data[, listeVariableQL] <- lapply(data[listeVariableQL], as.factor)

  ## set label to each factor modality
  for(currentVar in listeVariableQL){
    factorLevels <- factorLevelsLabel[factorLevelsLabel[varIndex] == currentVar, labelIndex]
    factorLevels <- as.factor(as.character(factorLevels))
    data[, currentVar] <- factorLevels[match(data[, currentVar], factorLevelsLabel[factorLevelsLabel[varIndex] == currentVar, levelsIndex])]
    data[, currentVar] <- factor(data[, currentVar], levels = unique(factorLevelsLabel[factorLevelsLabel[varIndex] == currentVar, ][, labelIndex]))
  }

  ## manage full digit column in french and english format
  data[] <- lapply(data, function(currentColumn){
    if(any(class(currentColumn) %in% c("character", "difftime"))){
      isDigit <- grepl("^-*\\d+((\\.|,)\\d+)*$", currentColumn)
      isNa <- is.na(currentColumn)
      if(!any(!as.logical(isDigit + isNa))){
        currentColumn <- as.numeric(gsub(pattern = ",", replacement = "\\.", x = currentColumn))
      }
    }
    return(currentColumn)
  })

  ## check if some variable may be factor
  lapply(seq_along(data), function(x){
    if(!is.factor(data[, x])){
      if(length(unique(data[, x])) <= levelsThreesholdCheck){
        warning(paste0("Variable '", colnames(data[x]), "' is considered as ",  class(data[,x]), " but may be a factor variable (", length(unique(data[, x])), ' distinct values).'), call. = FALSE)
      }
    }
  })

  return(data)
}



#' @title set label to each column of data.frame based on dictionnaty
#' @description add a label as attribute (var_label) to each column of data.frame based on dictionnary. This
#' label will be used during the reporting function.
#' @param data the data.frame witch nedd to be labelised
#' @param varIndex a numeric vector of length one, containing the index if the variable label column.
#' @param labelIndex a numeric vector of length one, containing the index if the variable label column.
#' @param varLabel a data.frame with two columns : 1) the variable colname, 2) the clean label to display
#' @return a data.frame with the var_label attribute
#' @export
#' @examples
#' data(mtcars)
#' label <- data.frame(VARIABLE = colnames(mtcars),
#'                     LABEL = c("Miles/(US) gallon",
#'                               "Number of cylinders",
#'                               "Displacement (cu.in.)",
#'                               "Gross horsepower",
#'                               "Rear axle ratio",
#'                               "Weight (1000 lbs)",
#'                               "1/4 mile time",
#'                               "Engine (0 = V-shaped, 1 = straight)",
#'                               "Transmission (0 = automatic, 1 = manual)",
#'                               "Number of forward gears",
#'                               "Number of carburetors"),
#'                     stringsAsFactors = FALSE)
#'
#' labeled_df <- setLabelToVariable(mtcars, label)
#' lapply(labeled_df, function(x) attributes(x)$`var_label`)
setLabelToVariable <- function (data, varLabel, varIndex = 1, labelIndex = 2){
  if(!is.data.frame(data)){
    stop("data must be a data.frame")
  }

  # check if some variable in data are not in varLabel
  lapply(colnames(data)[which(!colnames(data) %in% varLabel[, varIndex])], function(x){
    warning(paste0("Variable '", x, "' is not in the table containing the variable's label."), call. = FALSE)
  })

  listvarLabel <- as.character(varLabel[, varIndex])
  if(!all(listvarLabel %in% colnames(data))){
    diff <- setdiff(listvarLabel, colnames(data))
    if(length(diff) > 0) {
      stop(paste0("\nVariable '", diff, "' is in varLabel data.frame and not in data."))
    }
  }

  for(i in 1:nrow(varLabel)){
    indexVarInData <- which(colnames(data) == varLabel[i, varIndex])
    if(length(indexVarInData) == 1){
      attr(data[, indexVarInData], "var_label") <- as.character(varLabel[i, labelIndex])
      attr(data[, indexVarInData], "label") <- as.character(varLabel[i, labelIndex])
    }
  }
  return(data)
}


#' @title Get the label of variable in a data.frame
#' @description Get the label of variables witch were labelised thanks the setLabel function
#' @param variable a character vector, containing the colnames to querry
#' @param data a data.frame containing colname to get variable label
#' @return a named vector of the label of each variable
#' @export
#' @examples
#' data(mtcars)
#' label <- data.frame(VARIABLE = colnames(mtcars),
#'                     LABEL = c("Miles/(US) gallon",
#'                               "Number of cylinders",
#'                               "Displacement (cu.in.)",
#'                               "Gross horsepower",
#'                               "Rear axle ratio",
#'                               "Weight (1000 lbs)",
#'                               "1/4 mile time",
#'                               "Engine (0 = V-shaped, 1 = straight)",
#'                               "Transmission (0 = automatic, 1 = manual)",
#'                               "Number of forward gears",
#'                               "Number of carburetors"),
#'                     stringsAsFactors = FALSE)
#'
#' labeled_df <- setLabelToVariable(mtcars, label)
#' getVarLabel(labeled_df)
getVarLabel <- function(data, variable = colnames(data)){
  getVarLabel_int(data, variable)
}


#' @title Subset data keeping attributes
#' @description  Allows to subset data and keeping all the attributes of the data
#' @param data a data.frame
#' @param subset a logical vector
#' @return a data.frame with all the attributes
#' @export
#' @examples
#' data(mtcars)
#' mtcars$nSpeed <- c(5, 6, 5, "NA", "NA", "NA", "NA", "NA", "NA", "NA",
#'                   "NA", "NA", "NA", "NA", "NA", "NA", "NA", 6, 6, 6,
#'                   "NA", "NA", "NA", "NA", "NA", 5, 5, 6, 5, 6, 6, 5)
#' attributes(mtcars[, "nSpeed"])$var_label <- "Number of speed in case of manual transmission"
#' subset <- subset_withAttributes(mtcars, mtcars$nSpeed != "NA")
#' attributes(subset[, "nSpeed"])
subset_withAttributes <- function(data, subset){
  if(!is.data.frame(data)){
    stop("data must be a data.frame")
  }
  if(!is.vector(subset) || !is.logical(subset) || length(subset) != nrow(data)){
    stop("subset must be a boolean vector with the same length than data rownum")
  }

  subsetData <- subset(data, subset = subset)
  for(i in 1:ncol(data)){
    attr <- attributes(data[, i])
    attributes(subsetData[, i]) <- attr
  }
  return(subsetData)
}


#' @title Manage the not applicable condition into data.frame
#' @description Identify the not applicable char in data.frame and return clean list with :
#' 1) data.frame without not applicable character, 2) list of logical vector identifying the row witch should contain data
#' @param data a data.frame containing data to clean
#' @param notApplicableChar a character vector of length 1, with the not applicable character as in the data.frame
#' @return a list of two elements : 1) data.frame without not applicable character, 2) list of logical vector
#' identifying the row witch should contain data
#' @export
#' @examples
#' data <- data.frame(X1 = c(1, 1, 0, 1, 1, 0, 1, 1, 1),
#'                    X2 = c(0, 0, "NC", 1, 1, "NC", 0, 0, 1))
#' resultNotApplicable <- manageNotApplicable(data, "NC")
#' data <- resultNotApplicable[[1]]
#' resultNotApplicable <- resultNotApplicable[[2]]
manageNotApplicable <- function(data, notApplicableChar = 'NonApp'){
  if(!is.data.frame(data)){
    stop("data must be a data.frame")
  }
  if(!is.vector(notApplicableChar) | !is.character(notApplicableChar) | length(notApplicableChar) !=1 ){
    stop("notApplicableChar must be a character vector of length 1")
  }
  ## get notApplicableChar math
  resultNotApplicable <- lapply(data, function(currentColumn){as.character(currentColumn) != notApplicableChar})
  resultNotApplicable <- lapply(resultNotApplicable, function(currentColumn){
    currentColumn[is.na(currentColumn)] <- TRUE
    return(currentColumn)
  })

  ## remove notApplicableChar math
  data[] <- lapply(data, function(currentColumn){
    if(class(currentColumn) %in% c('integer', 'numeric', 'character')){
      currentColumn[currentColumn == notApplicableChar] <- NA
      return(currentColumn)
    } else {
      return(currentColumn)
    }
  })
  data[] <- lapply(data, function(currentColumn){
    if(is.factor(currentColumn)){
      return(as.factor(as.character(currentColumn)))
    } else {
      return(currentColumn)
    }
  })
  ## save data and notApplicable into a list
  result <- list(data, resultNotApplicable)
  names(result) <- c("data", "notApplicable")
  return(result)
}

#' @title create a data.frame with col and row as specified
#' @description create a data.frame withe number of columns and rows as specicied
#' @param ncol a digit. Default to 0
#' @param nrow a digit. Default to 0
#' @return a data.frame
#' @export
#' @examples
#' result <- createOutput(ncol = ncol(mtcars))
#' colnames(result) <- colnames(mtcars)
createOutput <- function(ncol = 0, nrow = 0) {
  if(!is.vector(ncol) | !is.numeric(ncol) | length(ncol) != 1){
    stop("ncol must be a vector of length 1")
  }
  if(!is.vector(nrow) | !is.numeric(nrow) | length(nrow) != 1){
    stop("nrow must be a vector of length 1")
  }
  df <- data.frame(matrix(ncol = ncol, nrow = nrow))
  return(df)
}


#' @title Generate description of numeric varaible.
#' @description A method that generate description of numeric variable and perform comparative statistic test
#' in case of comparaison group.
#' @param data a data.frame containing the data to describe
#' @param variable a character vector of length 1. The name of the numeric column to describe.
#' @param group a character vector of length 1. The name of the factor column to use as commparaison group. Default to NULL.
#' @param group_str a character vector. The name of the levels of the group variable to use. Default to NULL.
#' @param all a boolean. If TRUE, total column will be displayed. Default to FALSE
#' @param round an integer, number of maximal decimal. Default to 3
#' @param confint a boolean. If TRUE, the confidence interval of the mean will be displayed. Default to FALSE
#' @param desc a character vector. Could contain "Mean", "Median", "Range" and/or "Mode"
#' @param p_value a boolean. If TRUE, comparaison test are performed.
#' @param forcedTest a character vector of length 1. Must be one of "t-test for inequal variances", "t-test for equal variances",
#' "Wilcoxon test", "Analysis of variance", or "Kruskal-Wallis test". Default to NULL.
#' @param NA_group_AsModality a boolean. If TRUE, missing data of the group variable will be considered as levels.
#' Default to FALSE
#' @param output a data.frame, containing a previously generated data.frame or a new data.frame.
#' data.frame are generated using createOutput() function.
#' @return a data.frame containing the description of the variable
#' @export
#' @examples
#' data(mtcars)
#' mtcars$am <- as.factor(mtcars$am)
#' output <- createOutput()
#' output <- statsQT(output = output,
#'                   data = mtcars,
#'                   variable = "mpg",
#'                   group = "am")
#' output <- statsQT(output = output,
#'                   data = mtcars,
#'                   variable = "disp",
#'                   group = "am")
statsQT <- function(data, variable, group = NULL, group_str = NULL, all = FALSE, round = 3,
                    confint = FALSE, desc = c("Mean", "Median", "Range"),
                    p_value = FALSE, forcedTest = NULL, NA_group_AsModality = FALSE, output = NULL) {

  # PARAM TEST PART ------------------------------------------------------- ####

  if(!is.data.frame(data)){
    stop("data must be a data.frame.")
  }
  if(!is.vector(variable) | !is.character(variable) | length(variable) != 1){
    stop("variable must be a character vector of length one.")
  }
  if(!variable %in% colnames(data)){
    stop("variable must be the name of a column in data.")
  }
  if(!is.numeric(data[, variable])){
    stop("variable must be a numeric variable.")
  }
  if(!is.null(group)){
    if(!is.vector(group) | !is.character(group) | length(group) != 1){
      stop("group must be a character vector of length one.")
    }
    if(!group %in% colnames(data)){
      stop("group must be the name of a column in data.")
    }
    if(!is.factor(data[, group])){
      stop("group must be a factor variable.")
    }
  }
  if(!is.null(group_str)){
    if(!is.vector(group_str) | !is.numeric(group_str) | !length(group_str) >= 1){
      stop("group_str must be a character vector of length one.")
    }
    if(any(is.na(group_str))){
      stop("Use NA_group_AsModality to keep NA modality in stats.")
    }
    if(!all(group_str %in% which(!is.na(levels(data[, group]))))) {
      stop(paste0("group_str must be in ", paste0(1:length(levels(data[, group])), " (", levels(data[, group]), ")", collapse = ", ")))
    }
  }
  if(!is.vector(all) | !is.logical(all) | length(all) != 1){
    stop("all must be a boolean vector of length one.")
  }
  if(!is.vector(round) | !is.numeric(round) | !length(round) >= 1){
    stop("round must be a numeric vector of length one.")
  }
  if(!is.vector(confint) | !is.logical(confint) | length(confint) !=1){
    stop("confint must a logical vector of length 1")
  }
  if(length(setdiff(desc, c("Mean", "Median", "Range", "Mode"))) != 0) {
    stop(paste0("desc must be a characher vector composer with 'Mean', 'Median', 'Range' or 'Mode'"))
  }
  if(!is.vector(p_value) | !is.logical(p_value) | length(p_value) != 1){
    stop("p_value must be a boolean vector of length one.")
  }
  if(!is.null(forcedTest)){
    if(!is.vector(forcedTest) | !is.character(forcedTest) | length(forcedTest) != 1){
      stop("p_value must be a boolean vector of length one.")
    }
    if(!forcedTest %in% c("t-test for inequal variances", "t-test for equal variances", "Wilcoxon test", "Analysis of variance", "Kruskal-Wallis test")){
      stop("forcedTest must be in \"t-test for inequal variances\", \"t-test for equal variances\", \"Wilcoxon test\", \"Analysis of variance\", or \"Kruskal-Wallis test\"")
    }
  }
  if(!is.vector(NA_group_AsModality) | !is.logical(NA_group_AsModality) | length(NA_group_AsModality) != 1){
    stop("NA_group_AsModality must be a boolean vector of length one.")
  }

  # DATA SELECTION PART --------------------------------------------------- ####

  ## keep only data that match with group_str
  if(!is.null(group_str)){
    data <- subset_withAttributes(
      data = data,
      subset = data[, group] %in% c(levels(data[, group])[group_str], NA)
    )
  }

  ## set NA as m.d. in group and use it as factor level
  if(NA_group_AsModality){
    data[, group] <- addNA(data[, group])
    levels(data[, group])[is.na(levels(data[, group]))] <- getMissingData_text(getOption('lang.value'))
  }

  ## remove unuse modality in group variable
  # if(!is.null(group)){
  #   attr <- attributes(data[, group])
  #   data[, group] <- as.factor(as.character(data[, group]))
  #   attr(data[, group], 'var_label') <- attr$var_label
  # }

  # DESCRIPTION PART ------------------------------------------------------ ####

  internalOutput <- NULL

  ## if group is null or all = TRUE : QT description for all population
  if(is.null(group) | all){
    internalOutput <- statQT(data = data,
                             variable = variable,
                             desc = desc,
                             round = round,
                             confint = confint)

    colnames(internalOutput) <- c('Variable', 'Modality', 'Description', 'All')
  }

  if(!is.null(group)){
    ## if group is not null : QT description in each sub group
    dataSubGroup <- getSubGroupFromDataFrame(df = data, variable = variable, group = group,
                                             NA_group_AsModality = NA_group_AsModality)

    for(i in 1:length(dataSubGroup)){
      currentResult <- statQT(data = dataSubGroup[[i]],
                              variable = variable,
                              desc = desc,
                              round = round,
                              confint = confint)
      colnames(currentResult) <- c('Variable', 'Modality', 'Description', levels(data[, group])[i])
      if(is.null(internalOutput)){
        internalOutput <- currentResult
      } else {
        internalOutput <- cbind(internalOutput, currentResult[4])
      }
    }
  }

  # COMPARAISON PART ------------------------------------------------------ ####

  if(!is.null(group) & p_value){
    if(is.null(forcedTest)){
      test_to_use <- getTest_numericComparaison(dataSubGroup)
    } else {
      test_to_use <- forcedTest
    }

    ## set right test depend test_to_use value
    switch(test_to_use,
           ## t-test for inequal variances
           `t-test for inequal variances` = {
             test <- t.test(data[, variable] ~ data[, group], var.equal = FALSE)
             internalOutput <- cbind(internalOutput, data.frame(test = c(getInequalVarianceStudent_text(getOption('lang.value')), rep(NA, nrow(internalOutput)-1)),
                                                                p_value = c(setFormatToPvalue(test$p.value, round),
                                                                            rep(NA, nrow(internalOutput)-1))))
             colnames(internalOutput)[(ncol(internalOutput)-1):(ncol(internalOutput))] <- c("Test", "p-value")
           },

           ## t-test for equal variances
           `t-test for equal variances` = {
             test <- t.test(data[, variable] ~ data[, group], var.equal = TRUE)
             internalOutput <- cbind(internalOutput, data.frame(test = c(getEqualVarianceStudent_text(getOption('lang.value')), rep(NA, nrow(internalOutput)-1)),
                                                                p_value = c(setFormatToPvalue(test$p.value, round),
                                                                            rep(NA, nrow(internalOutput)-1))))
             colnames(internalOutput)[(ncol(internalOutput)-1):(ncol(internalOutput))] <- c("Test", "p-value")
           },

           ## Wilcoxon test
           `Wilcoxon test` = {
             test <- wilcox.test(data[, variable] ~ data[, group], correct = TRUE)
             internalOutput <- cbind(internalOutput, data.frame(test = c(getWilcoxon_text(getOption('lang.value')), rep(NA, nrow(internalOutput)-1)),
                                                                p_value = c(setFormatToPvalue(test$p.value, round),
                                                                            rep(NA, nrow(internalOutput)-1))))
             colnames(internalOutput)[(ncol(internalOutput)-1):(ncol(internalOutput))] <- c("Test", "p-value")
           },

           ## Analysis of variance
           `Analysis of variance` = {
             test <- aov(data[, variable] ~ data[, group])
             internalOutput <- cbind(internalOutput, data.frame(test = c(getAnova_text(getOption('lang.value')), rep(NA, nrow(internalOutput)-1)),
                                                                p_value = c(setFormatToPvalue(summary(test)[[1]][1, 'Pr(>F)'], round),
                                                                            rep(NA, nrow(internalOutput)-1))))
             colnames(internalOutput)[(ncol(internalOutput)-1):(ncol(internalOutput))] <- c("Test", "p-value")
           },

           ## Kruskal-Wallis test
           `Kruskal-Wallis test` = {
             test <- kruskal.test(data[, variable] ~ data[, group])
             internalOutput <- cbind(internalOutput, data.frame(test = c(getKruskal_text(getOption('lang.value')), rep(NA, nrow(internalOutput)-1)),
                                                                p_value = c(setFormatToPvalue(test$p.value, round),
                                                                            rep(NA, nrow(internalOutput)-1))))
             colnames(internalOutput)[(ncol(internalOutput)-1):(ncol(internalOutput))] <- c("Test", "p-value")
           },

           ## No test
           {
             internalOutput <- cbind(internalOutput, data.frame(test = c(test_to_use, rep(NA, nrow(internalOutput)-1)),
                                                                p_value = c(rep(NA, nrow(internalOutput)))))
             colnames(internalOutput)[(ncol(internalOutput)-1):(ncol(internalOutput))] <- c("Test", "p-value")
           })
  }

  # ATTRIBUTES MANAGEMENT ------------------------------------------------- ####

  if (!is.null(group)) {
    attr(internalOutput, 'var_group') <- group
    attr(internalOutput, 'label_var_group') <- getVarLabel(data = data, variable = group)
    if (!is.null(group_str)) {
      if(NA_group_AsModality){
        group_str <- c(group_str, NA)
      }
      attr(internalOutput, 'modality_var_group') <- group_str
      attr(internalOutput, 'label_modality_var_group') <- levels(data[,group])
    }
  }

  output <- rbind(output, internalOutput)

  return(output)
}


#' @title Generate description of factor varaible.
#' @description A method that generate description of factor variable and perform comparative statistic test
#' in case of comparaison group.
#' @param data a data.frame containing the data to describe
#' @param variable a character vector of length 1. The name of the factor column to describe.
#' @param group a character vector of length 1. The name of the factor column to use as commparaison group. Default to NULL.
#' @param group_str a character vector. The name of the levels of the group variable to use. Default to NULL.
#' @param all a boolean. If TRUE, total column will be displayed. Default to FALSE
#' @param round an integer, number of maximal decimal. Default to 3
#' @param p_value a boolean. If TRUE, comparaison test are performed.
#' @param forcedTest a character vector of length 1. Must be one of "Pearson's Chi-squared test", "Pearson's Chi-squared test with Yates" or
#' "Fisher's Exact Test for Count Data". Default to NULL.
#' @param NA_group_AsModality a boolean. If TRUE, missing data of the group variable will be considered as levels.
#' Default to FALSE
#' @param NA_asModality a boolean. If TRUE, missing data of the factor variable to describe will be considered as levels.
#' Default to FALSE
#' @param output a data.frame, containing a previously generated data.frame or a new data.frame.
#' data.frame are generated using createOutput() function.
#' @return a data.frame containing the description of the factor variable
#' @export
#' @examples
# data(mtcars)
# mtcars$am <- as.factor(mtcars$am)
# mtcars$vs <- as.factor(mtcars$vs)
# output <- createOutput()
# output <- statsQT(output = output,
#                   data = mtcars,
#                   variable = "mpg",
#                   group = "am")
# output <- statsQL(output = output,
#                   data = mtcars,
#                   variable = "vs",
#                   group = "am")
statsQL <- function(data, variable, group = NULL, group_str = NULL, all = NA_asModality, round = 3,
                    p_value = FALSE, forcedTest = NULL, NA_group_AsModality = FALSE, NA_asModality = FALSE, output = NULL){

  # PARAM TEST PART ------------------------------------------------------- ####

  if(!is.data.frame(data)){
    stop("data must be a data.frame.")
  }
  if(!is.vector(variable) | !is.character(variable) | length(variable) != 1){
    stop("variable must be a character vector of length one.")
  }
  if(!variable %in% colnames(data)){
    stop("variable must be the name of a column in data.")
  }
  if(!is.factor(data[, variable])){
    stop("variable must be a factor variable.")
  }
  if(!is.null(group)){
    if(!is.vector(group) | !is.character(group) | length(group) != 1){
      stop("group must be a character vector of length one.")
    }
    if(!group %in% colnames(data)){
      stop("group must be the name of a column in data.")
    }
    if(!is.factor(data[, group])){
      stop("group must be a factor variable.")
    }
  }
  if(!is.null(group_str)){
    if(!is.vector(group_str) | !is.numeric(group_str) | !length(group_str) >= 1){
      stop("group_str must be a character vector of length one.")
    }
    if(any(is.na(group_str))){
      stop("Use NA_group_AsModality to keep NA modality in stats.")
    }
    if(!all(group_str %in% which(!is.na(levels(data[, group]))))) {
      stop(paste0("group_str must be in ", paste0(1:length(levels(data[, group])), " (", levels(data[, group]), ")", collapse = ", ")))
    }
  }
  if(!is.vector(all) | !is.logical(all) | length(all) != 1){
    stop("all must be a boolean vector of length one.")
  }
  if(!is.vector(round) | !is.numeric(round) | !length(round) >= 1){
    stop("round must be a numeric vector of length one.")
  }
  if(!is.vector(p_value) | !is.logical(p_value) | length(p_value) != 1){
    stop("p_value must be a boolean vector of length one.")
  }
  if(!is.null(forcedTest)){
    if(!is.vector(forcedTest) | !is.character(forcedTest) | length(forcedTest) != 1){
      stop("p_value must be a boolean vector of length one.")
    }
    if(!forcedTest %in% c("Pearson's Chi-squared test", "Pearson's Chi-squared test with Yates", "Fisher's Exact Test for Count Data")){
      stop("forcedTest must be in \"Pearson's Chi-squared test\", \"Pearson's Chi-squared test with Yates\" or \"Fisher's Exact Test for Count Data\"")
    }
  }
  if(!is.vector(NA_group_AsModality) | !is.logical(NA_group_AsModality) | length(NA_group_AsModality) != 1){
    stop("NA_group_AsModality must be a boolean vector of length one.")
  }
  if(!is.vector(NA_asModality) | !is.logical(NA_asModality) | length(NA_asModality) != 1){
    stop("NA_asModality must be a boolean vector of length one.")
  }

  # DATA SELECTION PART --------------------------------------------------- ####

  internatOutput <- NULL

  ## keep only data that match with group_str
  if(!is.null(group_str)){
    data <- subset_withAttributes(
      data = data,
      subset = data[, group] %in% c(levels(data[, group])[group_str], NA)
    )
  }

  ## set NA as m.d. in variable and use it as factor level
  if(NA_asModality){
    data[, variable] <- addNA(data[, variable])
    levels(data[, variable])[is.na(levels(data[, variable]))] <- getMissingData_text(getOption('lang.value')) #"m.d."
  }

  ## set NA as m.d. in group and use it as factor level
  if(NA_group_AsModality){
    data[, group] <- addNA(data[, group])
    levels(data[, group])[is.na(levels(data[, group]))] <- getMissingData_text(getOption('lang.value')) #"m.d."
  }

  ## remove unuse modality in group variable
  # if(!is.null(group)){
  #   attr <- attributes(data[, group])
  #   data[, group] <- as.factor(as.character(data[, group]))
  #   attr(data[, group], 'var_label') <- attr$var_label
  # }

  # DESCRIPTION PART ------------------------------------------------------ ####

  ## if group is null or all = TRUE : QL description for all population
  if(is.null(group) | all){
    internatOutput <- statQL(data = data,
                             variable = variable,
                             round = round)
    colnames(internatOutput) <- c('Variable', 'Modality', 'Description', 'All')
  }

  if(!is.null(group)){
    ## if group is not null : QL description in each sub group
    dataSubGroup <- getSubGroupFromDataFrame(df = data, variable = variable, group = group,
                                             NA_group_AsModality = NA_group_AsModality)

    for(i in 1:length(dataSubGroup)){
      currentResult <- statQL(data = dataSubGroup[[i]],
                              variable = variable,
                              round = round)
      colnames(currentResult) <- c('Variable', 'Modality', 'Description', levels(data[, group])[i])
      if(is.null(internatOutput)){
        internatOutput <- currentResult
      } else {
        internatOutput <- cbind(internatOutput, currentResult[4])
      }
    }
  }

  # COMPARAISON PART ------------------------------------------------------ ####

  if(!is.null(group) & p_value){
    if(is.null(forcedTest)){
      test_to_use <- getTest_factorComparaison(data = data,
                                               variable = variable,
                                               group = group)
    } else {
      test_to_use <- forcedTest
    }

    ## set right test depend test_to_use value
    switch(test_to_use,
           ## Pearson's Chi-squared test
           `Pearson's Chi-squared test` = {
             test <- chisq.test(x = data[, variable], y = data[, group], correct = FALSE)
             internatOutput <- cbind(internatOutput, data.frame(test = c(getChiSquared_text(getOption('lang.value')), rep(NA, nrow(internatOutput)-1)),
                                                                p_value = c(setFormatToPvalue(test$p.value, round),
                                                                            rep(NA, nrow(internatOutput)-1))))
             colnames(internatOutput)[(ncol(internatOutput)-1):(ncol(internatOutput))] <- c("Test", "p-value")
           },

           ## Pearson's Chi-squared test with Yates' continuity correction
           `Pearson's Chi-squared test with Yates' continuity correction` = {
             test <- suppressWarnings(chisq.test(x = data[, variable], y = data[, group], correct = TRUE))
             internatOutput <- cbind(internatOutput, data.frame(test = c(getChiSquaredYates_text(getOption('lang.value')), rep(NA, nrow(internatOutput)-1)),
                                                                p_value = c(setFormatToPvalue(test$p.value, round),
                                                                            rep(NA, nrow(internatOutput)-1))))
             colnames(internatOutput)[(ncol(internatOutput)-1):(ncol(internatOutput))] <- c("Test", "p-value")
           },

           ## Fisher's Exact Test for Count Data
           `Fisher's Exact Test for Count Data` = {
             test <- tryCatch({
               fisher.test(x = data[, variable], y = data[, group], simulate.p.value = FALSE)
             }, error = function(e) {
               fisher.test(x = data[, variable], y = data[, group], simulate.p.value = TRUE)
             })
             internatOutput <- cbind(internatOutput, data.frame(test = c(getFisher_text(getOption('lang.value')), rep(NA, nrow(internatOutput)-1)),
                                                                p_value = c(setFormatToPvalue(test$p.value, round),
                                                                            rep(NA, nrow(internatOutput)-1))))
             colnames(internatOutput)[(ncol(internatOutput)-1):(ncol(internatOutput))] <- c("Test", "p-value")
           },

           ## No test
           {
             internatOutput <- cbind(internatOutput, data.frame(test = c(test_to_use, rep(NA, nrow(internatOutput)-1)),
                                                                p_value = c(rep(NA, nrow(internatOutput)))))
             colnames(internatOutput)[(ncol(internatOutput)-1):(ncol(internatOutput))] <- c("Test", "p-value")
           })
  }

  # ATTRIBUTES MANAGEMENT ------------------------------------------------- ####

  if (!is.null(group)) {
    attr(internatOutput, 'var_group') <- group
    attr(internatOutput, 'label_var_group') <- getVarLabel(data = data,
                                                           variable = group)
    if (!is.null(group_str)) {
      if(NA_group_AsModality){
        group_str <- c(group_str, NA)
      }
      attr(internatOutput, 'modality_var_group') <- group_str
      attr(internatOutput, 'label_modality_var_group') <- levels(data[,group])
    }
  }

  output <- rbind(output, internatOutput)

  return(output)
}


#' @title A convenient method that merge two data.frame of paired data
#' @description A convenient method than merge two data.frame of paired data and return a data.frame
#' in the good format to be analyzed
#' @param dataA a data.frame, containing some data to analyse
#' @param dataB a data.frame, containing some data to analyse
#' @param byA a character vector of length one, containing the name of the column in dataA used in the merge
#' @param byB a character vector of length one, containing the name of the column in dataB used in the merge
#' @param nameA a character vector of length one, containing the name of the group of dataA
#' @param nameB a character vector of length one, containing the name of the group of dataB
#' @return a data.frame containing the paired data to analyze
#' @export
mergePairedDataFrame <- function(dataA, dataB, byA, byB, nameA, nameB){

  if(!is.data.frame(dataA) | !is.data.frame(dataB)){
    stop("dataA and dataB must be a data.frame")
  }
  if(!is.vector(byA) | !is.character(byA) | length(byA) != 1){
    stop("byA must be a character vector of length 1")
  }
  if(!is.vector(byB) | !is.character(byB) | length(byB) != 1){
    stop("byB must be a character vector of length 1")
  }
  if(!is.vector(nameA) | !is.character(nameA) | length(nameA) != 1){
    stop("nameA must be a character vector of length 1")
  }
  if(!is.vector(nameB) | !is.character(nameB) | length(nameB) != 1){
    stop("nameB must be a character vector of length 1")
  }
  if(!byA %in% colnames(dataA)){
    stop("byA must be a column name of dataA")
  }
  if(!byB %in% colnames(dataB)){
    stop("byB must be a column name of dataB")
  }

  ## add prefix to each column of dataA
  ## to avoid exact match colname betwween data.frame
  prefixA <- "A_"
  colnamesA <- colnames(dataA)
  loc_byA <- which(colnamesA == byA)
  colnamesA <- paste0(prefixA, colnamesA)
  colnamesA[loc_byA] <- byA
  colnames(dataA) <- colnamesA
  dataA$groupPaired_ <- nameA

  ## add prefix to each column of dataB
  ## to avoid exact match colname betwween data.frame
  prefixB <- "B_"
  colnamesB <- colnames(dataB)
  loc_byB <- which(colnamesB == byB)
  colnamesB <- paste0(prefixB, colnamesB)
  colnamesB[loc_byA] <- byB
  colnames(dataB) <- colnamesB
  dataB$groupPaired_ <- nameB

  ## merge dataA and dataB as mergeData by groupPaired_ and byA/byB parameter
  mergeData <- merge(x = dataA,
                     y = dataB,
                     by.x = c(byA, "groupPaired_"),
                     by.y = c(byB, "groupPaired_"),
                     all = TRUE)

  ## get colnames of merge data
  colnamesMergeData <- colnames(mergeData)

  ## initialisation of vector containing treated columns
  treatedColumn <- NULL

  ## for each colnames of merge data
  lapply(colnamesMergeData, function(currentColname){

    ## get the origin name of current colname
    currentColnameOrigin <- gsub(pattern = paste0("^(", prefixA, "|", prefixB, ")"),
                                 replacement = "",
                                 x = currentColname)

    ## if currentColnameOrigin already is in treatedColumn
    if(currentColnameOrigin %in% treatedColumn){
      ## get the position of column identical to currentColnameOrigin
      currentSameColumnPos <- which(gsub(pattern = paste0("^(", prefixA, "|", prefixB, ")"),
                                         replacement = "",
                                         x = colnamesMergeData) == currentColnameOrigin)
      ## already treated column
      columnA <- mergeData[currentSameColumnPos[1]]
      ## current column
      columnB <- mergeData[currentSameColumnPos[2]]
      ## merge already treated column and current column in already treated column
      mergeData[is.na(columnA), currentSameColumnPos[1]] <<- columnB[is.na(columnA)]
      ## set current colnames as NA
      mergeData[, currentSameColumnPos[2]] <<- NA
    }
    ## if currentColnameOrigin not already in treatedColumn
    else {
      ## add current colnames into treatedColumn vector
      treatedColumn <<- c(treatedColumn, currentColnameOrigin)
    }
    return(NULL)
  })

  ## manage the mergeData colnames
  colnamesMergeData <- gsub(pattern = paste0("^(", prefixA, "|", prefixB, ")"),
                            replacement = "",
                            x = colnamesMergeData)
  colnames(mergeData) <- colnamesMergeData

  ## remove full NA columns
  mergeData <- mergeData[,colSums(is.na(mergeData)) < nrow(mergeData)]
  ## keep only row corresponding as paired data
  mergeData <- mergeData[mergeData[, byA] %in% names(which(table(mergeData[, byA]) == 2)), ]
  ## order df by groupPaired_ and byA
  mergeData <- mergeData[order(mergeData$groupPaired_, mergeData[, byA]), ]
  ## set groupPaired_ as.factor()
  mergeData$groupPaired_ <- as.factor(mergeData$groupPaired_)
  return(mergeData)
}


#' @title A convenient method to describe a full data.frame.
#' @description  A convenient method to describe a full data.frame.
#' @param data a data.frame containing the data to describe
#' @param variables a character vector contening the name of columns to describe. Default to colnames(data).
#' @param applicable a list of boolean vector generating based on manageNotApplicable(). Use in cas of NonApplicable data
#' @param group a character vector of length 1. The name of the factor column to use as commparaison group. Default to NULL.
#' @param group_str a numeric vector. The index of the levels of the group variable to use. Default to NULL.
#' @param p_value a boolean. If TRUE, comparaison test are performed.
#' @param all a boolean. If TRUE, total column will be displayed. Default to FALSE
#' @param desc a character vector. Could contain "Mean", "Median", "Range" and/or "Mode"
#' @param round an integer, number of maximal decimal. Default to 3
#' @param confint a boolean. If TRUE, the confidence interval of the mean will be displayed. Default to FALSE
#' @param NA_asModality a boolean. If TRUE, missing data of the factor variable to describe will be considered as levels.
#' Default to FALSE
#' @param NA_group_AsModality a boolean. If TRUE, missing data of the group variable will be considered as levels. Default to FALSE
#' @return a data.frame containing the description of the variables
#' @export
#' @examples
#' data(mtcars)
#' labels <- data.frame(Variable = c("vs", "vs", "am", "am"),
#'                      Modality = c(0, 1, 0, 1),
#'                      Label = c("V-shaped", "Straight", "Automatic", "Manual"))
#' labelVariable <- data.frame(Variable = c("mpg", "cyl", "disp", "hp", "drat", "wt",
#'                                          "qsec", "vs", "am", "gear", "carb"),
#'                             Label = c("Miles/(US) gallon", "Number of cylinders", "Displacement (cu.in.)",
#'                                       "Gross horsepower ", "Rear axle ratio", "Weight (1000 lbs)",
#'                                       "1/4 mile time", "Engine", "Transmission", "Number of forward gears",
#'                                       "Number of carburetors"))
#' labelledData <- statsBordeaux::labellisationDataFrame(mtcars, labels)
#' labelledData <- statsBordeaux::setLabelToVariable(labelledData, labelVariable)
#' comparaison <- describeDataFrame(mtcars, group = "vs", p_value = TRUE)
describeDataFrame <- function(data, variables = colnames(data), applicable = NULL, group = NULL, group_str = NULL,
                              p_value = FALSE, all = FALSE, desc = c("Mean", "Median", "Range"),
                              round = 3, confint = FALSE, NA_asModality = FALSE, NA_group_AsModality = FALSE){

  if(!is.data.frame(data) | nrow(data) == 0 | ncol(data) == 0){
    stop("data must be a data.frame containing some data.")
  }
  if(!is.vector(variables) | !is.character(variables) | !length(variables) > 0){
    stop("variables must be a character vector.")
  }
  if(!all(variables %in% colnames(data))){
    diffVar <- setdiff(variables, colnames(data))
    if(length(diffVar) == 1){
      stop(paste0("Variable '", paste0(diffVar, collapse = "', "), "' is not present in data."))
    } else {
      stop(paste0("Variables '", paste0(diffVar, collapse = "', "), "' are not present in data."))
    }
  }
  if(!is.null(group)){
    if(!is.vector(group) | !is.character(group) | length(group) != 1){
      stop("group must be a character vector of length one.")
    }
    if(!group %in% colnames(data)){
      stop("group must be a colname of data.")
    }
  }
  if(!is.null(group_str)){
    if(!is.vector(group_str) | !is.numeric(group_str) | length(group_str) < 1) {
      stop("group_str must a numeric vector of length over or equal to one.")
    }
    if(any(!(group_str %in% which(!is.na(levels(data[, group])))))){
      modalitiesList <- paste0(c(1:length(levels(data[, group]))), " ('", levels(data[, group]), "')", collapse = ", ")
      stop(paste0("group_str must be in ", modalitiesList, "."))
    }
  }
  if(!is.vector(p_value) | !is.logical(p_value) | length(p_value) != 1){
    stop("p_value must be a logical vector of length one.")
  }
  if(!is.vector(all) | !is.logical(all) | length(all) != 1){
    stop("all must be a logical vector of length one.")
  }
  if(!is.vector(desc) | !is.character(desc) | length(desc) < 1){
    stop("desc must be a character vector of length over or equal to one.")
  }
  if(!all(desc %in% c("Mean", "Median", "Range", "Mode"))){
    stop("desc must be in 'Mean', 'Median', 'Range', 'Mode'")
  }
  if(!is.vector(round) | !is.numeric(round) | length(round) != 1 | !round >=0){
    stop("round must be a numeric vector of length one with value over or equal to 0.")
  }
  if(!is.vector(confint) | !is.logical(confint) | length(confint) != 1){
    stop("confint must be a logical vector of length one.")
  }
  if(!is.vector(NA_asModality) | !is.logical(NA_asModality) | length(NA_asModality) != 1){
    stop("NA_asModality must be a logical vector of length one.")
  }
  if(!is.vector(NA_group_AsModality) | !is.logical(NA_group_AsModality) | length(NA_group_AsModality) != 1){
    stop("NA_group_AsModality must be a logical vector of length one.")
  }


  if(!is.null(group)){
    variables <- setdiff(variables, group)
  }

  result <- do.call("rbind", jumpDescribeDataFrame(data = data,
                                                   variable = variables,
                                                   applicable = applicable,
                                                   group = group,
                                                   group_str = group_str,
                                                   p_value = p_value,
                                                   all = all,
                                                   desc = desc,
                                                   round = round,
                                                   confint = confint,
                                                   NA_asModality = NA_asModality,
                                                   NA_group_AsModality = NA_group_AsModality))

  return(result)
}
