#' @title Check if data are only digits
#' @description Check if the input data.frale are only composed of digits
#' @param df the data.frame to check
#' @param returnError boolean. If TRUE, return a data.frame with the coordinate of the cell
#' containing some non digit data in excel style.
#' @return TRUE if the data.frame only contain digit, else return a data.frame
#' with the coordinate of the cell containing some wrond format data in excel style. FALSE by default.
#' @export
#' @examples
#' data(mtcars)
#' checkNotDigitInDataframe(df = mtcars, returnError = FALSE)
#'
#' mtcars$NOT_DIGIT <- rep("A", nrow(mtcars))
#' errorPosition <- checkNotDigitInDataframe(df = mtcars, returnError = TRUE)
checkNotDigitInDataframe <- function(df, returnError = FALSE){

  if(class(df) != "data.frame"){
    stop("df must be a data.frame")
  }

  df <- data.frame(lapply(df, function(x){gsub("^$|^ $", NA, x)}))
  df <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE)

  notNum <- sapply(df, function(x){
    !grepl("^-*[0-9]+(\\.[0-9]+)?$", x) & !is.na(x)
  })

  result <- data.frame(Lignes = integer(),
                       Colonnes = character(),
                       Contenu = character(),
                       stringsAsFactors = FALSE)

  # compute the non digit localisation
  if(any(notNum)){
    k = 1
    for(i in 1:ncol(notNum)){
      for(j in 1:nrow(notNum)){
        if(notNum[j,i] == TRUE){
          result[k,1] <- j + 1
          result[k,3] <- df[j,i]
          if(!identical(letters[i%%26], character(0))){
            result[k,2] <- toupper(paste0(letters[round(i/26, 1)], letters[i%%26]))
          } else {
            result[k,2] <- toupper(paste0(letters[round(i/26, 1)]))
          }
          k = k+1
        }
      }
    }
  }

  # non digit found
  if(any(notNum)){
    if(returnError){
      return(result)
    } else {
      return(FALSE)
    }
  }
  # only digit found
  else {
    if(returnError){
      return(result)
    } else {
      return(TRUE)
    }
  }
}




#' @title set label to qualitative variable into a data.frame based on dictionnary.
#' @description manage the conversion and sat label to qualitative into a data.frame based
#' on dictionnary.
#' @param df a data.frame containing qualitative variable witch need to be labelised.
#' @param labelTable a data.frame containing three columns : 1) the variable names, 2) one
#' modality as digit, 3) the label of the modality.
#' @return a data.frame with qualitative variable in the labelTable convert as factor with clean label
#' @export
#' @examples
#' df <- data.frame(X1 = c(0, 1, 2, 1, 2, 1, 2),
#' X2 = c(0, 1, 0, 1, 1, 0, 0))
#' labelTable <- data.frame(VARIABLE = c("X2", "X2"),
#'                          MODALITY = c(0, 1),
#'                          LABEL = c("Male", "Female"))
#' df <- labellisationDataFrame(df, labelTable)
labellisationDataFrame <- function(df, labelTable){

  if(class(df) != "data.frame"){
    stop("df must be a data.frame")
  }

  if(class(labelTable) != "data.frame"){
    stop("labelTable must be a data.frame")
  }

  listeVariableQL <- levels(labelTable[, 1])
  if(any(!listeVariableQL %in% colnames(df))){
    differences <- listeVariableQL[which(!listeVariableQL %in% colnames(df))]
    number <- length(differences)
    differences <- paste0("'", paste0(differences, collapse = "', '"), "'")
    if(number == 1){
      stop(paste0("Variable ", differences, " is in labelTable and not in df"))
    } else {
      stop(paste0("Variables ", differences, " are in labelTable and not in df"))
    }
  }

  # get colnames to reorder at the end the result
  colnamesData <- colnames(df)

  # all variable in the labelTable data.frame are converted as factor
  for(i in 1:length(df)){
    if(colnames(df)[i] %in% listeVariableQL) {
      df[, colnames(df)[i]] <- as.factor(df[, colnames(df)[i]])
    }
  }

  tabNotQL <- df[!sapply(df, is.factor)]
  tabQL <- df[sapply(df, is.factor)]

  # for each modality in the labelTable, revalue data in df
  for (i in 1:nrow(labelTable)) {
    variable <- as.character(labelTable[i, 1])
    codeModalite <- as.character(labelTable[i, 2])
    libelleModalite <- as.character(labelTable[i, 3])
    names(libelleModalite) <- codeModalite
    tabQL[, variable] <- plyr::revalue(tabQL[, variable], libelleModalite)
  }

  # convert character variable composed only by digit as numeric
  tabNotQL[] <- lapply(tabNotQL, function(x){
    if(is.character(x) | class(x) == "difftime"){
      isDigit <- grepl("^-*\\d+(\\.\\d+)*$", x)
      isNa <- is.na(x)
      if(!any(!as.logical(isDigit + isNa))){
        x <- as.numeric(x)
      }
    }
    return(x)
  })

  df <- cbind(tabNotQL, tabQL)
  df <- df[, colnamesData]
  return(df)

}



#' @title set label to each column of data.frame based on dictionnaty
#' @description add a label as attribute (var_label) to each column of data.frame based on dictionnary. This
#' label will be used during the reporting function.
#' @param df the data.frame witch nedd to be labelised
#' @param label a data.frame with two columns : 1) the variable colname, 2) the clean label to display
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
setLabelToVariable <- function(df, label){
  if(!is.data.frame(df)){
    stop("df must be a data.frame")
  }

  if(!is.data.frame(label) | ncol(label) !=2){
    stop("label must be a data.frame of 2 column")
  }

  listLabel <- as.character(label[, 1])
  if(any(!listLabel %in% colnames(df))){
    differences <- listLabel[which(!listLabel %in% colnames(df))]
    number <- length(differences)
    differences <- paste0("'", paste0(differences, collapse = "', '"), "'")
    if(number == 1){
      stop(paste0("Variable ", differences, " is in label data.frame and not in df"))
    } else {
      stop(paste0("Variables ", differences, " are in label data.frame and not in df"))
    }
  }

  for(i in 1:nrow(label)){
    indexVarInData <- which(colnames(df) == label[i, 1])
    if(length(indexVarInData) != 0) {
      attr(df[, indexVarInData], "var_label") <- label[i, 2]
    }
  }
  return(df)
}




#' @title get the label of variable in a data.frame
#' @description Get the label of variables witch were labelised thanks the setLabel function
#' @param df a data.frame witch contain some variable witch were labelized
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
#' getLabelFromVariable(labeled_df)
getLabelFromVariable <- function(df){
  if(!is.data.frame(df)){
    warning("df must be a data.frame.")
  }

  labelResult <- NULL
  for (i in 1:ncol(df)) {
    currentData <- df[i]
    label <- attributes(currentData[, 1])$`var_label`
    if (is.null(label)) {
      label <- colnames(currentData)
    }
    labelResult <- c(labelResult, label)
  }
  return(labelResult)
}

#' @title Subset data keeping attributes
#' @description  Allows to subset data and keeping all the attributes of the data
#' @param x a data.frame
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
subset_withAttributes <- function(x, subset){
  subset_df <- subset(x, subset = subset)
  for(i in 1:ncol(x)){
    attr <- attributes(x[, i])
    attributes(subset_df[, i]) <- attr
  }
  return(subset_df)
}

#' @title Manage the not applicable condition into data.frame
#' @description Identify the not applicable char in data.frame and return clean list with :
#' 1) data.frame without not applicable character, 2) list of logical vector identifying the row witch should contain data
#' @param df a data.frame containing data to clean
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
manageNotApplicable <- function(df, notApplicableChar){

  if(!is.data.frame(df)){
    stop("df must be a data.frame")
  }

  if(!is.vector(notApplicableChar) | !is.character(notApplicableChar) | length(notApplicableChar) !=1 ){
    stop("notApplicableChar must be a character vector of length 1")
  }

  resultNotApplicable <- lapply(df, function(x){
    result <- x == notApplicableChar
    result[is.na(result)] <- FALSE
    result <- !result
    return(result)
  })
  df[] <- lapply(df, function(x){
    if(is.factor(x) | is.character(x)){
      x <- as.character(x)
      x[x == notApplicableChar] <- NA
      return(x)
    } else {
      return(x)
    }
  })
  return(list(df, resultNotApplicable))
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
#' @param output a data.frame, containing a previously generated data.frame or a new data.frame.
#' data.frame are generated using createOutput() function.
#' @param input a data.frame containing the data to describe
#' @param variable a character vector of length 1. The name of the numeric column to describe.
#' @param group a character vector of length 1. The name of the factor column to use as commparaison group. Default to NULL.
#' @param group_str a character vector. The name of the levels of the group variable to use. Default to NULL.
#' @param p_value a boolean. If TRUE, comparaison test are performed.
#' @param forcedTest a character vector of length 1. Must be one of "Student", "Welch", "Wilcoxon", "ANOVA", or
#' "Kruskal-Wallis". Default to NULL.
#' @param all a boolean. If TRUE, total column will be displayed. Default to FALSE
#' @param desc a character vector. Could contain "Mean", "Median", "Range" and/or "Mode"
#' @param round an integer, number of maximal decimal. Default to 3
#' @param confint a boolean. If TRUE, the confidence interval of the mean will be displayed. Default to FALSE
#' @param NA_group_AsModality a boolean. If TRUE, missing data of the group variable will be considered as levels.
#' Default to FALSE
#' @return a data.frame containing the description of the variable
#' @export
#' @examples
#' data(mtcars)
#' mtcars$am <- as.factor(mtcars$am)
#' output <- createOutput()
#' output <- statsQT(output = output,
#'                   input = mtcars,
#'                   variable = "mpg",
#'                   group = "am")
#' output <- statsQT(output = output,
#'                   input = mtcars,
#'                   variable = "disp",
#'                   group = "am")
statsQT <- function(output, input, variable, group = NULL, group_str = NULL, p_value = FALSE, forcedTest = NULL,
                    all = FALSE, desc = c("Mean", "Median", "Range"), round = 3, confint = FALSE,
                    NA_group_AsModality = FALSE) {

  if(!is.data.frame(output)){
    stop("output must be a data.frame")
  }

  if(!is.data.frame(input)){
    stop("input must be a data.frame")
  }

  if(!is.vector(variable) | !is.character(variable) | length(variable) !=1){
    stop("variable must a character vector of length 1")
  }

  if(!variable %in% colnames(input)){
    stop("variable must be the name of one column in input")
  }

  if(!is.numeric(input[, variable])){
    stop("variable must be the name of a numeric variable")
  }

  if(!is.vector(p_value) | !is.logical(p_value) | length(p_value) !=1){
    stop("p_value must be the logical vector of length 1")
  }

  if(!is.vector(all) | !is.logical(all) | length(all) !=1){
    stop("all must be the logical vector of length 1")
  }

  if(!is.vector(confint) | !is.logical(confint) | length(confint) !=1){
    stop("confint must be the logical vector of length 1")
  }

  if(!is.vector(NA_group_AsModality) | !is.logical(NA_group_AsModality) | length(NA_group_AsModality) !=1){
    stop("NA_group_AsModality must be the logical vector of length 1")
  }

  if(!is.null(group)){
    if(!is.vector(group) | !is.character(group) | length(group) !=1){
      stop("group must a character vector of length 1")
    }
    if(!group %in% colnames(input)){
      stop("group must be the name of one column in input")
    }
    if(!is.factor(input[, group])){
      stop("group must be the name of a factor variable")
    }
  }

  if(!is.null(group_str)){
    if(!is.vector(group_str) | !is.numeric(group_str)){
      stop("group_str must a numeric vector")
    }
    # On vérifie que les modalités de group_str sont bien dans GROUP
    if(any(!(group_str %in% which(!is.na(levels(input[, group])))))){
      modalitiesList <- paste0(c(1:length(levels(input[, group]))), ' - "', levels(input[, group]), '", ',
                               collapse = '')
      stop(paste0("group_str must be in ", substr(modalitiesList, 1, nchar(modalitiesList)-2), "."))
    }
  }

  if(!is.vector(round) | !is.numeric(round) | length(variable) !=1){
    stop("round must a numeric vector of length 1")
  }

  if(!is.vector(confint) | !is.logical(confint) | length(confint) !=1){
    stop("confint must a logical vector of length 1")
  }

  # Si un autre terme que Mean, Median, Range et Mode ont été saisis dans l'argument desc, on génère un stop
  if (length(setdiff(desc, c("Mean", "Median", "Range", "Mode"))) != 0) {
    stop(paste0("desc must be a characher vector composer with 'Mean', 'Median', 'Range' or 'Mode'"))
  }

  # On s'assure que la variable QL de dichotomisation est identique à celle utilisée précédement
  if(!is.null(attributes(output)$var_group)) {
    if(attributes(output)$var_group == group){
      variableQLidentical = TRUE
      if(is.null(group_str)) {
        if(is.null(attributes(output)$label_modality_var_group)){
          modaliteVariableQLidentical = TRUE
        } else {
          modaliteVariableQLidentical = FALSE
        }
      } else if(identical(attributes(output)$label_modality_var_group,
                          levels(input[, group])[group_str])) {
        modaliteVariableQLidentical = TRUE
      } else {
        modaliteVariableQLidentical = FALSE
      }
    } else {
      variableQLidentical = FALSE
    }
  } else {
    variableQLidentical = TRUE
    modaliteVariableQLidentical = TRUE
  }

  if(!variableQLidentical) {
    stop(paste0(group, " must be the same as thoose used previously (", attributes(output)$var_group, ").
Generate a new output with the function createOutput() to describe data with an other group."))
  }

  if(!modaliteVariableQLidentical){
    if(is.null(group_str)){
      modalitesGroup <- "All"
    } else {
      modalitesGroup <- paste0(levels(input[, group])[group_str], collapse = ', ')
    }

    if(is.null(attributes(output)$label_modality_var_group)){
      modalitesOutput <- "All"
    } else {
      modalitesOutput <- paste0(attributes(output)$label_modality_var_group, collapse = ', ')
    }

    stop(paste0("group_str (", modalitesGroup, ") must be the same as thoose used previously (", modalitesOutput, ").
Generate a new output with the function createOutput() to describe data with an other group_str."
    )
    )
  }


  #################################


  # Si GROUP = NULL, pas de sous-groupe, statistique descriptive de l'ensemble de VARIABLE
  if(is.null(group)) {
    result <- statQT(input = input,
                     variable = variable,
                     desc = desc,
                     round = round,
                     confint = confint)
    colnames(result) <- c('Variable', 'Modality', 'Description', 'All')
  }

  # Sous-groupe en fonction de la variable GROUP, statistique descriptive par sous groupe
  else {

    # Description de VARIABLE selon certaines modalités de GROUP
    if(!is.null(group_str)){
      # On récupère les indexs de lignes des sous-groupes qui nous interessent
      vectSubData <- input[, group] %in% levels(input[, group])[group_str]
      # Cas particulier des données manquantes qui sont gardées si NA_group_AsModality = TRUE
      if(NA_group_AsModality){
        vectSubData[is.na(vectSubData)] <- TRUE
      } else {
        vectSubData[is.na(vectSubData)] <- FALSE
      }

      # On créé notre sub.data.frame basé sur les indexes de lignes créés précédemment
      inputSubData <- input[vectSubData,]
      inputSubData[group] <- as.factor(as.character(inputSubData[,group]))

      # On génère nos n data.frame pour chaque sous-groupes qui sont précisés dans group_str
      subGroupDataList <- getSubGroupFromDataFrame(df = inputSubData,
                                                   variable = variable,
                                                   group = group,
                                                   NA_group_AsModality = NA_group_AsModality)

      # Pour chaque sous-groupe de la lite de n data.frame, on réalise les statistique descriptives atomique
      for (i in 1:length(subGroupDataList)) {
        # Initialisation du tableau de sortie result pour le premier sub.data.frame
        # Réalisation des statistiques descriptives par sous-groupe (initialisation)
        if (i == 1) {
          currentResult <- statQT(input = subGroupDataList[[i]],
                                  variable = variable,
                                  round = round,
                                  confint = confint,
                                  desc = desc)
          colnames(currentResult) <- c('Variable',
                                       'Modality',
                                       'Description',
                                       levels(inputSubData[, group])[i])
          result <- currentResult
        }

        # Réalisation des statistiques descriptives par sous-groupe (hors initialisation)
        else {
          currentResult <- statQT(input = subGroupDataList[[i]],
                                  variable = variable,
                                  round = round,
                                  confint = confint,
                                  desc = desc)

          # On nomme les modalités (NA = 'm.d.')
          if(!is.na(levels(inputSubData[, group])[i])){
            colnames(currentResult)[ncol(currentResult)] <- levels(inputSubData[, group])[i]
          } else {
            colnames(currentResult)[ncol(currentResult)] <- 'm.d.'
          }

          # On fusionne la description du sub.data.frame actuel (currentResult)
          # avec les descriptions précédentes (result)
          result <- cbind(result, currentResult[ncol(currentResult)])
        }
      }
    }

    # Si group_str == NULL, description VARIABLE selon l'ensemble des modalités de GROUP
    else {
      # On génère nos n data.frame pour chaque modalité de group
      subGroupDataList <- getSubGroupFromDataFrame(df = input,
                                                   variable = variable,
                                                   group = group,
                                                   NA_group_AsModality = NA_group_AsModality)

      # Pour chaque sous-groupe de la lite de n data.frame, on réalise les statistique descriptives atomique
      for (i in 1:length(subGroupDataList)) {
        # Initialisation du tableau de sortie result pour le premier sub.data.frame
        # Réalisation des statistiques descriptives par sous-groupe (initialisation)
        if (i == 1) {
          currentResult <- statQT(input = subGroupDataList[[i]],
                                  variable = variable,
                                  round = round,
                                  confint = confint,
                                  desc = desc)
          colnames(currentResult) <- c('Variable',
                                       'Modality',
                                       'Description',
                                       levels(input[, group])[i])
          result <- currentResult
        }
        # Réalisation des statistiques descriptives par sous-groupe (hors initialisation)
        else {
          currentResult <- statQT(input = subGroupDataList[[i]],
                                  variable = variable,
                                  round = round,
                                  confint = confint,
                                  desc = desc)

          # On nomme les modalités (NA = 'm.d.')
          if(!is.na(levels(input[, group])[i])){
            colnames(currentResult)[ncol(currentResult)] <- levels(input[, group])[i]
          } else {
            colnames(currentResult)[ncol(currentResult)] <- 'm.d.'
          }

          # On fusionne la description du sub.data.frame actuel (currentResult)
          # avec les descriptions précédentes (result)
          result <- cbind(result, currentResult[ncol(currentResult)])
        }
      }
    }

    ####################################
    # Colonne totale
    ####################################

    # Cas où l'on affiche les statistiques descriptives que de certains sous groupe, la colonne totale
    # correspond aux sous-modalités affichées
    if(!is.null(group_str)){
      currentResult <- statQT(input = inputSubData,
                              variable = variable,
                              round = round,
                              confint = confint,
                              desc = desc)
    }
    # Cas où l'on affiche les statistiques descriptives selon tous les sous groupe, la colonne totale
    # correspond à l'ensemble des sous-modalités
    else {
      currentResult <- statQT(input = input,
                              variable = variable,
                              round = round,
                              confint = confint,
                              desc = desc)
    }

    colnames(currentResult) <- c('Variable', 'Modality', 'Description', 'All')
    # Si ALL = FALSE, on supprime les statistiques descriptives mais on display quand même la colonne
    # pour laisser la possibilité de décrire le total pour un autre variable
    if (all != TRUE) {
      currentResult$Variable <- NA
      currentResult$Modality <- NA
      currentResult$Description <- NA
      currentResult$All <- NA
    }
    result <- cbind(result, currentResult[ncol(currentResult)])


    ####################################
    # Statistique de test
    ####################################

    # Réalisation d'une statistique de test si P_VALUE = TRUE
    if (p_value == TRUE) {
      # On génère les n data.frame de VARIABLE en fonction des modalitées affichées de GROUP
      if(!is.null(group_str)){
        subGroupDataList <- getSubGroupFromDataFrame(df = inputSubData,
                                                     variable = variable,
                                                     group = group)
      }
      # On génère les n data.frame de VARIABLE en fonction toutes les modalitées de GROUP
      else {
        subGroupDataList <- getSubGroupFromDataFrame(df = input,
                                                     variable = variable,
                                                     group = group)
      }


      # Si forcedTest = NULL, on laisse le programme décider seul du test à réaliser
      if(is.null(forcedTest)){

        # Cas avec deux sous-groupes
        if(length(subGroupDataList) == 2) {

          # Création du tableau de sortie
          # 1 ligne est rajoutée si confint = TRUE pour avoir le même nombre de ligne que
          # result au moment du cbind() final

          if (confint == TRUE) {
            currentResult <- createOutput(ncol = 2, nrow = nrow(result)+1)
          } else {
            currentResult <- createOutput(ncol = 2, nrow = nrow(result))
          }

          colnames(currentResult) <- c('Test', 'p-value')

          # Test de student (effectifs supérieurs à 30 dans les deux groupes)
          if (all(c(
            sum(!is.na(subGroupDataList[[1]])) >= 30),
            sum(!is.na(subGroupDataList[[2]])) >= 30)) {

            # Cas où la variance est non nulle dans les deux groupes
            if (all(c(sd(subGroupDataList[[1]][, 1], na.rm = TRUE) != 0,
                      sd(subGroupDataList[[2]][, 1], na.rm = TRUE) != 0))) {

              # On vérifie l'égalité des variances
              varTest <- var.test(subGroupDataList[[1]][, 1],
                                  subGroupDataList[[2]][, 1])

              if (varTest$p.value <= 0.05) {
                # Test de student pour variances inégales
                tTest <- t.test(subGroupDataList[[1]],
                                subGroupDataList[[2]],
                                var.equal = FALSE)
              } else {
                # Test de student pour variances égales
                tTest <- t.test(subGroupDataList[[1]],
                                subGroupDataList[[2]],
                                var.equal = TRUE)
              }

              # On stocke le résultat du test de Student
              currentResult[1, 1] <- trimws(tTest$method)
              currentResult[1, 2] <- pvalFormat(pvalue = tTest$p.value,
                                                round = round)

              # On rajoute la ligne de l'IC de la différence au niveau des résultats descriptifs
              if (confint == TRUE) {
                IC_diff <- createOutput(ncol = ncol(result), nrow = 1)
                colnames(IC_diff) <- colnames(result)
                IC_diff[1, 3] <- 'Mean difference [IC95%]'

                # On sauvegarde l'IC de la moyenne dans les colonnes 4 et 5, seront fusionnées au moment de reporting
                IC_diff[1, 4] <- paste0(
                  numberFormat(tTest$estimate['mean of x'] - tTest$estimate['mean of y'], round),
                  ' [',
                  numberFormat(tTest$conf.int[1], round),
                  ' ; ',
                  numberFormat(tTest$conf.int[2], round),
                  ']'
                )
                IC_diff[1, 5] <- paste0(
                  numberFormat(tTest$estimate['mean of x'] - tTest$estimate['mean of y'], round),
                  ' [',
                  numberFormat(tTest$conf.int[1], round),
                  ' ; ',
                  numberFormat(tTest$conf.int[2], round),
                  ']'
                )
                result <- rbind(result, IC_diff)
              }
              # On fusionne les deux colonnes de tests statistiques à la suite des résultats descriptifs
              result <- cbind(result, currentResult)
            }

            # Cas où la variance est nulle dans l'un des deux groupes
            else {
              currentResult <- createOutput(ncol = 2, nrow = nrow(result))
              colnames(currentResult) <- c('Test', 'p-value')
              currentResult[1, 1] <- 'Variability needed to perform test'
              currentResult[1, 2] <- NA
              result <- cbind(result, currentResult)
            }
          }

          # Test de wilcoxon (effectifs inférieurs à 30 dans au moins un des deux groupes
          # et supérieurs à 2 dans les deux groupes)
          else if (all(c(sum(!is.na(subGroupDataList[[1]])) >= 2,
                         sum(!is.na(subGroupDataList[[2]])) >= 2))) {
            currentResult <- createOutput(ncol = 2, nrow = nrow(result))
            colnames(currentResult) <- c('Test', 'p-value')

            # Cas où la variance est non nulle dans les deux groupes
            if (all(c(sd(subGroupDataList[[1]][, 1], na.rm = TRUE) != 0,
                      sd(subGroupDataList[[2]][, 1], na.rm = TRUE) != 0))) {

              # On réalise le test de wilcoxon et on stoque les résultats
              wilcoxTest <- suppressWarnings(
                wilcox.test(subGroupDataList[[1]][, 1],
                            subGroupDataList[[2]][, 1],
                            correct = TRUE))

              currentResult[1, 1] <- trimws(wilcoxTest$method)
              currentResult[1, 2] <- pvalFormat(pvalue = wilcoxTest$p.value,
                                                round = round)
              result <- cbind(result, currentResult)
            }

            # Cas où la variance est nulle dans l'un des deux groupes
            else {
              currentResult[1, 1] <- 'Variability needed to perform test'
              currentResult[1, 2] <- NA
              result <- cbind(result, currentResult)
            }
          }

          # Pas de test possible
          else {
            currentResult <- createOutput(ncol = 2, nrow = nrow(result))
            colnames(currentResult) <- c('Test', 'p-value')
            currentResult[1, 1] <- 'Sample size not large enough to perform test'
            currentResult[1, 2] <- NA
            result <- cbind(result, currentResult)
          }
        }

        # Cas avec plus de deux groupes
        else if (length(subGroupDataList) > 2) {

          # On convertie les n sub.data.frame en liste de vector pour le test de Bartlett
          subGroupDataListVector <- lapply(subGroupDataList, function(x) {
            unlist(x[, 1])})

          # On créé le tableau de sortie
          currentResult <- createOutput(ncol = 2, nrow = nrow(result))
          colnames(currentResult) <- c('Test', 'p-value')

          # On vérifie que tous les groupes ont des effectifs supérieurs à 30
          # Hypothèse de normalité
          if (!length(grep(FALSE, (sapply(subGroupDataList, function(x) {sum(!is.na(x))}) >= 30))) > 0) {
            #On teste l'homoscédasticité avec un test de Bartlett
            bartlettTest <- bartlett.test(subGroupDataListVector)
            hypotheseANOVA <- bartlettTest$p.value > 0.05
          }
          # Effectif inférieur à 30, l'hypothèse de nomalité de l'ANOVA n'est pas vérifiée
          else {
            hypotheseANOVA <- FALSE
          }

          # ANOVA
          if (hypotheseANOVA) {

            # Cas où l'ANOVA est réalisée uniquement pour les sous-groupes affichés
            if(!is.null(group_str)){
              anova <- aov(inputSubData[, variable] ~ inputSubData[, group])
            }
            # Cas où l'ANOVA est réalisée poure l'ensemble des sous-groupes
            else {
              anova <- aov(input[, variable] ~ input[, group])
            }

            # Rendu des résultats de l'ANOVA
            currentResult[1, 1] <- 'Analysis of variance'
            currentResult[1, 2] <- pvalFormat(pvalue = summary(anova)[[1]][1, 'Pr(>F)'],
                                              round = round)
            result <- cbind(result, currentResult)
          }
          else {
            # Kruskal-Wallis si les effectifs sont supérieurs à 2 dans chaque sous-groupe
            if (!length(grep(FALSE, (sapply(subGroupDataList, function(x) {sum(!is.na(x))}) >= 2))) > 0) {

              # Cas où le test de Kruskal-Wallis est réalisée uniquement pour les sous-groupes affichés
              if(!is.null(group_str)){
                kruskalTest <- kruskal.test(inputSubData[, variable] ~ inputSubData[, group])
              }
              # Cas où le test de Kruskal-Wallis est réalisée poure l'ensemble des sous-groupes
              else {
                kruskalTest <- kruskal.test(input[, variable] ~ input[, group])
              }

              # Rendu des résultats du test de Kruskal-Wallis
              currentResult[1, 1] <- 'Kruskal-Wallis test'
              currentResult[1, 2] <- pvalFormat(pvalue = kruskalTest$p.value,
                                                round = round)
              result <- cbind(result, currentResult)
            }
            # Effectif trop petits pour effectuer un test de Kruskal-Wallis
            else {
              currentResult[1, 1] <- 'Sample size not large enough to perform test'
              currentResult[1, 2] <- NA
              result <- cbind(result, currentResult)
            }
          }
        }
      }

      # On force le test à réaliser
      else {
        # Création du tableau de sortie
        currentResult <- createOutput(ncol = 2, nrow = nrow(result))
        colnames(currentResult) <- c('Test', 'p-value')

        # Test de Student pour variance égales
        if(forcedTest == "Student"){

          # On vérifie qu'il y a bien 2 sous-groupes à comparer
          if(length(subGroupDataList) == 2){

            # Modification du tableau de sortie si CONFINT = TRUE
            if (confint == TRUE) {
              currentResult <- createOutput(ncol = 2, nrow = (nrow(result) + 1))
              colnames(currentResult) <- c('Test', 'p-value')
            }

            tTest <- t.test(subGroupDataList[[1]],
                            subGroupDataList[[2]],
                            var.equal = TRUE)
            currentResult[1, 1] <- trimws(tTest$method)
            currentResult[1, 2] <- pvalFormat(pvalue = tTest$p.value,
                                              round = round)

            # On rajoute la ligne de l'IC de la différence
            if (confint == TRUE) {
              IC_diff <- createOutput(ncol = ncol(result), nrow = 1)
              colnames(IC_diff) <- colnames(result)
              IC_diff[1, 3] <- 'Mean difference [IC95%]'
              IC_diff[1, 4] <- paste0(
                numberFormat(tTest$estimate['mean of x'] - tTest$estimate['mean of y'], round),
                ' [',
                numberFormat(tTest$conf.int[1], round),
                ' ; ',
                numberFormat(tTest$conf.int[2], round),
                ']'
              )
              IC_diff[1, 5] <- paste0(
                numberFormat(tTest$estimate['mean of x'] - tTest$estimate['mean of y'], round),
                ' [',
                numberFormat(tTest$conf.int[1], round),
                ' ; ',
                numberFormat(tTest$conf.int[2], round),
                ']'
              )
              result <- rbind(result, IC_diff)
            }
            result <- cbind(result, currentResult)
          }

          # Cas où il y a plus de deux sous-groupe
          else {
            warning(paste0("You try to perform Student to compare ", length(subGroupDataList), " groups.
use 'ANOVA' ou 'Kruskal-Wallis' as forcedTest."))
            return(output)
          }

        }

        # Test de Student pour variance innégales
        else if(forcedTest == "Welch"){
          # On vérifie qu'il y a bien 2 sous-groupes à comparer
          if(length(subGroupDataList) == 2){

            # Modification du tableau de sortie si CONFINT = TRUE
            if (confint == TRUE) {
              currentResult <- createOutput(ncol = 2, nrow = (nrow(result) + 1))
              colnames(currentResult) <- c('Test', 'p-value')
            }

            tTest <- t.test(subGroupDataList[[1]], subGroupDataList[[2]], var.equal = FALSE)
            currentResult[1, 1] <- trimws(tTest$method)
            currentResult[1, 2] <- pvalFormat(pvalue = tTest$p.value, round = round)

            # On rajoute la ligne de l'IC de la différence
            if (confint == TRUE) {
              IC_diff <- createOutput(ncol = ncol(result), nrow = 1)
              colnames(IC_diff) <- colnames(result)
              IC_diff[1, 3] <- 'Mean difference [IC95%]'
              IC_diff[1, 4] <- paste0(
                numberFormat(tTest$estimate['mean of x'] - tTest$estimate['mean of y'], round),
                ' [',
                numberFormat(tTest$conf.int[1], round),
                ' ; ',
                numberFormat(tTest$conf.int[2], round),
                ']'
              )
              IC_diff[1, 5] <- paste0(
                numberFormat(tTest$estimate['mean of x'] - tTest$estimate['mean of y'], round),
                ' [',
                numberFormat(tTest$conf.int[1], round),
                ' ; ',
                numberFormat(tTest$conf.int[2], round),
                ']'
              )
              result <- rbind(result, IC_diff)
            }
            result <- cbind(result, currentResult)
          }
          # Cas où il y a plus de deux sous-groupe
          else {
            warning(paste0("You try to perform Welch to compare ", length(subGroupDataList), " groups.
use 'ANOVA' ou 'Kruskal-Wallis' as forcedTest."))
            return(output)
          }
        }

        # Test de Wilcoxon
        else if(forcedTest == "Wilcoxon"){
          # On vérifie qu'il y a bien 2 sous-groupes à comparer
          if(length(subGroupDataList) == 2){

            wilcoxTest <- suppressWarnings(wilcox.test(subGroupDataList[[1]][, 1],
                                                       subGroupDataList[[2]][, 1],
                                                       correct = TRUE))
            currentResult[1, 1] <- trimws(wilcoxTest$method)
            currentResult[1, 2] <- pvalFormat(pvalue = wilcoxTest$p.value, round = round)
            result <- cbind(result, currentResult)
          }
          # Cas où il y a plus de deux sous-groupe
          else {
            warning(paste0("You try to perform Wilcoxon to compare ", length(subGroupDataList), " groups.
use 'ANOVA' ou 'Kruskal-Wallis' as forcedTest."))
            return(output)
          }
        }

        # ANOVA
        else if(forcedTest == "ANOVA"){

          # On applique le test d'ANOVA uniquement sur les sous-groupes affichées
          if(!is.null(group_str)){
            anova <- aov(inputSubData[, variable] ~ inputSubData[, group])
          } else {
            anova <- aov(input[, variable] ~ input[, group])
          }

          currentResult[1, 1] <- 'Analysis of variance'
          currentResult[1, 2] <- pvalFormat(pvalue = summary(anova)[[1]][1, 'Pr(>F)'], round = round)
          result <- cbind(result, currentResult)

        }

        # Kruskal-Wallis
        else if(forcedTest == "Kruskal-Wallis"){

          # On applique le test d'ANOVA uniquement sur les sous-groupes affichées
          if(!is.null(group_str)){
            kruskalTest <- kruskal.test(inputSubData[, variable] ~ inputSubData[, group])
          } else {
            kruskalTest <- kruskal.test(input[, variable] ~ input[, group])
          }

          currentResult[1, 1] <- 'Kruskal-Wallis test'
          currentResult[1, 2] <- pvalFormat(pvalue = kruskalTest$p.value, round = round)
          result <- cbind(result, currentResult)

        } else {
          warning("forcedTest must be in 'Student', 'Welch', 'Wilcoxon', 'ANOVA' or 'Kruskal-Wallis'.")
          return(output)
        }
      }
    }
    # Si P_VALUE == FALSE, on créé quand même un tableau de résultat vide
    else {
      currentResult <- createOutput(ncol = 2, nrow = nrow(result))
      colnames(currentResult) <- c('Test', 'p-value')
      result <- cbind(result, currentResult)
    }
  }

  # On vérifie que le tableau des résultats d'entrée et le tableau des résultats
  # pour la variable en cours font le même nombre de colonnes et on renvoie le
  # tableau des résultats d'enntrée dans le cas contraire
  if (ncol(output) == 0 | ncol(output) == ncol(result)) {
    output <- rbind(output, result)
    if (!is.null(group)) {
      attr(output, 'var_group') <- group
      attr(output, 'label_var_group') <- getLabelFromVariable(input[group])
      if (!is.null(group_str)) {
        attr(output, 'modality_var_group') <- group_str
        attr(output, 'label_modality_var_group') <- levels(input[,group])[group_str]
      }
    }
    return(output)
  } else {
    warning(
      "output and result of this description have a different number of columns.
Generate a nex output with createOutput() function.")
    return(output)
  }
}



#' @title Generate description of factor varaible.
#' @description A method that generate description of factor variable and perform comparative statistic test
#' in case of comparaison group.
#' @param output a data.frame, containing a previously generated data.frame or a new data.frame.
#' data.frame are generated using createOutput() function.
#' @param input a data.frame containing the data to describe
#' @param variable a character vector of length 1. The name of the factor column to describe.
#' @param group a character vector of length 1. The name of the factor column to use as commparaison group. Default to NULL.
#' @param group_str a character vector. The name of the levels of the group variable to use. Default to NULL.
#' @param p_value a boolean. If TRUE, comparaison test are performed.
#' @param forcedTest a character vector of length 1. Must be one of "Chi-squared", "Corrected Chi-squared", "Fisher".
#' Default to NULL.
#' @param all a boolean. If TRUE, total column will be displayed. Default to FALSE
#' @param round an integer, number of maximal decimal. Default to 3
#' @param NA_asModality a boolean. If TRUE, missing data of the factor variable to describe will be considered as levels.
#' Default to FALSE
#' @param NA_group_AsModality a boolean. If TRUE, missing data of the group variable will be considered as levels.
#' Default to FALSE
#' @return a data.frame containing the description of the factor variable
#' @export
#' @examples
#' data(mtcars)
#' mtcars$am <- as.factor(mtcars$am)
#' mtcars$vs <- as.factor(mtcars$vs)
#' output <- createOutput()
#' output <- statsQT(output = output,
#'                   input = mtcars,
#'                   variable = "mpg",
#'                   group = "am")
#' output <- statsQL(output = output,
#'                   input = mtcars,
#'                   variable = "vs",
#'                   group = "am")
statsQL <- function(output, input, variable, group = NULL, group_str = NULL, p_value = FALSE, forcedTest = NULL,
                    all = NA_asModality, round = 3, NA_asModality = FALSE, NA_group_AsModality = FALSE) {

  if(!is.data.frame(output)){
    stop("output must be a data.frame")
  }

  if(!is.data.frame(input)){
    stop("input must be a data.frame")
  }

  if(!is.vector(variable) | !is.character(variable) | length(variable) !=1){
    stop("variable must a character vector of length 1")
  }

  if(!variable %in% colnames(input)){
    stop("variable must be the name of one column in input")
  }

  if(!is.factor(input[, variable])){
    stop("variable must be the name of a factor variable")
  }

  if(!is.vector(p_value) | !is.logical(p_value) | length(p_value) !=1){
    stop("p_value must be the logical vector of length 1")
  }

  if(!is.vector(all) | !is.logical(all) | length(all) !=1){
    stop("all must be the logical vector of length 1")
  }

  # On s'assure que la variable QL de dichotomisation est identique à celle utilisée précédement
  if (!is.null(attributes(output)$var_group)) {
    if (attributes(output)$var_group == group){
      variableQLidentical = TRUE
      if (is.null(group_str)) {
        if(is.null(attributes(output)$label_modality_var_group)){
          modaliteVariableQLidentical = TRUE
        } else {
          modaliteVariableQLidentical = FALSE
        }
      } else if (identical(attributes(output)$label_modality_var_group,
                           levels(input[, group])[group_str])) {
        modaliteVariableQLidentical = TRUE
      } else {
        modaliteVariableQLidentical = FALSE
      }
    } else {
      variableQLidentical = FALSE
    }
  } else {
    variableQLidentical = TRUE
    modaliteVariableQLidentical = TRUE
  }

  if(!variableQLidentical) {
    stop(paste0(group, " must be the same as thoose used previously (", attributes(output)$var_group, ").
                Generate a new output with the function createOutput() to describe data with an other group."))
  }

  if(!modaliteVariableQLidentical){
    if(is.null(group_str)){
      modalitesGroup <- "All"
    } else {
      modalitesGroup <- paste0(levels(input[, group])[group_str], collapse = ', ')
    }

    if(is.null(attributes(output)$label_modality_var_group)){
      modalitesOutput <- "All"
    } else {
      modalitesOutput <- paste0(attributes(output)$label_modality_var_group, collapse = ', ')
    }

    stop(paste0("group_str (", modalitesGroup, ") must be the same as thoose used previously (", modalitesOutput, ").
                Generate a new output with the function createOutput() to describe data with an other group_str."))
  }



  ##################################


  # Description de la variable QL globale
  if (is.null(group)) {
    result <- statQL(input = input,
                     variable = variable,
                     NA_asModality = NA_asModality)
    colnames(result) <- c('Variable', 'Modality', 'Description', 'All')
  }

  # Statistique descriptive par sous groupe en fonction de la variable GROUP
  else {
    # Comparaison selon certaines modalité de GROUP
    if(!is.null(group_str)){
      # On vérifie que les groupes existent bien dans la modalité
      if(any(!(group_str %in% which(!is.na(levels(input[, group])))))){
        modalitiesList <- paste0(c(1:length(levels(input[, group]))),
                                 ' - "',
                                 levels(input[, group]),
                                 '", ',
                                 collapse = '')

        warning(paste0('Les seuls sous groupes valables pour la variable ',
                       group, ' sont : ',
                       substr(modalitiesList, 1, nchar(modalitiesList)-2), "."),
                call. = FALSE)
        return(output)

      }
      # Toutes les modalitées de group_str sont des modalitées existantes pour la variable group
      else {
        vectSubData <- input[, group] %in% levels(input[, group])[group_str]
        if(NA_group_AsModality){
          vectSubData[is.na(vectSubData)] <- TRUE
        } else {
          vectSubData[is.na(vectSubData)] <- FALSE
        }
        inputSubData <- input[vectSubData,]
        inputSubData[group] <- as.factor(as.character(inputSubData[,group]))
        subGroupDataList <- getSubGroupFromDataFrame(df = inputSubData,
                                                     variable = variable,
                                                     group = group,
                                                     NA_group_AsModality = NA_group_AsModality)
        for (i in 1:length(subGroupDataList)) {
          # Statistiques par sous-gropupe (initialisation)
          if (i == 1) {
            currentResult <- statQL(input = subGroupDataList[[i]],
                                    variable = variable,
                                    round = round,
                                    NA_asModality = NA_asModality)
            colnames(currentResult) <- c('Variable',
                                         'Modality',
                                         'Description',
                                         levels(inputSubData[, group])[i])
            result <- currentResult
          }
          # Suite des statistiques par sous-gropupe (hors initialisation)
          else {
            currentResult <- statQL(input = subGroupDataList[[i]],
                                    variable = variable,
                                    round = round,
                                    NA_asModality = NA_asModality)

            if(!is.na(levels(inputSubData[, group])[i])){
              colnames(currentResult)[ncol(currentResult)] <- levels(inputSubData[, group])[i]
            } else {
              colnames(currentResult)[ncol(currentResult)] <- 'm.d.'
            }

            result <- cbind(result, currentResult[ncol(currentResult)])
          }
        }
      }
    }
    # Statistiques descriptives par sous-groupe selon toutes les modalités de GROUP
    else {
      subGroupDataList <- getSubGroupFromDataFrame(df = input,
                                                   variable = variable,
                                                   group = group,
                                                   NA_group_AsModality = NA_group_AsModality)

      for (i in 1:length(subGroupDataList)) {
        # Statistiques globales (initialisation)
        if (i == 1) {
          currentResult <- statQL(input = subGroupDataList[[i]],
                                  variable = variable,
                                  round = round,
                                  NA_asModality = NA_asModality)
          colnames(currentResult) <- c('Variable',
                                       'Modality',
                                       'Description',
                                       levels(input[, group])[i])
          result <- currentResult
        }
        # Suite des statistiques globales (hors initialisation)
        else {
          currentResult <- statQL(input = subGroupDataList[[i]],
                                  variable = variable,
                                  round = round,
                                  NA_asModality = NA_asModality)

          if(!is.na(levels(input[, group])[i])){
            colnames(currentResult)[ncol(currentResult)] <- levels(input[, group])[i]
          } else {
            colnames(currentResult)[ncol(currentResult)] <- 'm.d.'
          }

          result <- cbind(result, currentResult[ncol(currentResult)])
        }
      }
    }

    ####################################
    # Colonne totale
    ####################################

    # Affichage d'une colonne totale si ALL == TRUE
    if (all == TRUE) {

      # Cas où l'on affiche les statistiques descriptives que de certains sous groupe, la colonne totale
      # correspond aux sous-modalités affichées
      if(!is.null(group_str)){
        currentResult <- statQL(input = inputSubData,
                                variable = variable,
                                round = round,
                                NA_asModality = NA_asModality)
      }

      # Cas où l'on affiche les statistiques descriptives que de tous les sous-groupe, la colonne totale
      # correspond à l'ensemble des sous-modalitées
      else{
        currentResult <- statQL(input = input,
                                variable = variable,
                                round = round,
                                NA_asModality = NA_asModality)
      }
    }

    # On créé une colonne ALL vide
    else {
      if(is.null(group_str)){
        if (NA_asModality == TRUE) {
          currentResult <- createOutput(ncol = 4, nrow = length(levels(input[, variable])) + 3)
        } else {
          currentResult <- createOutput(ncol = 4, nrow = length(levels(input[, variable])) + 2)
        }
      } else{
        if (NA_asModality == TRUE) {
          currentResult <- createOutput(ncol = 4, nrow = length(levels(inputSubData[, variable])) + 3)
        } else {
          currentResult <- createOutput(ncol = 4, nrow = length(levels(inputSubData[, variable])) + 2)
        }
      }
    }
    colnames(currentResult) <- c('Variable', 'Modality', 'Description', 'All')
    result <- cbind(result, currentResult[ncol(currentResult)])

    # Modification du libelle de la ligne 'All modality' en fonction des cas
    if (NA_asModality == FALSE & all == FALSE) {
      result[2, 3] <- 'N'
    } else if (NA_asModality == TRUE) {
      result[2, 3] <- 'N (m.d.)'
    }

    # Réalisation d'une statistique si P_VALUE == TRUE
    if (p_value == TRUE) {

      # Création du tableau de sortie
      currentResult <- createOutput(ncol = 2, nrow = nrow(result))
      colnames(currentResult) <- c('Test', 'p-value')

      # On laisse le programme décider seul du test à réaliser
      if (is.null(forcedTest)) {
        # Cas où l'on affiche les statistiques de test entre certains sous-groupes, la colonne p-value
        # correspond aux tests faits sur les sous-modalités affichées
        if(!is.null(group_str)){
          ftable <- ftable(inputSubData[, variable] ~ inputSubData[, group])
          if(length(levels(inputSubData[, variable])) > 1 & length(levels(inputSubData[, group])) > 1 &
             !(any(rowSums(ftable) == 0) | any(colSums(ftable) == 0))){
            chiTest <- suppressWarnings(chisq.test(x = inputSubData[, variable],
                                                   y = inputSubData[, group],
                                                   correct = FALSE))
          } else {
            chiTest <- NULL
          }
        }
        # Cas où l'on affiche les statistiques de test entre l'ensemble des sous-groupes, la colonne p-value
        # correspond aux tests faits sur l'ensemble des sous-groupes
        else {
          ftable <- ftable(input[, variable] ~ input[, group])
          if(length(levels(input[, variable])) > 1 & length(levels(input[, group])) > 1 &
             !(any(rowSums(ftable) == 0) | any(colSums(ftable) == 0))){
            chiTest <- suppressWarnings(chisq.test(x = input[, variable],
                                                   y = input[, group],
                                                   correct = FALSE))
          } else {
            chiTest <- NULL
          }
        }

        if(!is.null(chiTest)){
          if (any(chiTest$expected < 3)) {
            fisherTest <- TRUE
          } else if (any(chiTest$expected < 5)) {
            fisherTest <- FALSE
            yatesChiSquared <- TRUE
          } else {
            fisherTest <- FALSE
            yatesChiSquared <- FALSE
          }

          if (fisherTest) {
            # Cas où l'on affiche les statistiques de test entre certains sous-groupes, la colonne p-value
            # correspond aux tests faits sur les sous-modalités affichées
            if(!is.null(group_str)){

              fisherTest <- tryCatch({
                fisher.test(x = inputSubData[, variable],
                            y = inputSubData[, group],
                            simulate.p.value = FALSE)
              }, error = function(e) {
                fisher.test(x = inputSubData[, variable],
                            y = inputSubData[, group],
                            simulate.p.value = TRUE)
              })

            }
            # Cas où l'on affiche les statistiques de test entre l'ensemble des sous-groupes, la colonne p-value
            # correspond aux tests faits sur l'ensemble des sous-groupes
            else {

              fisherTest <- tryCatch({
                fisher.test(x = input[, variable],
                            y = input[, group],
                            simulate.p.value = FALSE)
              }, error = function(e) {
                fisher.test(x = input[, variable],
                            y = input[, group],
                            simulate.p.value = TRUE)
              })

            }
            # Sauvegarde des résultats du Fisher
            currentResult[1, 1] <- trimws(gsub('\n\t', '', fisherTest$method))
            currentResult[1, 2] <- pvalFormat(fisherTest$p.value,
                                              round = round)
            result <- cbind(result, currentResult)
          } else if (yatesChiSquared) {

            # Cas où l'on affiche les statistiques de test entre certains sous-groupes, la colonne p-value
            # correspond aux tests faits sur les sous-modalités affichées
            if(!is.null(group_str)){
              chiTest <- suppressWarnings(chisq.test(x = inputSubData[, variable],
                                                     y = inputSubData[, group],
                                                     correct = TRUE))
            }
            # Cas où l'on affiche les statistiques de test entre l'ensemble des sous-groupes, la colonne p-value
            # correspond aux tests faits sur l'ensemble des sous-groupes
            else {
              chiTest <- suppressWarnings(chisq.test(x = input[, variable],
                                                     y = input[, group],
                                                     correct = TRUE))
            }
            # Sauvegarde des résultats du Chi² avec yates
            currentResult[1, 1] <- trimws(chiTest$method)
            currentResult[1, 2] <- pvalFormat(chiTest$p.value,
                                              round = round)
            result <- cbind(result, currentResult)

          } else {
            # Sauvegarde des résultats du Chi²
            currentResult[1, 1] <- trimws(chiTest$method)
            currentResult[1, 2] <- pvalFormat(chiTest$p.value,
                                              round = round)
            result <- cbind(result, currentResult)
          }
        } else {
          currentResult[1, 1] <- ''
          currentResult[1, 2] <- ''
          result <- cbind(result, currentResult)
        }


      }
      # On force le test à réaliser
      else {
        # Test du Chi deux
        if (forcedTest == "Chi-squared") {
          # Cas où l'on affiche les statistiques de test entre certains sous-groupes, la colonne p-value
          # correspond aux tests faits sur les sous-modalités affichées
          if(!is.null(group_str)){
            chiTest <- suppressWarnings(chisq.test(x = inputSubData[, variable],
                                                   y = inputSubData[, group],
                                                   correct = FALSE))
          }
          # Cas où l'on affiche les statistiques de test entre l'ensemble des sous-groupes, la colonne p-value
          # correspond aux tests faits sur l'ensemble des sous-groupes
          else {
            chiTest <- suppressWarnings(chisq.test(x = input[, variable],
                                                   y = input[, group],
                                                   correct = FALSE))
          }

          # Sauvegarde des résultats du Chi²
          currentResult[1, 1] <- trimws(chiTest$method)
          currentResult[1, 2] <- pvalFormat(chiTest$p.value, round = round)
          result <- cbind(result, currentResult)
        }

        # Test du Chi deux corrigé
        else if (forcedTest == "Corrected Chi-squared") {
          # Cas où l'on affiche les statistiques de test entre certains sous-groupes, la colonne p-value
          # correspond aux tests faits sur les sous-modalités affichées
          if(!is.null(group_str)){
            chiTest <- suppressWarnings(chisq.test(x = inputSubData[, variable],
                                                   y = inputSubData[, group],
                                                   correct = TRUE))
          }
          # Cas où l'on affiche les statistiques de test entre l'ensemble des sous-groupes, la colonne p-value
          # correspond aux tests faits sur l'ensemble des sous-groupes
          else {
            chiTest <- suppressWarnings(chisq.test(x = input[, variable],
                                                   y = input[, group],
                                                   correct = TRUE))
          }

          # Sauvegarde des résultats du Chi² de yates
          currentResult[1, 1] <- trimws(chiTest$method)
          currentResult[1, 2] <- pvalFormat(chiTest$p.value, round = round)
          result <- cbind(result, currentResult)
        }

        # Test de Fisher
        else if (forcedTest == "Fisher") {
          # Cas où l'on affiche les statistiques de test entre certains sous-groupes, la colonne p-value
          # correspond aux tests faits sur les sous-modalités affichées
          if(!is.null(group_str)){
            fisherTest <- tryCatch({
              fisher.test(x = inputSubData[, variable],
                          y = inputSubData[, group],
                          simulate.p.value = FALSE)
            }, error = function(e) {
              fisher.test(x = inputSubData[, variable],
                          y = inputSubData[, group],
                          simulate.p.value = TRUE)
            })
          }
          # Cas où l'on affiche les statistiques de test entre l'ensemble des sous-groupes, la colonne p-value
          # correspond aux tests faits sur l'ensemble des sous-groupes
          else {
            fisherTest <- tryCatch({
              fisher.test(x = input[, variable],
                          y = input[, group],
                          simulate.p.value = FALSE)
            }, error = function(e) {
              fisher.test(x = input[, variable],
                          y = input[, group],
                          simulate.p.value = TRUE)
            })
          }

          # Sauvegarde des résultats du Fisher
          currentResult[1, 1] <- trimws(gsub('\n\t', '', fisherTest$method))
          currentResult[1, 2] <- pvalFormat(fisherTest$p.value,
                                            round = round)
          result <- cbind(result, currentResult)
        }

        # Other case
        else {
          warning(
            "forcedTest must be in 'Chi-squared', 'Corrected Chi-squared', 'Fisher'.",
            call. = FALSE
          )
          return(output)
        }
      }
    }
    # Si P_VALUE == FALSE, on créé un tableau de résultat vide
    else {
      currentResult <- createOutput(ncol = 2, nrow = nrow(result))
      colnames(currentResult) <- c('Test', 'p-value')
      result <- cbind(result, currentResult)
    }
  }

  # On vérifie que le tableau des résultats d'entrée et le tableau des résultats
  # pour la variable en cours font le même nombre de colonnes et on renvoie le
  # tableau des résultats d'enntrée dans le cas contraire
  if (ncol(output) == 0 | ncol(output) == ncol(result)) {
    output <- rbind(output, result)
    if (!is.null(group)) {
      attr(output, 'var_group') <- group
      attr(output, 'label_var_group') <- getLabelFromVariable(input[group])
      if (!is.null(group_str)) {
        attr(output, 'modality_var_group') <- group_str
        attr(output, 'label_modality_var_group') <- levels(input[,group])[group_str]
      }
    }
    return(output)
  }
  else {
    warning(
      "output and result of this description have a different number of columns.
Generate a nex output with createOutput() function.")
    return(output)
  }
}
