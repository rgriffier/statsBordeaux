#' @title split data based on factor variable
#' @description split data based on a levels factor variable as a list of sub-data.frame
#' @param df a data.frame containing data to split
#' @param variable a string. The colname of the data to split.
#' @param group a string. The colname of the factor data that will be used as sub-group.
#' @param NA_group_AsModality a boolean. If TRUE, NA will be considered as modality. FALSE by default
#' @return a list of data.frame
#' @noRd
#' @examples
#' df <- data.frame(X1 = c(0, 1, 2, 1, 2, 1, 2),
#'                  X2 = c(0, 1, 0, 1, 1, 0, 0))
#' labelTable <- data.frame(VARIABLE = c("X2", "X2"),
#'                          MODALITY = c(0, 1),
#'                          LABEL = c("Male", "Female"))
#' df <- labellisationDataFrame(df, labelTable)
#' list_subGroup <- getSubGroupFromDataFrame(df = df, variable = "X1", group = "X2")
getSubGroupFromDataFrame <- function(df, variable, group, NA_group_AsModality = FALSE) {

  # On vérifie que df est un data.frame
  if (!is.data.frame(df)) {
    stop("df must be a data.frame")
  }

  # Liste de sortie
  result <- list()
  # On sauverage les attributs, en particulier l'attribut var_label
  attr <- attributes(df[, variable])
  # Pour chaque modalité de la variable 'group', on génère un sub.data.frame
  for (i in 1:length(levels(df[, group]))) {
    # On génère le sub.data.frame basé sur la variable 'group'
    subData <- subset(df, df[,group] == levels(df[, group])[i])[variable]
    colnames(subData) <- variable
    # Gestion des attributs
    attributes(subData[, 1]) <- attr
    # On stoque le nom de la variable de QL de dichotomisation
    attr(subData[, 1], "var_sub_grouped") <- group
    # On stoque le nom de la modalité correspondant au sous-groupe
    attr(subData[, 1], "modality_var_sub_grouped") <- levels(df[, group])[i]
    # On retourne le sub.data.frame dans la lite de résultats finaux
    result[[i]] <- subData
  }
  # Si le paramètre NA_group_AsModality == TRUE, on créé un sous-groupe pour les valeurs NA de la variable 'group'
  if(NA_group_AsModality){
    if(nrow(df[is.na(df[, group]), ][variable]) != 0){
      subData <- df[is.na(df[, group]), ][variable]
      colnames(subData) <- variable
      attributes(subData[, 1]) <- attr
      attr(subData[, 1], "var_sub_grouped") <- group
      attr(subData[, 1], "modality_var_sub_grouped") <- '<NA>'
      result[[i + 1]] <- subData
    }
  }
  return(result)
}


#' @title A conveinient method to check if data.frame contain only factor or numeric variable
#' @description A conveinient metho to check if data.frame contain only factor or numeric variable
#' @param data a data.frame
#' @return a boolean vector of length one
#' @noRd
#' @examples
#' df <- data.frame(X1 = c(0, 1, 2, 1, 2, 1, 2),
#'                  X2 = c(0, 1, 0, 1, 1, 0, 0))
#' checkIfNumericOrFactor(df)
checkIfNumericOrFactor <- function(data){
  result <- lapply(data, function(x){
    any(c(is.numeric(x), is.factor(x)))
  })
  if(any(!unlist(result))){
    errorVariable <- unlist(result)[!unlist(result)]
    warning(paste0(names(errorVariable), " is not factor or numeric", collapse = "\n"), call. = FALSE)
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' @title Get the mod of numeric vector
#' @description  Get the most represented value (mod) of numeric vector
#' @param data a numeric vector
#' @return a numeric vector of length 1
#' @noRd
#' @examples
#' numeric <- c(1, 2, 1, 2, 3, 1, 4, 1, 5, 1, 7, 8 , 1)
#' getMode(numeric)
getMode <- function(data) {
  if(!is.vector(data) | !is.numeric(data)){
    stop("data must be a numeric vector")
  }
  uniqData <- unique(data[!is.na(data)])
  mode <- uniqData[which.max(tabulate(match(data, uniqData)))]
  return(mode)
}


#' @title an internal function to do the description of quantitative variable from a data.frame
#' @description an internal function to do the description of quantitative variable from a data.frame
#' @param data a data.frame containing the data to analyse
#' @param variable a characher vector of length 1 containing the name of the column to describe
#' @param round an integer, number of maximal decimal. Default to 3
#' @param confint a boolean. If TRUE, the confidence interval of the mean will be displayed. Default to FALSE
#' @param desc a character vector. Could contain "Mean", "Median", "Range" and/or "Mode"
#' @return a data.frame containing the description of the quantitative variable
#' @noRd
#' @examples
#' data(mtcars)
#' statQT(mtcars, "mpg")
statQT <- function(data, variable, round = 3, confint = FALSE, desc = c("Mean", "Median", "Range")) {
  if(!is.data.frame(data)){
    stop("data must be a data.frame")
  }
  if(!is.vector(variable) | !is.character(variable) | length(variable) !=1){
    stop("variable must a character vector of length 1")
  }
  if(!variable %in% colnames(data)){
    stop("variable must be the name of one column in data")
  }
  if(!is.numeric(data[, variable])){
    stop("variable must be the name of a numeric variable")
  }
  if(!is.vector(round) | !is.numeric(round) | length(variable) !=1){
    stop("round must a numeric vector of length 1")
  }
  if(!is.vector(confint) | !is.logical(confint) | length(confint) !=1){
    stop("confint must a logical vector of length 1")
  }
  if(length(setdiff(desc, c("Mean", "Median", "Range", "Mode"))) != 0) {
    stop(paste0("desc must be a characher vector composer with 'Mean', 'Median', 'Range' or 'Mode'"))
  }

  ## output creation
  output <- data.frame(matrix(ncol = 4, nrow = 0))

  ## descriptive statistic
  summary <- summary(data[, variable])

  min <- setFormatToNumber(as.numeric(summary['Min.']), round)
  quart1st <- setFormatToNumber(as.numeric(summary['1st Qu.']), round)
  median <- setFormatToNumber(as.numeric(summary['Median']), round)
  mean <- setFormatToNumber(as.numeric(summary['Mean']), round)
  quart3th <- setFormatToNumber(as.numeric(summary['3rd Qu.']), round)
  max <- setFormatToNumber(as.numeric(summary['Max.']), round)
  sd <- setFormatToNumber(sd(data[, variable], na.rm = TRUE), round)

  ## current row definition
  currentRow <- nrow(output) + 1

  ## varName
  output[currentRow, 1] <- getVarLabel(data[variable])
  currentRow <- nrow(output) + 1

  ## sample size
  output[currentRow, 3] <- getSampleSizeMissingData_text(getOption('lang.value')) #N (m.d.)
  output[currentRow, 4] <- paste0(sum(!is.na(data[variable])), ' (', sum(is.na(data[variable])), ')')
  currentRow <- nrow(output) + 1

  ## mean and sd
  if('Mean' %in% desc) {
    output[currentRow, 3] <- getMeanStdv_text(getOption('lang.value')) #'Mean (SD)'
    output[currentRow, 4] <- paste0(mean, ' (', sd, ')')
    currentRow <- nrow(output) + 1

    ## confint
    if(confint == TRUE) {
      output[currentRow, 3] <- getConfintMean_text(getOption('lang.value')) #'IC95% [Mean]'
      if(!is.na(sd(data[, variable], na.rm = TRUE)) & sd(data[, variable], na.rm = TRUE) != 0) {
        confint_mean <- setFormatToNumber(t.test(data[, variable])$conf.int, round)
        output[currentRow, 4] <- paste0('[', confint_mean[1], ' ; ', confint_mean[2], ']')
      } else {
        output[currentRow, 4] <- NA
      }
      currentRow <- nrow(output) + 1
    }
  }

  ## median and IQR
  if('Median' %in% desc){
    output[currentRow, 3] <- getMedianIRQ_text(getOption('lang.value')) #'Median [Q1 ; Q3]'
    output[currentRow, 4] <- paste0(median, ' [', quart1st, ' ; ', quart3th, ']')
    currentRow <- nrow(output) + 1
  }

  ## range
  if('Range' %in% desc){
    output[currentRow, 3] <- getRange_text(getOption('lang.value')) #'Min ; Max'
    output[currentRow, 4] <- paste0(min, ' ; ', max)
    currentRow <- nrow(output) + 1
  }

  ## mode
  if('Mode' %in% desc & !any(grepl('\\.', as.character(data[, variable])))){
    output[currentRow, 3] <- getMode_text(getOption('lang.value')) #'Mode'
    output[currentRow, 4] <- setFormatToNumber(getMode(data[, variable]), round)
    currentRow <- nrow(output) + 1
  }
  return(output)
}


#' @title an internal function to do the description of factor from a data.frame
#' @description an internal function to do the description of factor from a data.frame
#' @param data a data.frame containing the data to analyse
#' @param variable a characher vector of length 1 containing the name of the column to describe
#' @param round an integer, number of maximal decimal. Default to 3
#' @param NA_asModality a boolean. If TRUE, missing data of the variable will be considered as levels.
#' Default to FALSE
#' @return a data.frame containing the description of the factor
#' @noRd
#' @examples
#' data(mtcars)
#' mtcars$am <- as.factor(mtcars$am)
#' statQL(mtcars, "am")
statQL <- function(data, variable, round = 3, NA_asModality = FALSE) {
  if(!is.data.frame(data)){
    stop("data must be a data.frame")
  }
  if(!is.vector(variable) | !is.character(variable) | length(variable) !=1){
    stop("variable must a character vector of length 1")
  }
  if(!variable %in% colnames(data)){
    stop("variable must be the name of one column in data")
  }
  if(!is.factor(data[, variable])){
    stop("variable must be the name of a numeric variable")
  }
  if(!is.vector(round) | !is.numeric(round) | length(variable) !=1){
    stop("round must a numeric vector of length 1")
  }
  if(!is.vector(NA_asModality) | !is.logical(NA_asModality) | length(NA_asModality) !=1){
    stop("NA_asModality must be the logical vector of length 1")
  }

  ## output creation
  output <- data.frame(matrix(ncol = 4, nrow = 0))

  ## sample size
  size <- table(data[, variable], useNA = 'always')
  sizeWithoutNA <- table(data[, variable], useNA = 'no')
  percent <- setFormatToNumber(prop.table(table(data[, variable], useNA = 'always')) * 100, round)
  percentWithoutNA <- setFormatToNumber(prop.table(table(data[, variable], useNA = 'no')) * 100, round)

  ## result save
  for(i in 1:length(size)){
    # initialisation
    if(i == 1){
      output[i, 1] <- getVarLabel(data[variable])
      # if NA_asModality == FALSE, missing data in header
      if(NA_asModality){
        output[i + 1, 2] <- getAllModailties_text(getOption('lang.value')) #'All modalities'
        output[i + 1, 3] <- getSampleSizeMissingData_text(getOption('lang.value')) #'N (m.d.)'
        output[i + 1, 4] <- paste0(sum(sizeWithoutNA), ' (', sum(size) - sum(sizeWithoutNA), ')')
      } else {
        output[i + 1, 2] <- getAllModailties_text(getOption('lang.value')) #'All modalities'
        output[i + 1, 3] <- getSampleSizeMissingDataPercent_text(getOption('lang.value')) #'N (m.d. ; %)'
        output[i + 1, 4] <- paste0(sum(sizeWithoutNA), ' (', sum(size) - sum(sizeWithoutNA), ' ; ', percent[length(size)], ')')
      }
    }
    ## NA_asModality == TRUE and NA current modality
    if(is.na(names(size[i])) & NA_asModality == TRUE){
      output[i + 2, 2] <- getMissingData_text(getOption('lang.value')) #'m.d.'
      output[i + 2, 3] <- getModailtySizePercent_text(getOption('lang.value')) #'N (%)'
      output[i + 2, 4] <- paste0(size[i], ' (', percent[i], ')')
    }
    ## NA_asModality == TRUE and current modality not NA
    else if(!is.na(names(size[i])) & NA_asModality == TRUE){
      output[i + 2, 2] <- names(size[i])
      output[i + 2, 3] <- getModailtySizePercent_text(getOption('lang.value')) #'N (%)'
      output[i + 2, 4] <- paste0(size[i], ' (', percent[i], ')')
    }
    ## NA_asModality == FALSE
    else if(!is.na(names(size[i])) & NA_asModality == FALSE){
      output[i + 2, 2] <- names(size[i])
      output[i + 2, 3] <- getModailtySizePercent_text(getOption('lang.value')) #'N (%)'
      output[i + 2, 4] <- paste0(size[i], ' (', percentWithoutNA[i], ')')
    }
  }
  return(output)
}


#' @title A convenient that remove rows with same id_paired in case of missing data
#' @description A convenient that remove rows with same id_paired in case of missing data
#' @param df a data.frame, containing the paired data to analyse
#' @param variable a character vector of length one, containing the colname of the variable to analyse
#' @param group a character vector of length one, containing the colname of the group variable
#' @param id_paired a character vector of length one, containing the colname of the id_paired
#' @return a data.frame, containing paired data ready to be analyzed
#' @noRd
manageDataBeforePairedTest <- function(df, variable, group, id_paired){
  result <- df[, c(id_paired, variable, group)]
  result <- result[complete.cases(result), ]
  result <- result[result[, id_paired] %in% names(which(table(result[, id_paired]) == 2)), ]
  return(result)
}


#' @title A convenient warp method to describe a full data.frame.
#' @description  A convenient warp method to describe a full data.frame.
#' @param data a data.frame containing the data to describe
#' @param variable a character vector contening the name of column to describe. Default to colnames(data).
#' @param applicable a list of boolean vector generating based on manageNotApplicable()
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
#' @noRd
jumpDescribeDataFrame <- Vectorize(function(data, variable, applicable = NULL, group = NULL, group_str = NULL,
                                            p_value = FALSE, all = FALSE, desc = c("Mean", "Median", "Range"),
                                            round = 3, confint = FALSE, NA_asModality = FALSE, NA_group_AsModality = FALSE){
  if(!is.null(applicable)){
    if(is.numeric(data[, variable])){
      output <- statsQT(
        data = subset_withAttributes(
          data = data,
          subset = applicable[[colnames(data[variable])]]
        ),
        variable = colnames(data[variable]),
        group = group,
        group_str = group_str,
        p_value = p_value,
        all = all,
        desc = desc,
        round = round,
        confint = confint,
        NA_group_AsModality = NA_group_AsModality)
    }
    else if(is.factor(data[, variable])){
      output <- statsQL(
        data = subset_withAttributes(
          data = data,
          subset = applicable[[colnames(data[variable])]]
        ),
        variable = colnames(data[variable]),
        group = group,
        group_str = group_str,
        p_value = p_value,
        all = all,
        round = round,
        NA_asModality = NA_asModality,
        NA_group_AsModality = NA_group_AsModality)
    }
    else {
      message(paste0("Variable '", colnames(data[variable]), "' non décrite (", class(data[, variable]), ")"))
      return(NULL)
    }
  } else {
    if(is.numeric(data[, variable])){
      output <- statsQT(
        data = data,
        variable = colnames(data[variable]),
        group = group,
        group_str = group_str,
        p_value = p_value,
        all = all,
        desc = desc,
        round = round,
        confint = confint,
        NA_group_AsModality = NA_group_AsModality)
    }
    else if(is.factor(data[, variable])){
      output <- statsQL(
        data = data,
        variable = colnames(data[variable]),
        group = group,
        group_str = group_str,
        p_value = p_value,
        all = all,
        round = round,
        NA_asModality = NA_asModality,
        NA_group_AsModality = NA_group_AsModality)
    }
    else {
      message(paste0("Variable '", colnames(data[variable]), "' non décrite (", class(data[, variable]), ")"))
      return(NULL)
    }
  }
  return(output)
}, vectorize.args = "variable", SIMPLIFY = FALSE, USE.NAMES = FALSE)

#' @title Test normality of numeric variable based on graphic and statistc methods
#' @description Test normality of numeric variable based on graphic and statistc methods
#' @param data a data.frace conteting the numeric data witch normality need to be evaluate
#' @param variable a character vector of length 1. The name of the numeric column to describe.
#' @param group a character vector of length 1. The name of the factor column to use as commparaison group. Default to NULL.
#' @param p_value a boolean. If TRUE, comparaison test to normality are performed. Default to FALSE.
#' @param method a character vector of length 1. The name og the statistic method to use : 'Kolmogorov' or 'Shapiro'
#' @return a plot
#' @importFrom ggpubr ggtexttable ggparagraph ggarrange
#' @import ggplot2
#' @noRd
checkNormalityInternal <- Vectorize(function(data, variable, group = NULL, p_value = FALSE, method = 'Kolmogorov') {

  if(!is.data.frame(data)){
    stop("data must be a data.frame.")
  }
  if(!is.vector(variable) | !is.character(variable) | length(variable) != 1){
    stop("variable must be a character vector of length one.")
  }
  if(!variable %in% colnames(data)){
    stop("variable must be the name of one column in data.")
  }
  if(!is.numeric(data[, variable])){
    stop("variable must be a numerical variable in data.")
  }
  if(!is.null(group)){
    if(!is.vector(group) | !is.character(group) | length(group) != 1){
      stop("group must be a character vector of length one.")
    }
    if(!group %in% colnames(data)){
      stop("group must be the name of one column in data.")
    }
  }
  if(!is.vector(p_value) | !is.logical(p_value) | length(p_value) != 1){
    stop("p_value must be a boolean vector of length one.")
  }
  if(isTRUE(p_value)){
    if(!is.vector(method) | !is.character(method) | length(method) != 1 | !method %in% c('Kolmogorov', 'Shapiro')){
      stop("method must be a character vector of length one in 'Kolmogorov' or 'Shapiro'.")
    }
  }

  currentData <- colnames(data[variable])
  if(p_value){
    dataStatistic <- as.data.frame(matrix(nrow = 0, ncol = 4))
    colnames(dataStatistic) <- c("Groupe", "Effectif", "Test", "p-value")

    if(method == "Kolmogorov"){
      ## Kolmogorov-Smirnov normality test
      if(!is.null(group)){
        j <- 0
        for(grp in levels(data[, group])){
          j <- j + 1
          dataKolmogorov <- subset(data[, variable], data[group] == grp)
          dataStatistic[j, 1] <- grp
          dataStatistic[j, 2] <- sum(!is.na(dataKolmogorov))
          if(sum(!is.na(dataKolmogorov)) >= 2 & sum(!is.na(unique(dataKolmogorov))) > 1){
            kolmogorov <-  suppressWarnings(ks.test(dataKolmogorov,"plnorm", mean(dataKolmogorov, na.rm = TRUE),
                                                    sd(dataKolmogorov, na.rm = TRUE)))
            dataStatistic[j, 3] <- kolmogorov$method
            dataStatistic[j, 4] <- setFormatToPvalue(kolmogorov$p.value)
          } else {
            dataStatistic[j, 3] <- "-"
            dataStatistic[j, 4] <- "-"
          }
        }
      } else {
        dataKolmogorov <- data[, variable]
        dataStatistic[1, 1] <- ""
        dataStatistic[1, 2] <- sum(!is.na(dataKolmogorov))
        if(sum(!is.na(dataKolmogorov)) >= 2 & sum(!is.na(unique(dataKolmogorov))) > 1){
          kolmogorov <-  suppressWarnings(ks.test(dataKolmogorov,"plnorm", mean(dataKolmogorov, na.rm = TRUE),
                                                  sd(dataKolmogorov, na.rm = TRUE)) )
          dataStatistic[1, 3] <- kolmogorov$method
          dataStatistic[1, 4] <- setFormatToPvalue(kolmogorov$p.value)
        } else {
          dataStatistic[1, 3] <- "-"
          dataStatistic[1, 4] <- "-"
        }
      }
    } else if(method == "Shapiro"){
      ## Shapiro-Wilk normality test
      if(!is.null(group)){
        j <- 0
        for(grp in levels(data[, group])){
          j <- j+1
          dataShapiro <- subset(data[, variable], data[group] == grp)
          dataStatistic[j, 1] <- grp
          dataStatistic[j, 2] <- sum(!is.na(dataShapiro))
          if(sum(!is.na(dataShapiro)) >= 3 & sum(!is.na(dataShapiro)) <= 5000 & sum(!is.na(unique(dataShapiro))) > 1){
            shapiro <- shapiro.test(dataShapiro)
            dataStatistic[j, 3] <- shapiro$method
            dataStatistic[j, 4] <- setFormatToPvalue(shapiro$p.value)
          } else {
            dataStatistic[j, 3] <- "-"
            dataStatistic[j, 4] <- "-"
          }
        }
      } else {
        dataShapiro <- data[, variable]
        dataStatistic[1, 1] <- ""
        dataStatistic[1, 2] <- sum(!is.na(dataShapiro))
        if(sum(!is.na(dataShapiro)) >= 3 & sum(!is.na(dataShapiro)) <= 5000 & sum(!is.na(unique(dataShapiro))) > 1){
          shapiro <- shapiro.test(dataShapiro)
          dataStatistic[1, 3] <- shapiro$method
          dataStatistic[1, 4] <- setFormatToPvalue(shapiro$p.value)
        } else {
          dataStatistic[1, 3] <- "-"
          dataStatistic[1, 4] <- "-"
        }
      }

      if(is.null(group)){
        dataStatistic$Groupe <- NULL
      }
    }
  }

  ## plot
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = get(currentData))) +
    ggplot2::geom_histogram(alpha = 0.8, fill = "lightblue", color = "black", bins = 30) +
    ggplot2::xlab(attr(data[, variable], "var_label")) +
    ggplot2::ylab("Effectif") +
    ggplot2::theme_minimal()

  if(!is.null(group)){
    plot <- plot + ggplot2::facet_grid(get(group) ~ .)
  }

  if(p_value){
    table <- ggpubr::ggtexttable(dataStatistic, rows = NULL)
    paragraph <- ggpubr::ggparagraph(text = "H0 : la variable suit une loi normale.\nH1 : la variable ne suit pas une loi normale.\np-value < 0,05 : on rejette H0, la variable ne suit pas une loi normale", face = "italic", size = 11, color = "black")
    plot <- suppressWarnings(
      ggpubr::ggarrange(plot, table, paragraph,
                        ncol = 1, nrow = 3,
                        heights = c(0.7, 0.3, 0.3), align = "hv"))
  }
  return(plot)
}, vectorize.args = "variable", SIMPLIFY = FALSE)


#' @title An internal function that allow to get the label of variable of a data.frame
#' @description An internal function that allow to get the label of variable of a data.frame
#' @param data a data.frame in witch we want to get
#' @param variable a character vector of length one, the colname to get varriable label
#' @return a character vector of length one, containing the varriable label
#' @noRd
getVarLabel_int <- Vectorize(function(data, variable){
  if(!is.data.frame(data)) {
    stop("data must be a data.frame.")
  }
  varLabel <- attributes(data[, variable])$var_label
  if(is.null(varLabel)){
    varLabel <- attributes(data[, variable])$label
  }
  if(is.null(varLabel)){
    varLabel <- variable
  }
  return(clearLabel(varLabel))
}, vectorize.args = "variable", SIMPLIFY = TRUE, USE.NAMES = TRUE)


#' @title A convenient method to get the test to perform in case of 2 groups numeric comparaison
#' @description A convenient method to get the test to perform in case of 2 groups numeric comparaison
#' @param data.as.list a list of data containing the two vectors of numerical data
#' @return a character vector of length one, containing the test name to perform
#' @noRd
getTest_numericComparaison_2groups <- function(data.as.list){

  ## test's application condition
  allSampleSizeSup_30 <- all(sapply(data.as.list, function(x) {sum(!is.na(x))}) >= 30)
  if(!allSampleSizeSup_30){
    allSampleSizeSup_2 <- all(sapply(data.as.list, function(x) {sum(!is.na(x))}) >= 2)
  }
  allVar_notZero <- all(sapply(data.as.list, function(x) {sd(unlist(x), na.rm = TRUE)}) != 0)

  ## test if all sample size >= 30
  if (allSampleSizeSup_30) {
    ## test if all sd <> 0
    if (allVar_notZero) {
      ## equality of variance test
      varTest <- var.test(x = unlist(data.as.list[[1]]), unlist(data.as.list[[2]]))
      if (varTest$p.value < 0.05) {
        return("t-test for inequal variances")
      } else {
        return("t-test for equal variances")
      }
    } else {
      return(getVariabililtyNeeded_text(getOption('lang.value')))
    }
  }
  ## test if all sample size >=2
  else if (allSampleSizeSup_2) {
    ## test if all sd <> 0
    if (allVar_notZero) {
      return("Wilcoxon test")
    } else {
      return(getVariabililtyNeeded_text(getOption('lang.value')))
    }
  }
  # test not possible
  else {
    return(getSmallSample_text(getOption('lang.value')))
  }
}



#' @title A convenient method to get the test to perform in case of more than 2 groups numeric comparaison
#' @description A convenient method to get the test to perform in case of more than 2 groups numeric comparaison
#' @param data.as.list a list of data containing the vectors of numerical data
#' @return a character vector of length one, containing the test name to perform
#' @noRd
getTest_numericComparaison_moreThan2groups <- function(data.as.list){

  ## test's application condition
  allSampleSizeSup_30 <- all(sapply(data.as.list, function(x) {sum(!is.na(x))}) >= 30)
  allSampleSizeSup_2 <- all(sapply(data.as.list, function(x) {sum(!is.na(x))}) >= 2)

  ## test if all sample size >= 30
  if (allSampleSizeSup_30) {
    ## Homoscedasticity test
    bartlett <- bartlett.test(lapply(data.as.list, function(x){ unlist(x)}))
    if(bartlett$p.value < 0.05){
      hypothesisANOVA <- FALSE
    } else {
      hypothesisANOVA <- TRUE
    }
  } else {
    hypothesisANOVA <- FALSE
  }
  # ANOVA
  if (hypothesisANOVA) {
    return("Analysis of variance")
  } else {
    ## test if all sample size >= 2
    if (allSampleSizeSup_2) {
      return("Kruskal-Wallis test")
    } else {
      return(getSmallSample_text(getOption('lang.value')))
    }
  }
}



#' @title A convenient method to get the test to perform in case of numeric comparaison
#' @description A convenient method to get the test to perform in case of numeric comparaison
#' @param data.as.list a list of data containing the vectors of numerical data
#' @return a character vector of length one, containing the test name to perform
#' @noRd
getTest_numericComparaison <- function(data.as.list){
  if(length(data.as.list) == 2){
    resultTest <- getTest_numericComparaison_2groups(data.as.list)
  } else if(length(data.as.list) > 2){
    resultTest <- getTest_numericComparaison_moreThan2groups(data.as.list)
  }
  return(resultTest)
}



#' @title A convenient method to get the test to perform in case of qualitative comparaison
#' @description A convenient method to get the test to perform in case of qualitative comparaison
#' @param data a data.frame containing the qualitative data to compare
#' @param variable a character vector of length one, cntaining the colname of the variable to compare
#' @param group a character vector of length one, cntaining the colname of the group variable to use in comparaison
#' @return a character vector of length one, containing the test name to perform
#' @noRd
getTest_factorComparaison <- function(data, variable, group){
  if(!is.data.frame(data)){
    stop("data must be a data.frame")
  }
  if(!is.vector(variable) || !is.character(variable) || length(variable) != 1 || !variable %in% colnames(data)){
    stop("variable must be a character vector as colname of some column in data")
  }
  if(!is.vector(group) || !is.character(group) || length(group) != 1 || !group %in% colnames(data)){
    stop("group must be a character vector as colname of some column in data")
  }

  ftable <- ftable(data[, variable] ~ data[, group])
  if(length(levels(data[, variable])) > 1 & length(levels(data[, group])) > 1){
    if(all(rowSums(ftable) != 0) | all(colSums(ftable) != 0)){
      expected <- tcrossprod(rowSums(ftable), colSums(ftable))/sum(rowSums(ftable))
      if(all(expected > 5)){
        return("Pearson's Chi-squared test")
      } else if(all(expected > 3)){
        if(length(ftable) == 4){
          return("Pearson's Chi-squared test with Yates' continuity correction")
        } else {
          return("Fisher's Exact Test for Count Data")
        }
      } else {
        return("Fisher's Exact Test for Count Data")
      }
    } else {
      return("Fisher's Exact Test for Count Data")
    }
  } else {
    return(getNotEnought_text(getOption('lang.value')))
  }
}
