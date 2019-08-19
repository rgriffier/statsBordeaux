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


#' @title format some numeric for display usage
#' @description format numeric with comma as decimal separator and space as big-mark
#' @param num a digit, witch need to be formated
#' @param round an integer, number of maximal decimal. Default to 3
#' @return a character representing the formated digit
#' @noRd
#' @examples
#' num <- 10348.3147
#' num <- numberFormat(num)
numberFormat <- function(num, round = 3){
  if(!is.numeric(num)){
    stop("num must be a numeric vector")
  }
  if(!is.vector(round) | !is.numeric(round) | length(round) !=1){
    stop("round must be a integer vector of length 1")
  }
  num <- round(num, round)
  # num <- format(num, decimal.mark = ",")
  num <- gsub('\\.', ',', num)
  return(num)
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
#' @param input a data.frame
#' @param variable a characher vector of length 1 containing the name of the column to describe
#' @param round an integer, number of maximal decimal. Default to 3
#' @param confint a boolean. If TRUE, the confidence interval of the mean will be displayed. Default to FALSE
#' @param desc a character vector. Could contain "Mean", "Median", "Range" and/or "Mode"
#' @return a data.frame containing the description of the quantitative variable
#' @noRd
#' @examples
#' data(mtcars)
#' statQT(mtcars, "mpg")
statQT <- function(input, variable, round = 3, confint = FALSE, desc = c("Mean", "Median", "Range")) {

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

  # Création du tableau de sortie
  output <- data.frame(matrix(ncol = 4, nrow = 0))

  # Statistiques descriptives
  summary <- summary(input[, variable])

  min <- numberFormat(as.numeric(summary['Min.']), round)
  quart1st <- numberFormat(as.numeric(summary['1st Qu.']), round)
  median <- numberFormat(as.numeric(summary['Median']), round)
  mean <- numberFormat(as.numeric(summary['Mean']), round)
  quart3th <- numberFormat(as.numeric(summary['3rd Qu.']), round)
  max <- numberFormat(as.numeric(summary['Max.']), round)
  sd <- numberFormat(sd(input[, variable], na.rm = TRUE), round)

  # On initialise la ligne actuelle comme la premièrè ligne du tableau
  currentRow <- nrow(output) + 1

  # Sauvegarde du nom de la variable
  output[currentRow, 1] <- getLabelFromVariable(input[variable])
  currentRow <- nrow(output) + 1

  # Sauvegarde des éffectifs
  output[currentRow, 3] <- 'N (m.d.)'
  output[currentRow, 4] <- paste0(sum(!is.na(input[variable])), ' (', sum(is.na(input[variable])), ')')
  currentRow <- nrow(output) + 1

  # Sauvegarde de la moyenne / ecart-type si desc contient 'Mean'
  if (any(grepl('Mean', desc))) {
    output[currentRow, 3] <- 'Mean (SD)'
    output[currentRow, 4] <- paste0(mean, ' (', sd, ')')
    currentRow <- nrow(output) + 1

    # Sauvegarde de l'IC de la moyenne (ne peut se faire que si la moyenne est aussi sauvegardée)
    if (confint == TRUE) {
      output[currentRow, 3] <- 'IC95% [Mean]'
      if (!is.na(sd(input[, variable], na.rm = TRUE))) {
        if (sd(input[, variable], na.rm = TRUE) != 0) {
          conf_int_mean <- numberFormat(t.test(input[, variable])$conf.int, round)
          output[currentRow, 4] <- paste0('[', conf_int_mean[1], ' ; ', conf_int_mean[2], ']')
        }
      } else {
        output[currentRow, 4] <- NA
      }
      currentRow <- nrow(output) + 1
    }
  }

  # Sauvegarde de la médiane / de Q1 ; Q3 si desc contient 'Median'
  if (any(grepl('Median', desc))) {
    output[currentRow, 3] <- 'Median [Q1 ; Q3]'
    output[currentRow, 4] <- paste0(median, ' [', quart1st, ' ; ', quart3th, ']')
    currentRow <- nrow(output) + 1
  }

  # Sauvegarde du range si desc contient 'Range'
  if (any(grepl('Range', desc))) {
    output[currentRow, 3] <- 'Min ; Max'
    output[currentRow, 4] <- paste0(min, ' ; ', max)
    currentRow <- nrow(output) + 1
  }

  # Sauvegarde du mode si desc contient 'Mode'
  if (any(grepl('Mode', desc)) & !any(grepl('\\.', as.character(input[, variable])))) {
    output[currentRow, 3] <- 'Mode'
    output[currentRow, 4] <- numberFormat(getMode(input[, variable]), round)
    currentRow <- nrow(output) + 1
  }
  return(output)
}


#' @title an internal function to do the description of factor from a data.frame
#' @description an internal function to do the description of factor from a data.frame
#' @param input a data.frame
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
statQL <- function(input, variable, round = 3, NA_asModality = FALSE) {

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
    stop("variable must be the name of a numeric variable")
  }

  if(!is.vector(round) | !is.numeric(round) | length(variable) !=1){
    stop("round must a numeric vector of length 1")
  }

  if(!is.vector(NA_asModality) | !is.logical(NA_asModality) | length(NA_asModality) !=1){
    stop("NA_asModality must be the logical vector of length 1")
  }



  # Création du tableau de sortie
  output <- data.frame(matrix(ncol = 4, nrow = 0))

  # Calcul des effectifs et des pourcentages des différentes modalitées
  size <- table(input[, variable], useNA = 'always')
  sizeWithoutNA <- table(input[, variable], useNA = 'no')
  percent <- numberFormat(prop.table(table(input[, variable], useNA = 'always')) * 100, round)
  percentWithoutNA <- numberFormat(prop.table(table(input[, variable], useNA = 'no')) * 100, round)

  # Structuration des données de résultat dans le tableau de sortie
  for (i in 1:length(size)) {
    # Initialisation du tableau de ortie
    if (i == 1) {
      output[i, 1] <- getLabelFromVariable(input[variable])
      # Si NA_asModality == FALSE, on affiche dasn l'entête le pourcentage de donnees manquantes
      if (NA_asModality) {
        output[i + 1, 2] <- 'All modalities'
        output[i + 1, 3] <- 'N (m.d.)'
        output[i + 1, 4] <- paste0(sum(sizeWithoutNA),
                                   ' (',
                                   sum(size) - sum(sizeWithoutNA),
                                   ')')
      } else {
        output[i + 1, 2] <- 'All modalities'
        output[i + 1, 3] <- 'N (m.d. ; %)'
        output[i + 1, 4] <- paste0(sum(sizeWithoutNA),
                                   ' (',
                                   sum(size) - sum(sizeWithoutNA),
                                   ' ; ',
                                   percent[length(size)],
                                   ')')
      }
    }

    # Cas où NA_asModality = TRUE et modalité actuelle is.na()
    # On affiche les résultats de calcul d'effectif et de pourcentage avec useNA = 'always'
    # On renomme la modalite NA par m.d.
    if (is.na(names(size[i])) & NA_asModality == TRUE) {
      output[i + 2, 2] <- 'm.d.'
      output[i + 2, 3] <- 'N (%)'
      output[i + 2, 4] <- paste0(size[i], ' (', percent[i], ')')
    }

    # Cas où NA_asModality = TRUE et modalité actuelle !is.na()
    # On affiche les résultats de calcul d'effectif et de pourcentage avec useNA = 'always'
    else if (!is.na(names(size[i])) & NA_asModality == TRUE) {
      output[i + 2, 2] <- names(size[i])
      output[i + 2, 3] <- 'N (%)'
      output[i + 2, 4] <- paste0(size[i], ' (', percent[i], ')')
    }

    # Cas où NA_asModality = FALSE
    # On affiche les résultats de calcul d'effectif et de pourcentage avec useNA = 'no'
    else if (!is.na(names(size[i])) & NA_asModality == FALSE) {
      output[i + 2, 2] <- names(size[i])
      output[i + 2, 3] <- 'N (%)'
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
#' @examples
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
jumpDescribeDataFrame <- function(data, variable, group = NULL, group_str = NULL,
                                  p_value = FALSE, all = FALSE, desc = c("Mean", "Median", "Range"),
                                  round = 3, confint = FALSE, NA_asModality = FALSE, NA_group_AsModality = FALSE){
  output <- statsBordeaux::createOutput()
  if(is.numeric(data[, variable])){
    output <- statsBordeaux::statsQT(output = output,
                                     input = data,
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
    output <- statsBordeaux::statsQL(output = output,
                                     input = data,
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
  return(output)
}

jumpDescribeDataFrame <- Vectorize(jumpDescribeDataFrame, vectorize.args = "variable", SIMPLIFY = FALSE, USE.NAMES = FALSE)


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
checkNormalityInternal <- function(data, variable, group = NULL, p_value = FALSE, method = 'Kolmogorov') {

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
            dataStatistic[j, 4] <- statsBordeaux::pvalFormat(kolmogorov$p.value)
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
          dataStatistic[1, 4] <- statsBordeaux::pvalFormat(kolmogorov$p.value)
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
            dataStatistic[j, 4] <- statsBordeaux::pvalFormat(shapiro$p.value)
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
          dataStatistic[1, 4] <- statsBordeaux::pvalFormat(shapiro$p.value)
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
}
checkNormalityInternal <- Vectorize(checkNormalityInternal, vectorize.args = "variable", SIMPLIFY = FALSE)
