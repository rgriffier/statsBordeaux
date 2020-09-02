#' @title Get diagnostic performance evaluation of some qualitative procedure
#' @description Get diagnostic performance evaluation of some qualitative test
#' @param data a data.frame, containing the diagnostic result
#' @param goldStandard a character vector of length one, containing the colname
#' of the gold-standard procedure
#' @param indexTest a character vector of length one, containing the colname
#' of the index procedure
#' @param M_pos a character vector, containing the levels of positive result
#' with gold-standard procedure
#' @param T_pos a character vector, containing the levels of positive result
#' with index procedure
#' @param round an integer, number of maximal decimal. Default to 3
#' @return a list contaning contengency table and performance evaluation as data.frame
#' @export
#' @import dplyr
#' @importFrom binom binom.exact
#' @examples
diag.perf <- function(data, goldStandard, indexTest, M_pos, T_pos, round = 3){

  if(!is.data.frame(data)){
    stop("data must be a data.frame.")
  }
  if(!is.vector(goldStandard) | !is.character(goldStandard) | length(goldStandard) != 1){
    stop("goldStandard must be a character vector of length one.")
  }
  if(!goldStandard %in% colnames(data)){
    stop("goldStandard must be the name of a column in data.")
  }
  if(!is.vector(indexTest) | !is.character(indexTest) | length(indexTest) != 1){
    stop("indexTest must be a character vector of length one.")
  }
  if(!indexTest %in% colnames(data)){
    stop("indexTest must be the name of a column in data.")
  }
  if(!is.vector(M_pos) | !is.character(M_pos)){
    stop("M_pos must be a character vector.")
  }
  if(!M_pos %in% data[, goldStandard]){
    stop("M_pos must be one or more value of the gold-standard column")
  }
  if(!is.vector(T_pos) | !is.character(T_pos)){
    stop("T_pos must be a character vector.")
  }
  if(!T_pos %in% data[, indexTest]){
    stop("T_pos must be one or more value of the index-test column")
  }

  ## get data perf
  dataPerf <- data %>%
    dplyr::select(one_of(goldStandard, indexTest)) %>%
    dplyr::mutate(
      `__m__` = case_when(
        get(goldStandard) %in% M_pos ~ 1,
        TRUE ~ 0
      ),
      `__t__` = case_when(
        get(indexTest) %in% T_pos ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::select(`__m__`, `__t__`)

  ## compute perf
  VP <- dataPerf %>% dplyr::filter(`__m__` == 1 & `__t__` == 1) %>% nrow()
  FN <- dataPerf %>% dplyr::filter(`__m__` == 1 & `__t__` == 0) %>% nrow()
  VN <- dataPerf %>% dplyr::filter(`__m__` == 0 & `__t__` == 0) %>% nrow()
  FP <- dataPerf %>% dplyr::filter(`__m__` == 0 & `__t__` == 1) %>% nrow()

  ## get contengency table
  contingencyTable <- data.frame(
    `M_pos` = c(VP, FN, VP + FN),
    `M_neg` = c(FP, VN, VN + FP),
    Total = c(VP + FP, FN + VN, VP + FN + VN + FP)
  )
  rownames(contingencyTable) <- c('T+', 'T-', 'Total')

  ## compute perf confidence interval
  Se <- VP/(VP + FN)
  Se_confint <- binom::binom.exact(x = VP, n = (VP + FN))[c('lower', 'upper')]
  Se_confint <- sapply(Se_confint, function(x) ifelse(x > 1, 1, x))
  Se_confint <- sapply(Se_confint, function(x) ifelse(x < 0, 0, x))

  Sp <- VN/(VN + FP)
  Sp_confint <- binom::binom.exact(x = VN, n = (VN + FP))[c('lower', 'upper')]
  Sp_confint <- sapply(Sp_confint, function(x) ifelse(x > 1, 1, x))
  Sp_confint <- sapply(Sp_confint, function(x) ifelse(x < 0, 0, x))

  VPP <- VP/(VP + FP)
  VPP_confint <- binom::binom.exact(x = VP, n = (VP + FP))[c('lower', 'upper')]
  VPP_confint <- sapply(VPP_confint, function(x) ifelse(x > 1, 1, x))
  VPP_confint <- sapply(VPP_confint, function(x) ifelse(x < 0, 0, x))

  VPN <- VN/(VN + FN)
  VPN_confint <- binom::binom.exact(x = VN, n = (VN + FN))[c('lower', 'upper')]
  VPN_confint <- sapply(VPN_confint, function(x) ifelse(x > 1, 1, x))
  VPN_confint <- sapply(VPN_confint, function(x) ifelse(x < 0, 0, x))

  RVP <- Se / (1-Sp)
  RVP.low <- exp(log(RVP) - qnorm(0.975, mean = 0, sd = 1) * sqrt((1 - Se)/((VP + FN) * Se) + (Sp)/((VN + FP) * (1 - Sp))))
  RVP.up <- exp(log(RVP) + qnorm(0.975, mean = 0, sd = 1) * sqrt((1 - Se)/((VP + FN) * Se) + (Sp)/((VN + FP) * (1 - Sp))))

  RVN <- (1-Se)/ Sp
  RVN.low <- exp(log(RVN) - qnorm(0.975, mean = 0, sd = 1) * sqrt((Se)/((VP + FN) * (1 - Se)) + (1 - Sp)/((VN + FP) * (Sp))))
  RVN.up <- exp(log(RVN) + qnorm(0.975, mean = 0, sd = 1) * sqrt((Se)/((VP + FN) * (1 - Se)) + (1 - Sp)/((VN + FP) * (Sp))))

  ## format perf confidence interval
  perfResult <- data.frame(
    Parameter = c('Sensitivity', 'Specificity', 'Positive Predictive Value', 'Negative Predictive Value', 'Likelihood Ratio Positive', 'Likelihood Ratio Negative'),
    Value = c(
      format(round(Se, round), nsmall = round),
      format(round(Sp, round), nsmall = round),
      format(round(VPP, round), nsmall = round),
      format(round(VPN, round), nsmall = round),
      format(round(RVP, round), nsmall = round),
      format(round(RVN, round), nsmall = round)
    ),
    `CI95%` = c(
      paste0('[', paste0(format(round(Se_confint, round), nsmall = round), collapse = ' ; '), ']'),
      paste0('[', paste0(format(round(Sp_confint, round), nsmall = round), collapse = ' ; '), ']'),
      paste0('[', paste0(format(round(VPP_confint, round), nsmall = round), collapse = ' ; '), ']'),
      paste0('[', paste0(format(round(VPN_confint, round), nsmall = round), collapse = ' ; '), ']'),
      paste0('[', paste0(format(round(c(RVP.low, RVP.up), round), nsmall = round), collapse = ' ; '), ']'),
      paste0('[', paste0(format(round(c(RVN.low, RVN.up), round), nsmall = round), collapse = ' ; '), ']')
    )
  )

  perfResult <- as.data.frame(t(apply(perfResult, 1, function(x){
    if(x['Value'] == 'NaN'){
      x['CI95.'] <- NA
      x['Value'] <- NA
    }
    return(x)
  })))

  return(list(contingencyTable, perfResult))
}

#' @title Display contengency table in some Rmarkdown document
#' @description Display contengency table produce with diag.perf() function in some Rmarkdown document
#' @param diag.perf a list, result of diag.perf() function
#' @return a kableExtra table
#' @export
#' @import kableExtra
#' @import dplyr
#' @examples
diag.perf.getContingencyTable <- function(diag.perf){
  if(!is.list(diag.perf) || length(diag.perf) != 2){
    stop("diag.perf must be generated by diag.perf() function")
  }
  table <- diag.perf[[1]]
  kable <- table %>%
    kableExtra::kable(booktabs = TRUE, escape = FALSE, format = "html", col.names = c('M+', 'M-', 'Total')) %>%
    kableExtra::kable_styling(bootstrap_options = c('hover', 'condensed', 'responsive'), full_width = FALSE) %>%
    kableExtra::column_spec(column = 1, bold = TRUE)
  return(kable)
}

#' @title Display performance table in some Rmarkdown document
#' @description Display performance table produce with diag.perf() function in some Rmarkdown document
#' @param diag.perf a list, result of diag.perf() function
#' @return a kableExtra table
#' @export
#' @import kableExtra
#' @import dplyr
#' @examples
diag.perf.getPerformanceTable <- function(diag.perf){
  if(!is.list(diag.perf) || length(diag.perf) != 2){
    stop("diag.perf must be generated by diag.perf() function")
  }
  table <- diag.perf[[2]]
  kable <- table %>% kableExtra::kable(booktabs = TRUE, escape = FALSE, format = "html",
                                       col.names = c('Parameters', 'Value', 'CI95%')) %>%
    kableExtra::kable_styling(bootstrap_options = c('hover', 'condensed', 'responsive'),
                              full_width = FALSE)
  return(kable)
}

#' @title Perform some kappa test between two qualitative test
#' @description Perform some kappa test between two qualitative test
#' @param data a data.frame, containing the tests' result as factor
#' @param test_1 a character vector of length one, containing the colname
#' of the test_1 procedure
#' @param test_2 a character vector of length one, containing the colname
#' of the test_2 procedure
#' @param round an integer, number of maximal decimal. Default to 3
#' @return a list contaning agreement matrix table and agreement parameters as data.frame
#' @export
#' @importFrom psych cohen.kappa
#' @import dplyr
#' @examples
diag.kappa <- function(data, test_1, test_2, round = 3){

  if(!is.data.frame(data)){
    stop("data must be a data.frame.")
  }
  if(!is.vector(test_1) | !is.character(test_1) | length(test_1) != 1){
    stop("test_1 must be a character vector of length one.")
  }
  if(!test_1 %in% colnames(data)){
    stop("test_1 must be the name of a column in data.")
  }
  if(!is.vector(test_2) | !is.character(test_2) | length(test_2) != 1){
    stop("test_2 must be a character vector of length one.")
  }
  if(!test_1 %in% colnames(data)){
    stop("test_2 must be the name of a column in data.")
  }

  dataKappa <- data %>%
    dplyr::select(one_of(test_1, test_2))

  kappa <- suppressWarnings(psych::cohen.kappa(dataKappa, alpha = 0.05))

  # get agreement matrix
  agreementMatrix <- kappa$agree*kappa$n.obs
  names(dimnames(agreementMatrix)) <- c(getVarLabel(data[test_1]),
                                        getVarLabel(data[test_2]))

  # agreement parameters
  agreement <- sum(diag(agreementMatrix))
  agreement_tx <- sum(diag(kappa$agree))
  disagreement <- kappa$n.obs - sum(diag(agreementMatrix))
  disagreement_tx <- disagreement/kappa$n.obs

  agreementParam <- data.frame(
    Parameter = c('Agreement', 'Disagreement', "Cohen's kappa"),
    Value = c(
      paste0('N = ', agreement, ' (', round(agreement_tx, round), ')'),
      paste0('N = ', disagreement, ' (', round(disagreement_tx, round), ')'),
      paste0(format(round(kappa$kappa, round), nsmall = round), ' ', paste0('CI95%[', paste0(format(round(kappa$confid['unweighted kappa', c('lower', 'upper')], round), nsmall = round), collapse = ' ; '), ']'))
    )
  )

  return(list(agreementMatrix, agreementParam))
}

#' @title Display agreement matrix table in some Rmarkdown document
#' @description Display agreement matrix table produces by diag.kappa() in some Rmarkdown document
#' @param diag.kappa a list, result of diag.kappa() function
#' @return a kableExtra table
#' @export
#' @import kableExtra
#' @import dplyr
#' @examples
diag.kappa.getAgreeentMatrix <- function(diag.kappa){
  if(!is.list(diag.kappa) || length(diag.kappa) != 2){
    stop("diag.kappa must be generated by diag.kappa() function")
  }

  table <- diag.kappa[[1]]

  name1 <- names(dimnames(table))[1]
  name2 <- names(dimnames(table))[2]

  table <- rbind(
    rep(NA, ncol(table)),
    table
  )
  row.names(table) <- c(name1, row.names(table)[2:length(row.names(table))])

  header <- c('', ncol(table))
  names(header) <- c('',name2)

  kable <- table %>%
    kableExtra::kable(booktabs = TRUE, escape = FALSE, format = "html") %>%
    kableExtra::kable_styling(bootstrap_options = c('hover', 'condensed', 'responsive'), full_width = FALSE) %>%
    kableExtra::column_spec(column = 1, bold = TRUE) %>%
    kableExtra::add_header_above(header) %>%
    kableExtra::add_indent(positions = c(2:nrow(table)))
  return(kable)
}

#' @title Display agreement parameters table in some Rmarkdown document
#' @description Display agreement parameters table produces by diag.kappa() in some Rmarkdown document
#' @param diag.kappa a list, result of diag.kappa() function
#' @return a kableExtra table
#' @export
#' @import kableExtra
#' @import dplyr
#' @examples
diag.kappa.getAgreeentPram <- function(diag.kappa){
  if(!is.list(diag.kappa) || length(diag.kappa) != 2){
    stop("diag.kappa must be generated by diag.kappa() function")
  }
  table <- diag.kappa[[2]]
  kable <- table %>%
    kableExtra::kable(booktabs = TRUE, escape = FALSE, format = "html") %>%
    kableExtra::kable_styling(bootstrap_options = c('hover', 'condensed', 'responsive'), full_width = FALSE) %>%
    kableExtra::column_spec(column = 1, bold = TRUE)
  return(kable)
}
