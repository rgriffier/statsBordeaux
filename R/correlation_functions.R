#' @title Get correlation coeficient between two numeric variable
#' @description Get correlation coeficient between two numeric variable
#' @param data a data.frame, containing the data to use to compute the correlation coeficient
#' @param var1 a character vector of length one, containing the variable name of numeric variable in data
#' @param var2 a character vector of length one, containing the variable name of numeric variable in data
#' @param method a character string indicating which correlation coefficient is to be used for the test. One of "pearson", "kendall", or "spearman".
#' @param conf.level confidence level for the returned confidence interval.
#' @param exact a logical indicating whether an exact p-value should be computed. Used for Kendall's tau and Spearman's rho.
#' @param round a numeric vector of length one, containing the number of digit for rounding
#' @return a data.frame
#' @export
#' @examples
#' data(mtcars)
#' cor <- getCorrelationCoeficient(data = mtcars, var1 = 'mpg', var2 = colnames(mtcars)[sapply(mtcars, is.numeric)])
#' cor <- do.call('rbind', cor)
getCorrelationCoeficient <- Vectorize(function(data, var1, var2, method = 'pearson', exact = NULL,conf.level = 0.95, round = 3){
  if (!is.data.frame(data) | nrow(data) == 0) {
    stop("data must be a data.frame containing some data")
  }
  if (!is.vector(var1) | !is.character(var1) | length(var1) != 1 | !var1 %in% colnames(data)) {
    stop("var1 must be a character vector of length one corresponding to some variable in data")
  }
  if (!is.vector(var2) | !is.character(var2) | length(var2) != 1 | !var2 %in% colnames(data)) {
    stop("var2 must be a character vector of length one corresponding to some variable in data")
  }
  if(!is.vector(method) | !is.character(method) | length(method) != 1 | ! method %in% c("pearson", "kendall", "spearman")){
    stop("method must be a character vector of length one containing in 'pearson', 'kendall' ou 'spearman'")
  }
  if(!is.vector(conf.level) | !is.numeric(conf.level) | length(conf.level) != 1 | conf.level < 0 | conf.level > 1){
    stop("conf.level must be a numeric vector of length one between 0 and 1")
  }
  if(!is.vector(round) | !is.numeric(round) | length(round) != 1){
    stop("round must be a numeric vector of length one")
  }
  if(var1 == var2){
    return(NULL)
  }
  cor <- cor.test(data[, var1], data[, var2], method = method, exact = exact, conf.level = conf.level)
  cor$estimate
  cor$conf.int
  conf.level <- attr(cor$conf.int,"conf.level")
  cor <- data.frame(
    Var_1 = getVarLabel(data, var1),
    Var_2 = getVarLabel(data, var2),
    Estimate = paste0(formatC(round(cor$estimate, round), format = 'f', flag='0', digits =round), ' [', paste0(formatC(round(cor$conf.int, round), format = 'f', flag='0', digits =round), collapse = ' ; '), ']'),
    method = cor$method,
    row.names = NULL
  )
  attr(cor,"conf.level") <- conf.level
  attr(cor,"stats.type") <- 'cor'
  return(cor)
}, vectorize.args = 'var2', SIMPLIFY = FALSE)

#' @title Display corelation result generate with getCorrelationCoeficient()
#' @description Display corelation result generate with getCorrelationCoeficient() as kableExtra object
#' @param data_frame a data.frame, corelation result generate with getCorrelationCoeficient()
#' @return an html data table to display
#' @export
#' @examples
#' data(mtcars)
#' cor <- getCorrelationCoeficient(data = mtcars, var1 = 'mpg', var2 = colnames(mtcars)[sapply(mtcars, is.numeric)])
#' cor <- do.call('rbind', cor)
#' addCorrelationKable(cor)
addCorrelationKable <- function(data_frame){
  if (!is.data.frame(data_frame) | nrow(data_frame) == 0) {
    stop("data_frame must be a data.frame containing some data")
  }
  options(knitr.kable.NA = "")

  table <- data_frame
  listTest <- unique(table$method[!is.na(table$method)])
  if (length(listTest) >= 1) {
    footNote <- kableExtra::footnote_marker_symbol(1:length(listTest))
    names(footNote) <- listTest
    table$`Estimate` <- apply(table, 1, function(currentRow) {
      ifelse(test = !is.na(footNote[currentRow["method"]]),
             yes = paste0(currentRow["Estimate"], "&nbsp;",
                          unname(footNote[currentRow["method"]])), no = currentRow["Estimate"])
    })
    table$method <- NULL
  }

  kable <- table %>% kableExtra::kable(booktabs = TRUE, escape = FALSE, format = "html", row.names = FALSE,
                                       col.names = c('Variable 1', 'Variable 2', paste0('Coefficient [', attr(table, 'conf.level')*100, '%CI]'))) %>%
    kableExtra::kable_styling(fixed_thead = TRUE, full_width = FALSE, bootstrap_options = c("hover", "condensed", "responsive")) %>%
    kableExtra::column_spec(column = 3, extra_css = "text-align:right;") %>%
    kableExtra::row_spec(row = 0, bold = TRUE, align = "center", extra_css = "border-bottom: 2px solid black; padding: 0px 10px 5px 10px;") %>%
    kableExtra::row_spec(row = c(1:nrow(table)), hline_after = FALSE, extra_css = "border:none; padding: 5px 5px;")
  if (length(listTest) >= 1) {
    kable <- kable %>% kableExtra::footnote(symbol = names(footNote), footnote_as_chunk = FALSE)
  }
  kable <- gsub(pattern = "<tbody>", x = kable, replacement = "<tbody style=\"border-bottom: 1px solid black; border-top: 1px solid black;border-collapse: collapse;\">")
  kable <- gsub(pattern = "<table class=\"table table-hover table-condensed table-responsive\" style=\"width: auto !important; margin-left: auto; margin-right: auto;\">",
                x = kable, replacement = "<table class=\"table table-hover table-condensed table-responsive\" style=\"width: auto !important; margin-left: auto; margin-right: auto; border-top: 1px solid black;border-collapse: collapse;\">")

  return(kable)
}
