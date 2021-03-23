#' @title A convenient method to add kable into a RMarkdown document
#' @description  A convenient method to add kable into a RMarkdown document
#' @param data_frame a data.frame, containing the data to add
#' @param var.width a numeric vector of length one, containing the witdh of the variable column
#' @return a knitr::kable
#' @export
#' @import dplyr
#' @import knitr
#' @importFrom kableExtra footnote_marker_symbol kable kable_styling row_spec column_spec footnote add_header_above
#' @importFrom stringr str_wrap
#' @examples
# data(mtcars)
# output <- statsQT(data = mtcars, variable = "mpg")
# addKable(output)
addKable <- function(data_frame, all_before = FALSE, var.width = 50){

  if(!is.data.frame(data_frame) | nrow(data_frame) == 0){
    stop("data_frame must be a data.frame containing some data")
  }
  if(!is.vector(all_before) | !is.logical(all_before) | length(all_before) != 1){
    stop("all_before must be a boolean vector of length one")
  }
  if(!is.vector(var.width) | !is.numeric(var.width) | length(var.width) != 1){
    stop("var.width must be a numeric vector of length one")
  }

  ## knitr option
  options(knitr.kable.NA = '')

  ## order all before group
  if(all_before){
    data_frame <- data_frame %>%
      dplyr::select(Variable, Modality, Description, All, dplyr::everything())
  }

  containsQualitativeVariable <- any(!is.na(data_frame$Modality))

  ## manage '0 (NaN)', 'NaN (NA)',  'NA [NA ; NA]' 	, 'NA ; NA'
  data_frame[data_frame == "0 (NaN)"] <- "\\-"
  data_frame[data_frame == "NaN (NA)"] <- "\\-"
  data_frame[data_frame == "NA [NA ; NA]"] <- "\\-"
  data_frame[data_frame == "NA ; NA"] <- "\\-"
  data_frame[] <- lapply(data_frame, function(x) gsub(pattern = '\\(NA\\)$', replacement = "\\(\\-\\)", x = x, ignore.case = FALSE))

  # replace Variable by concat of Variable and Modality
  varRowsIndex <- which(!is.na(data_frame$Variable))
  data_frame$Variable <- stringr::str_replace_all(string = stringr::str_wrap(data_frame$Variable, width = var.width), pattern = '\n', replacement = '<br>')
  data_frame$Modality <- stringr::str_replace_all(string = stringr::str_wrap(data_frame$Modality, width = var.width), pattern = '\n', replacement = '<br>')
  data_frame$Modality <- ifelse(test = !is.na(data_frame$Modality) & data_frame$Modality %in% getAllModailties_text(getOption('lang.value')),
                                yes = paste0(data_frame$Description),
                                no = data_frame$Modality)
  data_frame$Variable <- ifelse(test = is.na(data_frame$Modality),
                                yes = data_frame$Variable,
                                no = paste0("<p style=\"text-indent:20px;\">", data_frame$Modality, "</p>"))
  data_frame$Variable <- ifelse(test = is.na(data_frame$Variable),
                                yes = paste0("<p style=\"text-indent:20px;\">", data_frame$Description, "</p>"),
                                no = data_frame$Variable)
  data_frame$Modality <- NULL
  data_frame$Description <- NULL

  if(!is.null(attributes(data_frame)$var_group)){
    table <- data_frame
  } else {
    table <- data_frame
  }

  ## footer management
  listTest <- unique(table$Test[!is.na(table$Test) & !table$Test %in% c("", getSmallSample_text(getOption('lang.value')), getVariabililtyNeeded_text(getOption('lang.value')))])
  if(length(listTest) >= 1){
    footNote <- kableExtra::footnote_marker_symbol(1:length(listTest))
    names(footNote) <- listTest
    table$`p-value` <- apply(table, 1, function(currentRow){
      ifelse(test = !is.na(footNote[currentRow["Test"]]),
             yes =  paste0(currentRow["p-value"], "&nbsp;", unname(footNote[currentRow["Test"]])),
             no = currentRow["p-value"])
    })
    table$Test <- NA
  }

  if(!any(!is.na(table$All))){
    table$All <- NULL
  }
  if(!any(!is.na(table$Test))){
    table$Test <- NULL
  }
  if(!any(!is.na(table$`p-value`))){
    table$`p-value` <- NULL
  }

  ## manage col.names
  beforeIndex <- which(colnames(table) %in% c("Variable", "Description"))
  col.names <- c(rep(" ", length(beforeIndex)), colnames(table)[-beforeIndex])

  ## get kable
  kable <- table %>%
    kableExtra::kable(booktabs = TRUE, escape = FALSE, format = "html", col.names = col.names) %>%
    kableExtra::kable_classic(full_width = FALSE,
                              html_font = 'Cambria'
                              # fixed_thead = TRUE,
                              # bootstrap_options = c("hover", "condensed", "responsive")
                              ) %>%
    kableExtra::row_spec(row = 0, bold = TRUE, align = "center",
                         extra_css = "padding: 0px 10px 5px 10px;") %>%
    kableExtra::column_spec(column = which(!colnames(table) %in% c("Variable", "Description", "Test")),
                            extra_css = "text-align:right;") %>%
    kableExtra::row_spec(row = varRowsIndex, bold = TRUE) %>%
    kableExtra::row_spec(row = c(1:nrow(table)), hline_after = FALSE, extra_css = "border:none; padding: 0px 0px;")

  ## add footer if some test
  if(length(listTest) >= 1){
    kable <- kable %>%
      kableExtra::footnote(symbol_title = paste0(getTest_text(getOption('lang.value')), "\n"),
                           symbol = names(footNote),
                           footnote_as_chunk = FALSE)
  }

  ## add footer if qualitative data
  if(containsQualitativeVariable){
    kable <- kable %>%
      kableExtra::footnote(general_title = paste0(getNote_text(getOption('lang.value')), "\n"),
                           general = paste0(
                             getSampleSizeNote_text(getOption('lang.value')), ' ; ',
                             getMissingDataNote_text(getOption('lang.value')), ' ; ',
                             getQualitativeDataNote_text(getOption('lang.value'))),
                           footnote_as_chunk = TRUE)
  } else {
    kable <- kable %>%
      kableExtra::footnote(general_title = paste0(getNote_text(getOption('lang.value')), "\n"),
                           general = paste0(getSampleSizeNote_text(getOption('lang.value')), ' ; ',
                                            getMissingDataNote_text(getOption('lang.value'))),
                           footnote_as_chunk = TRUE)
  }

  ## manage header
  if(!is.null(attributes(data_frame)$var_group)){
    labelVar <- attributes(data_frame)$label_var_group
    if(is.null(labelVar)){
      labelVar <- attributes(data_frame)$var_group
    }
    nbBefore <- length(which(colnames(table) %in% c("Variable", "Description")))
    nbAfter <- length(which(colnames(table) %in% c("p-value")))
    nbVar <- length(which(!colnames(table) %in% c("Variable", "Description", "Test", "p-value")))
    namedVector <- c(nbBefore, nbVar, nbAfter)
    names(namedVector) <- c(" ", labelVar, " ")
    kable <- kable %>%
      kableExtra::add_header_above(header = namedVector,
                                   bold = TRUE,
                                   extra_css = "border-bottom: 1px solid black; padding: 0px 10px 0px 10px;")
  }

  kable <- gsub(pattern = '<tbody>',
                x = kable,
                replacement = '<tbody style="border-bottom: 1px solid black; border-top: 1px solid black;border-collapse: collapse;">')

  # kable <- gsub(pattern = '<table class="table table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">',
  #               x = kable,
  #               replacement = '<table class="table table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto; border-top: 1px solid black;border-collapse: collapse;">')

  return(kable)
}

#' @title A convenient method to add regression kable into a RMarkdown document
#' @description A convenient method to add regression kable into a RMarkdown document
#' @param data_frame a data.frame, containing the data to add
#' @return a knitr::kable
#' @export
#' @import dplyr
#' @import knitr
#' @importFrom kableExtra kable kable_styling row_spec column_spec
addRegressionKable <- function(data_frame){
  if(!is.data.frame(data_frame) | nrow(data_frame) == 0){
    stop("data_frame must be a data.frame containing some data")
  }

  ## knitr option
  options(knitr.kable.NA = '')
  table <- data_frame

  # get variable index row
  varIndexRow <- which(!is.na(table$Variable))

  # replace Variable by concat of Variable and Modality
  table$Variable <- ifelse(test = is.na(table$Modality),
                           yes = table$Variable,
                           no = paste0("<p style=\"text-indent:20px;\">", table$Modality, "</p>"))
  table$Modality <- NULL

  if(any(grepl(pattern = "Beta", x = colnames(table)))){
    table$`95%CI[Beta]` <- gsub(pattern = " ", replacement = "&nbsp;", x = table$`95%CI[Beta]`)
  } else {
    table$`95%CI[OR]` <- gsub(pattern = " ", replacement = "&nbsp;", x = table$`95%CI[OR]`)
  }
  table$`p-value` <- gsub(pattern = " ", replacement = "&nbsp;", x = table$`p-value`)

  ## get kable
  kable <- table %>%
    kableExtra::kable(booktabs = TRUE, escape = FALSE, format = "html") %>%
    kableExtra::kable_styling(fixed_thead = TRUE, full_width = FALSE,
                              bootstrap_options = c("hover", "condensed", "responsive")) %>%
    kableExtra::row_spec(row = 0, bold = TRUE, align = "center", extra_css = "border-bottom: 2px solid black;") %>%
    kableExtra::row_spec(row = c(1:nrow(table)), hline_after = FALSE, extra_css = "border:none; padding-top: 0px; padding-bottom: 0px;") %>%
    kableExtra::column_spec(column = which(!colnames(table) %in% c("Variable")), extra_css = "text-align:right;") %>%
    kableExtra::row_spec(row = varIndexRow, bold = TRUE)

  # kable <- gsub(pattern = '<table class="table table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">',
  #               x = kable,
  #               replacement = '<table class="table table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto; border-bottom: 1px solid black; border-top: 1px solid black;border-collapse: collapse;">')

  return(kable)
}
