#' @title A convenient method to add kable into a RMarkdown documet
#' @description  A convenient method to add kable into a RMarkdown documet
#' @param data_frame a data.frame, containing the data to add
#' @return a knitr::kable
#' @export
#' @import dplyr
#' @import knitr
#' @import kableExtra
#' @examples
#' data(mtcars)
#' output <- createOutput()
#' output <- statsQT(output, mtcars, "mpg")
#' addKable(output)
addKable <- function(data_frame){

  if(!is.data.frame(data_frame) | nrow(data_frame) == 0){
    stop("data_frame must be a data.frame containing some data")
  }

  ## knitr option
  options(knitr.kable.NA = '')

  ## manage '0 (NaN)', 'NaN (NA)',  'NA [NA ; NA]' 	, 'NA ; NA'
  data_frame[data_frame == "0 (NaN)"] <- "\\-"
  data_frame[data_frame == "NaN (NA)"] <- "\\-"
  data_frame[data_frame == "NA [NA ; NA]"] <- "\\-"
  data_frame[data_frame == "NA ; NA"] <- "\\-"
  data_frame[] <- lapply(data_frame, function(x) gsub(pattern = '\\(NA\\)$', replacement = "\\(\\-\\)", x = x, ignore.case = FALSE))

  ## manage variable label as pack_rows
  varRowsIndex <- which(!is.na(data_frame$Variable))
  varLabel <- data_frame$Variable[varRowsIndex]
  groupLength <- diff(varRowsIndex)
  groupLength <- c(groupLength, nrow(data_frame) - sum(groupLength))

  if(!is.null(attributes(data_frame)$var_group)){
    labelVar <- data_frame$Variable[varRowsIndex]
    names(groupLength) <- varLabel
    table <- data_frame %>%
      dplyr::select(-Variable)
  } else {
    groupLength <- groupLength - 1
    labelVar <- data_frame$Variable[varRowsIndex]
    names(groupLength) <- varLabel
    table <- data_frame %>%
      dplyr::filter(is.na(Variable)) %>%
      dplyr::select(-Variable)
  }

  ## footer management
  listTest <- unique(table$Test[!is.na(table$Test) & !table$Test %in% c("", "Sample size not large enough to perform test")])
  if(length(listTest) >= 1){
    footNote <- kableExtra::footnote_marker_symbol(1:length(listTest))
    names(footNote) <- listTest
    table$`p-value` <- apply(table, 1, function(currentRow){
      ifelse(test = !is.na(footNote[currentRow["Test"]]),
             yes =  paste0(currentRow["p-value"], " ", unname(footNote[currentRow["Test"]])),
             no = currentRow["p-value"])
    })
    table$Test <- NA
  }

  ## manage empty column
  if(!any(!is.na(table$Modality))){
    table$Modality <- NULL
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
  beforeIndex <- which(colnames(table) %in% c("Modality", "Description"))
  if(length(colnames(table)[-beforeIndex]) == 1){
    col.names <- c(rep(" ", length(beforeIndex) + 1))
  } else {
    col.names <- c(rep(" ", length(beforeIndex)), colnames(table)[-beforeIndex])
  }

  ## get kable
  kable <- table %>%
    kableExtra::kable(booktabs = TRUE, escape = FALSE, format = "html", col.names = col.names) %>%
    kableExtra::kable_styling(fixed_thead = TRUE, full_width = FALSE) %>%
    kableExtra::row_spec(row = 0, bold = T, align = "center") %>%
    kableExtra::row_spec(row = c(1:nrow(table)), hline_after = FALSE, extra_css = "border:none;") %>%
    kableExtra::column_spec(column = which(!colnames(table) %in% c("Modality", "Description", "Test")), extra_css = "text-align:right;") %>%
    kableExtra::pack_rows(index = groupLength, label_row_css = "background-color:#EBEBEB;")

  ## add footer if some test
  if(length(listTest) >= 1){
    kable <- kable %>% kableExtra::footnote(symbol = names(footNote))
  }

  ## manage header
  if(!is.null(attributes(data_frame)$var_group)){
    labelVar <- attributes(data_frame)$label_var_group
    if(is.null(labelVar)){
      labelVar <- attributes(data_frame)$var_group
    }
    nbBefore <- length(which(colnames(table) %in% c("Modality", "Description")))
    nbAfter <- length(which(colnames(table) %in% c("Test", "p-value")))
    nbVar <- length(which(!colnames(table) %in% c("Modality", "Description", "Test", "p-value")))
    namedVector <- c(nbBefore, nbVar, nbAfter)
    names(namedVector) <- c(" ", labelVar, " ")
    kable <- kable %>% kableExtra::add_header_above(namedVector)
  }

  return(kable)
}
