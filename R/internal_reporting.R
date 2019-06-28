#' @title An internal method to manage the display of data.frame header generate by statsBordeaux package
#' @description An internal method to manage the display of data.frame header generate by statsBordeaux package
#' @param table a data.frame containing description data generate by statsBordeaux package
#' @param group the name of the grouping variable
#' @param displayTestName a boolean, if TRUE, testName will be displayed
#' @return a data.frame ready o be transforme as data.table
#' @noRd
#' @import officer
#' @import magrittr
#' @import flextable
#' @import devEMF
displayVarNameInHeaderTable <- function(table, group, displayTestName) {

  # Prétraitement
  # Suppression de la colonne modality si vide
  if(is.element('Modality', colnames(table))){
    if(sum(!is.na(table['Modality'])) == 0){
      table['Modality'] <- NULL
    }
  }
  if(is.element('Test', colnames(table))){
    if(!displayTestName){
      table['Test'] <- NULL
    }
  }
  if(is.element('All', colnames(table))){
    if(sum(!is.na(table['All'])) == 0){
      table['All'] <- NULL
    }
  }
  if(is.element('p-value', colnames(table))){
    if(sum(!is.na(table['p-value'])) == 0){
      table['p-value'] <- NULL
    }
  }


  colnamesInputTable <- colnames(table)
  modalityColumnIndexMainVariable <- which(colnamesInputTable == 'Variable' |
                                             colnamesInputTable == 'Modality' |
                                             colnamesInputTable == 'Description' |
                                             colnamesInputTable == 'Test' |
                                             colnamesInputTable == 'p-value')

  colnamesOutput_B <- rep(group, length(colnamesInputTable))
  colnamesOutput_B[modalityColumnIndexMainVariable] <- colnamesInputTable[modalityColumnIndexMainVariable]
  colnamesOutput_B[colnamesOutput_B == 'Variable' | colnamesOutput_B == 'Modality' | colnamesOutput_B == 'Description'] <- NA

  colnamesOutput_A <- colnamesInputTable
  colnamesOutput_A[colnamesOutput_A == 'Variable' | colnamesOutput_A == 'Modality' | colnamesOutput_A == 'Description'] <- NA

  df <- data.frame(col_keys = colnamesInputTable,
                   colB = colnamesOutput_B,
                   colA = colnamesOutput_A,
                   stringsAsFactors = FALSE)
  return(df)
}


#' @title An internal method to generate a regular table based on data.frame generate by statsBordeaux package
#' @description An internal method to generate a regular table based on data.frame generate by statsBordeaux package
#' @param table a data.frame containing description data generate by statsBordeaux package
#' @param displayTestName a boolean, if TRUE, testName will be displayed
#' @param modalitySize double, size in inch of the modality column
#' @param descriptionSize double, size in inch of the description column
#' @param valueSize double, size in inch of the value column
#' @param testSize double, size in inch of the test column
#' @return a regularTable
#' @noRd
displayTable <- function(table, displayTestName, modalitySize, descriptionSize, valueSize, testSize) {
  # Prétraitement
  # Suppression de la colonne modality si vide
  # if(is.element('Modality', colnames(table))){
  #   if(sum(!is.na(table['Modality'])) == 0){
  #     table['Modality'] <- NULL
  #   }
  # }
  if(is.element('Test', colnames(table))){
    if(!displayTestName){
      table['Test'] <- NULL
    }
  }
  if(is.element('All', colnames(table))){
    if(sum(!is.na(table['All'])) == 0){
      table['All'] <- NULL
    }
  }
  if(is.element('p-value', colnames(table))){
    if(sum(!is.na(table['p-value'])) == 0){
      table['p-value'] <- NULL
    }
  }

  for(i in which(table[1] != '')){
    varName <- table[i, 1]
    successive <- TRUE
    for(j in 2 : ncol(table[i, ])){
      if(is.na(table[i, j]) & successive == TRUE){
        table[i, j] <- varName
      } else {
        successive <- FALSE
      }
    }
  }


  # Les NA sont remplacées par des ''
  table[is.na(table)] <- ''
  table[table == 'NaN (NA)'] <- ''
  table[table == 'NA [NA ; NA]'] <- ''
  table[table == 'NA ; NA'] <- ''
  table[table == '0 (NaN)'] <- ''


  # Compute regulartable
  outputRT <- regulartable(table)
  outputRT <- border_remove(outputRT)
  outputRT <- font(outputRT, fontname = "Calibri", part = 'all')
  big_border <- fp_border(color='black', width = 1)

  # Header
  outputRT <- set_header_labels(outputRT, Variable = NA, Modality = NA, Description = NA)
  if(!is.null(attributes(table)$label_var_group)){
    colnameHeader <- displayVarNameInHeaderTable(table = table, group = attributes(table)$label_var_group, displayTestName = displayTestName)
    outputRT <- set_header_df(outputRT, mapping = colnameHeader, key = "col_keys")
    outputRT <- hline(outputRT, j = which(!is.na(colnameHeader[2])), border = big_border, part = 'header')
    outputRT <- merge_h(outputRT, part = 'header')
    outputRT <- merge_v(outputRT, part = 'header')
  }
  outputRT <- align(outputRT, align = 'center', part = 'header')
  outputRT <- bold(outputRT, part = 'header')
  outputRT <- hline_top(outputRT, border = big_border, part = 'header')
  outputRT <- hline_bottom(outputRT, border = big_border, part = 'header')
  outputRT <- fontsize(outputRT, size = 11, part = 'header')
  outputRT <- font(outputRT, fontname = 'Calibri', part = 'header')

  # Body
  outputRT <- align(outputRT, j = c(grep('Variable', colnames(table)):grep('Description', colnames(table))),
                    align = 'left', part = 'body')
  outputRT <- align(outputRT, j = grep('Description', colnames(table)), align = 'right', part = 'body')
  if(is.element('Test', colnames(table))){
    outputRT <- align(outputRT, j = 'Test', align = 'left', part = 'body')
  }
  outputRT <- bg(outputRT, i = which(table[1] != ''), bg = '#f2f2f2', part = 'body')
  outputRT <- merge_h(outputRT, i = which(table[1] != ''))
  outputRT <- bold(outputRT, i = which(table[1] != ''), j = 1, part = 'body')
  outputRT <- hline_bottom(outputRT, border = big_border, part = 'body' )
  outputRT <- merge_v(outputRT, j = 3)
  outputRT <- fontsize(outputRT, size = 10, part = 'body')
  outputRT <- merge_h(outputRT, i = grep('Mean difference \\[IC95%\\]', table[,'Description']), part = 'body')

  # Autofit
  #outputRT <- autofit(outputRT)
  outputRT <- width(outputRT, width = valueSize)
  outputRT <- width(outputRT, j = 1, width = 0.2)

  if(length((grep('Modality', colnames(table)))) != 0){
    outputRT <- width(outputRT, j = c(grep('Modality', colnames(table))), width = modalitySize)
  }

  if(length((grep('Description', colnames(table)))) != 0){
    outputRT <- width(outputRT, j = c(grep('Description', colnames(table))), width = descriptionSize)
  }

  if(length((grep('Test', colnames(table)))) != 0){
    outputRT <- width(outputRT, j = c(grep('Test', colnames(table))), width = testSize)
  }

  if(length((grep('p-value', colnames(table)))) != 0){
    outputRT <- width(outputRT, j = c(grep('p-value', colnames(table))), width = 0.6)
  }

  outputRT <- height(outputRT, height = 0.2)

  return(outputRT)
}


#' @title An internal method to generate a regular table based on data.frame generate by regression function from statsBordeaux package
#' @description An internal method to generate a regular table based on data.frame generate by regression function from statsBordeaux package
#' @param table a data.frame containing regression data generate by statsBordeaux package
#' @param modalitySize double, size in inch of the modality column
#' @param sampleSize double, size in inch of the sample size column
#' @param parameterSize double, size in inch of the parameter column
#' @param confintSize double, size in inch of the confidnece interval column
#' @return a regularTable
#' @noRd
displayRegressionTable <- function(table, modalitySize, sampleSize, parameterSize, confintSize) {
  for(i in which(table[1] != '')){
    varName <- table[i, 1]
    successive <- TRUE
    for(j in 2 : ncol(table[i, ])){
      if(is.na(table[i, j]) & successive == TRUE){
        table[i, j] <- varName
      } else {
        successive <- FALSE
      }
    }
  }

  # Les NA sont remplacées par des ''
  table[is.na(table)] <- ''
  table[table == 'NaN (NA)'] <- ''
  table[table == 'NA [NA ; NA]'] <- ''
  table[table == 'NA ; NA'] <- ''
  table[table == '0 (NaN)'] <- ''

  # Compute regulartable
  outputRT <- flextable::regulartable(table)
  outputRT <- flextable::border_remove(outputRT)
  outputRT <- flextable::font(outputRT, fontname = "Calibri", part = 'all')
  big_border <- officer::fp_border(color='black', width = 1)

  # Header
  outputRT <- flextable::set_header_labels(outputRT, Variable = NA, Modality = NA)
  outputRT <- flextable::align(outputRT, align = 'right', part = 'header')
  outputRT <- flextable::bold(outputRT, part = 'header')
  outputRT <- flextable::hline_top(outputRT, border = big_border, part = 'header')
  outputRT <- flextable::hline_bottom(outputRT, border = big_border, part = 'header')
  outputRT <- flextable::fontsize(outputRT, size = 11, part = 'header')
  outputRT <- flextable::font(outputRT, fontname = 'Calibri', part = 'header')

  # Body
  outputRT <- flextable::align(outputRT, j = c(grep('Variable', colnames(table)):grep('N', colnames(table))),
                               align = 'left', part = 'body')
  outputRT <- flextable::align(outputRT, j = c(grep('N', colnames(table)):length(colnames(table))),
                               align = 'right', part = 'body')
  outputRT <- flextable::bg(outputRT, i = which(table[1] != ''), bg = '#f2f2f2', part = 'body')
  outputRT <- flextable::merge_h(outputRT, i = which(table[1] != ''))
  outputRT <- flextable::bold(outputRT, i = which(table[1] != ''), j = 1, part = 'body')
  outputRT <- flextable::hline_bottom(outputRT, border = big_border, part = 'body' )
  outputRT <- flextable::fontsize(outputRT, size = 10, part = 'body')

  # Autofit
  outputRT <- flextable::width(outputRT, width = 2)
  outputRT <- flextable::width(outputRT, j = 1, width = 0.2)

  if(length((grep('Modality', colnames(table)))) != 0){
    outputRT <- flextable::width(outputRT, j = c(grep('Modality', colnames(table))), width = modalitySize)
  }

  if(length((grep('N', colnames(table)))) != 0){
    outputRT <- flextable::width(outputRT, j = c(grep('Modality', colnames(table))), width = sampleSize)
  }

  if(length((grep('(Beta)|(OR)', colnames(table)))) != 0){
    outputRT <- flextable::width(outputRT, j = c(grep('Modality', colnames(table))), width = parameterSize)
  }

  if(length((grep('95%CI', colnames(table)))) != 0){
    outputRT <- flextable::width(outputRT, j = c(grep('Modality', colnames(table))), width = confintSize)
  }

  if(length((grep('p-value', colnames(table)))) != 0){
    outputRT <- flextable::width(outputRT, j = c(grep('p-value', colnames(table))), width = 1)
  }

  outputRT <- flextable::height(outputRT, height = 0.2)

  return(outputRT)
}

#' @title An internal method to generate a regular table based on data.frame containing description of i2b2data
#' @description An internal method to generate a regular table based on data.frame containing description of i2b2data
#' @param table a data.table
#' @return a regular table
#' @noRd
displayDataDescriptionTable <- function(table) {

  # Prétraitement
  # Suppression de la colonne SUBSUBCHAPTER / SUBCHAPTER si vide
  if(is.element('SUBSUBCHAPTER', colnames(table))){
    if(sum(!is.na(table['SUBSUBCHAPTER'])) == 0){
      table['SUBSUBCHAPTER'] <- NULL
    }
  }
  if(is.element('SUBCHAPTER', colnames(table))){
    if(sum(!is.na(table['SUBCHAPTER'])) == 0){
      table['SUBCHAPTER'] <- NULL
    }
  }

  for(i in which(table[1] != '')){
    varName <- table[i, 1]
    successive <- TRUE
    for(j in 2 : ncol(table[i, ])){
      if(is.na(table[i, j]) & successive == TRUE){
        table[i, j] <- varName
      } else {
        successive <- FALSE
      }
    }
  }

  # Les NA sont remplacées par des ''
  table[is.na(table)] <- ''

  # Compute regulartable
  outputRT <- regulartable(table)
  outputRT <- border_remove(outputRT)
  outputRT <- font(outputRT, fontname = "Calibri", part = 'all')
  big_border <- fp_border(color='black', width = 1)

  # Header
  outputRT <- set_header_labels(outputRT, CHAPTER = NA, SUBCHAPTER = NA, SUBSUBCHAPTER = NA,
                                OBSERVATION_COUNT = "Observations (N)")
  if(any(grepl("ENCOUNTER", colnames(table)))){
    outputRT <- set_header_labels(outputRT, DISTINCT_ENCOUNTER_NUM_COUNT = "Venues distinctes (N)",
                                  DISTINCT_ENCOUNTER_NUM_PERCENT = "Venues distinctes (%)")
  } else if(any(grepl("PATIENT", colnames(table)))){
    outputRT <- set_header_labels(outputRT, DISTINCT_PATIENT_NUM_COUNT = "Patients distincts (N)",
                                  DISTINCT_PATIENT_NUM_PERCENT = "Patients distincts (%)")
  }

  outputRT <- align(outputRT, align = 'center', part = 'header')
  outputRT <- bold(outputRT, part = 'header')
  outputRT <- hline_top(outputRT, border = big_border, part = 'header')
  outputRT <- hline_bottom(outputRT, border = big_border, part = 'header')
  outputRT <- fontsize(outputRT, size = 11, part = 'header')
  outputRT <- font(outputRT, fontname = 'Calibri', part = 'header')

  # Body
  outputRT <- align(outputRT, j = c(grep('^CHAPTER$', colnames(table)):grep('^SUBSUBCHAPTER$', colnames(table))),
                    align = 'left', part = 'body')
  outputRT <- align(outputRT, j = c(grep('^OBSERVATION_COUNT$', colnames(table)):length(colnames(table))), align = 'right', part = 'body')
  outputRT <- merge_h(outputRT, i = which(table[1] != ''))
  outputRT <- merge_h(outputRT, i = which(table[2] != ''))
  outputRT <- bold(outputRT, i = which(table[1] != ''), j = 1, part = 'body')
  outputRT <- hline_bottom(outputRT, border = big_border, part = 'body' )
  outputRT <- fontsize(outputRT, size = 10, part = 'body')

  # Autofit
  #outputRT <- autofit(outputRT)
  if(ncol(table) == 6){
    outputRT <- width(outputRT, j = 1, width = 0.2)
    outputRT <- width(outputRT, j = 2, width = 0.2)
    outputRT <- width(outputRT, j = 3, width = 2.5)
    outputRT <- width(outputRT, j = 4, width = 1.5)
    outputRT <- width(outputRT, j = 5, width = 1.5)
    outputRT <- width(outputRT, j = 6, width = 1.5)
  } else if(ncol(table) == 5){
    outputRT <- width(outputRT, j = 1, width = 0.2)
    outputRT <- width(outputRT, j = 2, width = 2.5)
    outputRT <- width(outputRT, j = 3, width = 1.5)
    outputRT <- width(outputRT, j = 4, width = 1.5)
    outputRT <- width(outputRT, j = 5, width = 1.5)
  } else if(ncol(table) == 4){
    outputRT <- width(outputRT, j = 1, width = 2.5)
    outputRT <- width(outputRT, j = 2, width = 1.5)
    outputRT <- width(outputRT, j = 3, width = 1.5)
    outputRT <- width(outputRT, j = 4, width = 1.5)
  }

  outputRT <- height(outputRT, height = 0.2)

  return(outputRT)
}
