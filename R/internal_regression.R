#' @title Get formula based on dependentVariable and independentVariables names
#' @description Get formula based on dependentVariable and independentVariables names
#' @param dependentVariable a character vector of length one, containing the colname of the dependant variable
#' @param independentVariables a character vector, containing the colnames of the dependant variables
#' @return a formula vector
#' @noRd
getFormulaFromVariableName <- function(dependentVariable, independentVariables) {
  formula <- formula(paste0(c(dependentVariable,
                              paste0(c(1, independentVariables), collapse = ' + ')),
                            collapse = ' ~ '))
  return(formula)
}


#' @title Extact from linear model fit data.frame with meaning result
#' @description Extact from linear model fit data.frame with meaning result
#' @param fit a fit
#' @param data a data.frame, containing the data used in the model
#' @param independentVariable a character vector of length one, containing the name of the independent variable
#' @param round a numeric vector of length one, containing the round value to apply
#' @return a data.frame, containing the result of the regression
#' @noRd
extractDataFromUnivariableLinearModel <- function(fit, data, independentVariable, independentVariableName, round = 2){

  ## get summary fit
  summaryFit <- tryCatch({
    summary(fit)
  }, warning = function(w) {
    "Convergence issue"
  }, error = function(e) {
    "Convergence issue"
  })

  ## get confint
  confint <- tryCatch({
    as.data.frame(confint(fit))
  }, warning = function(w) {
    "Convergence issue"
  }, error = function(e) {
    "Convergence issue"
  })

  ## case with convergence issue
  if("Convergence issue" %in% confint | "Convergence issue" %in% summaryFit){
    summaryFitData <- data.frame(Variable = independentVariable,
                                 Modality = NA,
                                 N = NA,
                                 Beta = NA,
                                 IC95_Beta = NA,
                                 p_value = confint,
                                 stringsAsFactors = FALSE)
    colnames(summaryFitData) <- c("Variable", "Modality", "N", "Beta", "95%CI[Beta]", "p-value")
    return(summaryFitData)
  }

  ## extract coefficient from summary fit and cbind it with confint
  summaryFitData <- as.data.frame(summaryFit$coefficients)
  summaryFitData <- cbind(summaryFitData, confint)
  summaryFitData <- summaryFitData[c(2:nrow(summaryFitData)), ]

  ## create the output
  ## case where independentVariable is factor
  if(is.factor(data[, independentVariable])){

    Variable <- c(independentVariableName, rep(NA, nrow(summaryFitData) + 1))
    Modality = c(NA, paste(levels(data[, independentVariable])[1], "(Ref.)"),
                 gsub(pattern = independentVariable, replacement = "", x = rownames(summaryFitData)))
    N = c(length(summaryFit$residuals), rep(NA, nrow(summaryFitData)+1))
    Beta = setFormatToNumber(c(NA, NA, summaryFitData$Estimate), round = round)
    IC95_Beta = c(NA, NA, paste0("[", setFormatToNumber(summaryFitData$`2.5 %`,  round = round), " ; " ,
                                 setFormatToNumber(summaryFitData$`97.5 %`,  round = round), "]"))
    p_value =  c(setFormatToPvalue(anova(fit)$'Pr(>F)'[1], round = round),
                 rep(NA, nrow(summaryFitData)+1))

    summaryFitData <- data.frame(Variable = Variable,
                                 Modality = Modality,
                                 N = N,
                                 Beta = Beta,
                                 IC95_Beta = IC95_Beta,
                                 p_value = p_value,
                                 stringsAsFactors = FALSE)

  }
  ## case where independentVariable is numeric
  else if(is.numeric(data[, independentVariable])){
    summaryFitData <- data.frame(Variable = independentVariableName,
                                 Modality = NA,
                                 N = length(summaryFit$residuals),
                                 Beta = setFormatToNumber(summaryFitData$Estimate, round = round),
                                 IC95_Beta = paste0("[", setFormatToNumber(summaryFitData$`2.5 %`, round = round), " ; " ,
                                                    setFormatToNumber(summaryFitData$`97.5 %`, round = round), "]"),
                                 p_value =  setFormatToPvalue(anova(fit)$'Pr(>F)'[1], round = round),
                                 stringsAsFactors = FALSE)
  }

  colnames(summaryFitData) <- c("Variable", "Modality", "N", "Beta", "95%CI[Beta]", "p-value")
  return(summaryFitData)
}


#' @title Extact from logistic model fit data.frame with meaning result
#' @description Extact from logistic model fit data.frame with meaning result
#' @param fit a fit
#' @param data a data.frame, containing the data used in the model
#' @param independentVariable a character vector of length one, containing the name of the independent variable
#' @param round a numeric vector of length one, containing the round value to apply
#' @return a data.frame, containing the result of the regression
#' @noRd
extractDataFromUnivariableLogisticModel <- function(fit, data, independentVariable, independentVariableName, round = 2){

  ## get summary fit
  summaryFit <- tryCatch({
    summary(fit)
  }, warning = function(w) {
    "Convergence issue"
  }, error = function(e) {
    "Convergence issue"
  })

  ## get confint
  confint <- tryCatch({
    as.data.frame(exp((suppressMessages(confint(fit)))))
  }, warning = function(w) {
    "Convergence issue"
  }, error = function(e) {
    "Convergence issue"
  })

  ## case with convergence issue
  if("Convergence issue" %in% confint | "Convergence issue" %in% summaryFit){
    summaryFitData <- data.frame(Variable = independentVariableName,
                                 Modality = NA,
                                 N = NA,
                                 OR = NA,
                                 IC95_OR = NA,
                                 p_value = confint,
                                 stringsAsFactors = FALSE)
    colnames(summaryFitData) <- c("Variable", "Modality", "N", "OR", "95%CI[OR]", "p-value")
    return(summaryFitData)
  }

  ## extract coefficient from summary fit and cbind it with confint
  summaryFitData <- as.data.frame(exp(summaryFit$coefficients))
  summaryFitData <- cbind(summaryFitData, confint)
  summaryFitData <- summaryFitData[c(2:nrow(summaryFitData)), ]

  ## create the output
  ## case where independentVariable is factor
  if(is.factor(data[, independentVariable])){

    Variable <- c(independentVariableName, rep(NA, nrow(summaryFitData) + 1))
    Modality <- c(NA, paste(levels(data[, independentVariable])[1], "(Ref.)"),
                  gsub(pattern = independentVariable, replacement = "", x = rownames(summaryFitData)))
    N <- c(length(fit$fitted.values), rep(NA, nrow(summaryFitData)+1))
    OR <- setFormatToNumber(c(NA, 1, summaryFitData$Estimate), round = round)
    IC95_OR <- c(NA, NA, paste0("[", setFormatToNumber(summaryFitData$`2.5 %`, round = round), " ; " ,
                                setFormatToNumber(summaryFitData$`97.5 %`, round = round), "]"))
    p_value <-  c(setFormatToPvalue(pvalue = 1 - pchisq(fit$null.deviance - fit$deviance,
                                                  length(fit$coef) - 1),
                             round = round),
                  rep(NA, nrow(summaryFitData)+1))

    summaryFitData <- data.frame(Variable = Variable,
                                 Modality = Modality,
                                 N = N,
                                 OR = OR,
                                 IC95_OR = IC95_OR,
                                 p_value = p_value,
                                 stringsAsFactors = FALSE)

  }
  ## case where independentVariable is numeric
  else if(is.numeric(data[, independentVariable])){
    summaryFitData <- data.frame(Variable = independentVariableName,
                                 Modality = NA,
                                 N = length(fit$fitted.values),
                                 OR = setFormatToNumber(summaryFitData$Estimate, round = round),
                                 IC95_OR = paste0("[", setFormatToNumber(summaryFitData$`2.5 %`, round = round), " ; " ,
                                                  setFormatToNumber(summaryFitData$`97.5 %`, round = round), "]"),
                                 p_value =  setFormatToPvalue(pvalue = 1 - pchisq(fit$null.deviance - fit$deviance,
                                                                            length(fit$coef) - 1),
                                                       round = round),
                                 stringsAsFactors = FALSE)
  }

  colnames(summaryFitData) <- c("Variable", "Modality", "N", "OR", "95%CI[OR]", "p-value")
  return(summaryFitData)
}


#' @title Perform univariable logistic regression models
#' @description A fonction that allow to perform univariable logistic regression
#' @param data a data.frame, containing the data to be used in the regression
#' @param dependentVariable a character vector of length one, containing the names of the dependant variable
#' @param independentVariable a character vector of length one, containing the names of the independant variable
#' @param round a numeric vector of length one, containing the round value to apply
#' @return a data.frame, containing the result of the regression model
#' @noRd
setUnivariableLogisticRegression <- function(data, dependentVariable, independentVariable, round = 2) {

  ## keep only complete casses data and get independent var name
  independentVariableName <- getVarLabel_int(data = data,
                                             variable = independentVariable)
  data <- data[, c(dependentVariable, independentVariable)]
  data <- data[complete.cases(data), ]

  ## case where independentVariable if factor variable
  if(is.factor(data[, independentVariable])){
    ## need two or more levels for regression
    if(length(unique(data[, independentVariable])) > 1){
      ## set logistic model and catch warning as convergence issue
      fitUnivariable <- tryCatch({
        glm(
          formula = getFormulaFromVariableName(
            dependentVariable = dependentVariable,
            independentVariables = independentVariable),
          data = data,
          family = binomial())
      },
      warning = function(w){
        fitUnivariable <- data.frame(
          Variable = independentVariableName,
          Modality = NA,
          N = NA,
          OR = NA,
          IC95_OR = NA,
          p_value = "Convergence issue",
          stringsAsFactors = FALSE
        )
        colnames(fitUnivariable) <- c("Variable", "Modality", "N", "OR", "95%CI[OR]", "p-value")
        return(fitUnivariable)
      },
      error = function(e){
        fitUnivariable <- data.frame(
          Variable = independentVariableName,
          Modality = NA,
          N = NA,
          OR = NA,
          IC95_OR = NA,
          p_value = "Regression not possible",
          stringsAsFactors = FALSE
        )
        colnames(fitUnivariable) <- c("Variable", "Modality", "N", "OR", "95%CI[OR]", "p-value")
        return(fitUnivariable)
      })
    }
    ## case when not enougth modality to perform regression
    else {
      fitUnivariable <- data.frame(
        Variable = independentVariableName,
        Modality = NA,
        N = NA,
        OR = NA,
        IC95_OR = NA,
        p_value = "Not enougth modality to perform regression",
        stringsAsFactors = FALSE
      )
      colnames(fitUnivariable) <- c("Variable", "Modality", "N", "OR", "95%CI[OR]", "p-value")
    }
  }

  ## case where independentVariable if numeric variable
  else if(is.numeric(data[, independentVariable])){
    ## set logistic model and catch warning as convergence issue
    fitUnivariable <- tryCatch({
      glm(
        formula = getFormulaFromVariableName(
          dependentVariable = dependentVariable,
          independentVariables = independentVariable),
        data = data,
        family = binomial())
    },
    warning = function(w){
      fitUnivariable <- data.frame(
        Variable = independentVariableName,
        Modality = NA,
        N = NA,
        OR = NA,
        IC95_OR = NA,
        p_value = "Convergence issue",
        stringsAsFactors = FALSE
      )
      colnames(fitUnivariable) <- c("Variable", "Modality", "N", "OR", "95%CI[OR]", "p-value")
      return(fitUnivariable)
    },
    error = function(e){
      fitUnivariable <- data.frame(
        Variable = independentVariableName,
        Modality = NA,
        N = NA,
        OR = NA,
        IC95_OR = NA,
        p_value = "Regression not possible",
        stringsAsFactors = FALSE
      )
      colnames(fitUnivariable) <- c("Variable", "Modality", "N", "OR", "95%CI[OR]", "p-value")
      return(fitUnivariable)
    })
  }

  ## if fit is OK, extract from fit revelant data
  if(any(class(fitUnivariable) %in% c("glm"))){
    outputFitUnivariable <- extractDataFromUnivariableLogisticModel(
      fit = fitUnivariable,
      data = data,
      independentVariable = independentVariable,
      independentVariableName = independentVariableName,
      round = round)
    return(outputFitUnivariable)
  } else{
    return(fitUnivariable)
  }
}


#' @title Perform univariable linear regression models
#' @description A fonction that allow to perform univariable linear regression
#' @param data a data.frame, containing the data to be used in the regression
#' @param dependentVariable a character vector of length one, containing the names of the dependant variable
#' @param independentVariable a character vector of length one, containing the names of the independant variable
#' @param round a numeric vector of length one, containing the round value to apply
#' @return a data.frame, containing the result of the regression model
#' @noRd
setUnivariableLinearRegression <- function(data, dependentVariable, independentVariable, round = 2) {
  ## keep only complete casses data and get independent var name
  independentVariableName <- getVarLabel_int(data = data,
                                             variable = independentVariable)
  data <- data[, c(dependentVariable, independentVariable)]
  data <- data[complete.cases(data), ]

  ## case where independentVariable is factor variable
  if(is.factor(data[, independentVariable])){
    ## need two or more levels for regression
    if(length(unique(data[, independentVariable])) > 1){
      ## set linear model and catch warning as Convergence issue
      fitUnivariable <- tryCatch({
        lm(formula = getFormulaFromVariableName(
          dependentVariable = dependentVariable,
          independentVariables = independentVariable),
          data = data)
      },
      warning = function(w){
        fitUnivariable <- data.frame(
          Variable = independentVariableName,
          Modality = NA,
          N = NA,
          Beta = NA,
          IC95_Beta = NA,
          p_value = "Convergence issue",
          stringsAsFactors = FALSE
        )
        colnames(fitUnivariable) <- c("Variable", "Modality", "N", "Beta", "95%CI[Beta]", "p-value")
      },
      error = function(e){
        fitUnivariable <- data.frame(
          Variable = independentVariableName,
          Modality = NA,
          N = NA,
          Beta = NA,
          IC95_Beta = NA,
          p_value = "Regression non possible",
          stringsAsFactors = FALSE
        )
        colnames(fitUnivariable) <- c("Variable", "Modality", "N", "Beta", "95%CI[Beta]", "p-value")
      })
    }

    ## case when not enougth modality to perform regression
    else {
      fitUnivariable <- data.frame(
        Variable = independentVariableName,
        Modality = NA,
        N = NA,
        Beta = NA,
        IC95_Beta = NA,
        p_value = "Not enougth modality to perform regression",
        stringsAsFactors = FALSE
      )
      colnames(fitUnivariable) <- c("Variable", "Modality", "N", "Beta", "95%CI[Beta]", "p-value")
    }
  }

  ## case where independentVariable is numeric variable
  else if(is.numeric(data[, independentVariable])){
    ## set linear model and catch warning as Convergence issue
    fitUnivariable <- tryCatch({
      lm(formula = getFormulaFromVariableName(
        dependentVariable = dependentVariable,
        independentVariables = independentVariable),
        data = data)
    },
    warning = function(w){
      fitUnivariable <- data.frame(
        Variable = independentVariableName,
        Modality = NA,
        N = NA,
        Beta = NA,
        IC95_Beta = NA,
        p_value = "Convergence issue",
        stringsAsFactors = FALSE
      )
      colnames(fitUnivariable) <- c("Variable", "Modality", "N", "Beta", "95%CI[Beta]", "p-value")
    },
    error = function(e){
      fitUnivariable <- data.frame(
        Variable = independentVariableName,
        Modality = NA,
        N = NA,
        Beta = NA,
        IC95_Beta = NA,
        p_value = "Regression not possible",
        stringsAsFactors = FALSE
      )
      colnames(fitUnivariable) <- c("Variable", "Modality", "N", "Beta", "95%CI[Beta]", "p-value")
    })
  }

  ## if fit is OK, extract from fit revelant data
  if(any(class(fitUnivariable) %in% c("lm"))){
    outputFitUnivariable <- extractDataFromUnivariableLinearModel(
      fit = fitUnivariable,
      independentVariable = independentVariable,
      independentVariableName = independentVariableName,
      data = data,
      round = round)
    return(outputFitUnivariable)
  } else{
    return(fitUnivariable)
  }
}


#' @title A convenient method to extract data from multivariable logistic model.
#' @description A convenient method to extract data from multivariable logistic model.
#' @param data a data.frame, containing the data to be used in the regression
#' @param dependentVariable a character vector of length one, containing the names of the dependant variable
#' @param independentVariables a character vector containing the names of the independant variables
#' @param round a numeric vector of length one, containing the round value to apply
#' @param fit a fit element generated by glm function
#' @param summaryFit the summary of the fit element
#' @param confint the confint of the fit element
#' @return a data.frame, containing the result of the regression model
#' @noRd
extractDataFromMultivariableLogisticModel <- function(data, dependentVariable, independentVariables, round, fit, summaryFit, confint){
  ## merge coef and confint
  summaryFit <- cbind(summaryFit, confint)
  ## compute the global p-value of each variable
  globalPvalue <- drop1(fit, test = "LRT")
  ## get ordrer of modality in factor variable
  fitModality <- fit$xlevels
  ## get the N used to compute the fit
  fitSize <- length(fit$fitted.values)

  ## initialization of the row number of the summaryFit (intercept row excluded)
  currentRowFit <- 2
  ## initialization of the output
  output <- NULL
  ## for each independent varable
  for(i in 1:length(independentVariables)){
    ## get the current independent variable
    currentIndependentVariable <- independentVariables[i]
    ## get the current data of independent variable
    currentDataIndependentVariable <- data[, currentIndependentVariable]
    ## get the current independent variable name
    currentIndependentVariableName <- getVarLabel_int(data = data,
                                                      variable = currentIndependentVariable)
    ## if current independent variable is numeric
    if(is.numeric(currentDataIndependentVariable)){
      ## manage the currentOutput
      currentOutputFitUnivariable <- data.frame(
        Variable = currentIndependentVariableName,
        Modality = NA,
        N = fitSize,
        OR = setFormatToNumber(num = summaryFit[currentRowFit, 1], round = round),
        IC95_OR = paste0("[", setFormatToNumber(summaryFit[currentRowFit, 5], round = round), " ; ",
                         setFormatToNumber(summaryFit[currentRowFit, 6], round = round), "]"),
        p_value = setFormatToPvalue(pvalue = globalPvalue[currentIndependentVariable, "Pr(>Chi)"], round = round),
        stringsAsFactors = FALSE
      )
      ## merge the currentOutput with the global output
      output <- rbind(output, currentOutputFitUnivariable)
      ## increment row to based on in the fit
      currentRowFit <- currentRowFit + 1
    }
    ## if current independent variable is factor variable
    else if(is.factor(currentDataIndependentVariable)){
      ## get modality list of current factor variable
      listModalityCurrentFactor <- fitModality[[currentIndependentVariable]]
      ## for each modality
      for(j in 1:length(listModalityCurrentFactor)){
        currentModalityCurrentFactor <- listModalityCurrentFactor[j]
        ## Ref. modality
        if(j==1){
          ## manage the currentOutput
          currentOutputFitUnivariable <- data.frame(
            Variable = c(currentIndependentVariableName, NA),
            Modality = c(NA, paste0(currentModalityCurrentFactor, " (Ref.)")),
            N = c(fitSize, NA),
            OR = c(NA, 1),
            IC95_OR = c(NA, NA),
            p_value = c(setFormatToPvalue(pvalue = globalPvalue[currentIndependentVariable, "Pr(>Chi)"], round = round), NA),
            stringsAsFactors = FALSE
          )
          ## merge the currentOutput with the global output
          output <- rbind(output, currentOutputFitUnivariable)
        }
        ## Other modality
        else {
          ## manage the currentOutput
          currentOutputFitUnivariable <- data.frame(
            Variable = NA,
            Modality = currentModalityCurrentFactor,
            N = NA,
            OR = setFormatToNumber(num = summaryFit[currentRowFit, 1], round = round),
            IC95_OR = paste0("[", setFormatToNumber(summaryFit[currentRowFit, 5], round = round), " ; ",
                             setFormatToNumber(summaryFit[currentRowFit, 6], round = round), "]"),
            p_value = NA,
            stringsAsFactors = FALSE)
          ## merge the currentOutput with the global output
          output <- rbind(output, currentOutputFitUnivariable)
          ## increment row to based on in the fit
          currentRowFit <- currentRowFit + 1
        }
      }
    }
  }
  colnames(output) <- c("Variable", "Modality", "N", "OR", "95%CI[OR]", "p-value")
  attr(output, "var_dep") <- dependentVariable
  attr(output, "mod_var_dep") <- levels(data[, dependentVariable])
  attr(output, "var_indep") <- independentVariables
  attr(output, "modelisation") <- paste0("Sont modélisées les probabilités de transition des modalitées : '",
                                         paste0(levels(data[, dependentVariable]), collapse = "' -> '"),
                                         "' (variable : '", getVarLabel_int(data = data,
                                                                            variable = dependentVariable), "')")
  return(output)
}


#' @title A convenient method to extract data from multivariable linear model.
#' @description A convenient method to extract data from multivariable linear model.
#' @param data a data.frame, containing the data to be used in the regression
#' @param dependentVariable a character vector of length one, containing the names of the dependant variable
#' @param independentVariables a character vector containing the names of the independant variables
#' @param round a numeric vector of length one, containing the round value to apply
#' @param fit a fit element generated by lm function
#' @param summaryFit the summary of the fit element
#' @param confint the confint of the fit element
#' @return a data.frame, containing the result of the regression model
#' @noRd
extractDataFromMultivariableLinearModel <- function(data, dependentVariable, independentVariables, round, fit, summaryFit, confint){
  ## merge coef and confint
  summaryFit <- cbind(summaryFit, confint)
  ## compute the global p-value of each variable
  globalPvalue <- drop1(fit, test = "F")
  ## get ordrer of modality in factor variable
  fitModality <- fit$xlevels
  ## get the N used to compute the fit
  fitSize <- length(fit$fitted.values)

  ## initialization of the row number of the summaryFit (intercept row excluded)
  currentRowFit <- 2
  ## initialization of the output
  output <- NULL
  ## for each independent varable
  for(i in 1:length(independentVariables)){
    ## get the current independent variable
    currentIndependentVariable <- independentVariables[i]
    ## get the current data of independent variable
    currentDataIndependentVariable <- data[, currentIndependentVariable]
    ## get the current independent variable name
    currentIndependentVariableName <- getVarLabel_int(data = data,
                                                      variable = currentIndependentVariable)
    ## if current independent variable is numeric
    if(is.numeric(currentDataIndependentVariable)){
      ## manage the currentOutput
      currentOutputFitUnivariable <- data.frame(
        Variable = currentIndependentVariableName,
        Modality = NA,
        N = fitSize,
        Beta = setFormatToNumber(num = summaryFit[currentRowFit, 1], round = round),
        IC95_beta = paste0("[", setFormatToNumber(summaryFit[currentRowFit, 5], round = round), " ; ",
                           setFormatToNumber(summaryFit[currentRowFit, 6], round = round), "]"),
        p_value = setFormatToPvalue(pvalue = globalPvalue[currentIndependentVariable, "Pr(>F)"], round = round),
        stringsAsFactors = FALSE
      )
      ## merge the currentOutput with the global output
      output <- rbind(output, currentOutputFitUnivariable)
      ## increment row to based on in the fit
      currentRowFit <- currentRowFit + 1
    }
    ## if current independent variable is factor variable
    else if(is.factor(currentDataIndependentVariable)){
      ## get modality list of current factor variable
      listModalityCurrentFactor <- fitModality[[currentIndependentVariable]]
      ## for each modality
      for(j in 1:length(listModalityCurrentFactor)){
        currentModalityCurrentFactor <- listModalityCurrentFactor[j]
        ## Ref. modality
        if(j==1){
          ## manage the currentOutput
          currentOutputFitUnivariable <- data.frame(
            Variable = c(currentIndependentVariableName, NA),
            Modality = c(NA, paste0(currentModalityCurrentFactor, " (Ref.)")),
            N = c(fitSize, NA),
            Beta = c(NA, NA),
            IC95_beta = c(NA, NA),
            p_value = c(setFormatToPvalue(pvalue = globalPvalue[currentIndependentVariable, "Pr(>F)"], round = round), NA),
            stringsAsFactors = FALSE
          )
          ## merge the currentOutput with the global output
          output <- rbind(output, currentOutputFitUnivariable)
        }
        ## Other modality
        else {
          ## manage the currentOutput
          currentOutputFitUnivariable <- data.frame(
            Variable = NA,
            Modality = currentModalityCurrentFactor,
            N = NA,
            Beta = setFormatToNumber(num = summaryFit[currentRowFit, 1], round = round),
            IC95_beta = paste0("[", setFormatToNumber(summaryFit[currentRowFit, 5], round = round), " ; ",
                               setFormatToNumber(summaryFit[currentRowFit, 6], round = round), "]"),
            p_value = NA,
            stringsAsFactors = FALSE)
          ## merge the currentOutput with the global output
          output <- rbind(output, currentOutputFitUnivariable)
          ## increment row to based on in the fit
          currentRowFit <- currentRowFit + 1
        }
      }
    }
  }
  colnames(output) <- c("Variable", "Modality", "N", "Beta", "95%CI[Beta]", "p-value")
  attr(output, "var_dep") <- dependentVariable
  attr(output, "var_indep") <- independentVariables
  return(output)
}
