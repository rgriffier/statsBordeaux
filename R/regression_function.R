#' @title Perform univariable regression models
#' @description A fonction that allow to perform univariable linear and logistic regression and
#' produce a data.frame to display
#' @param data a data.frame, containing the data to be used in the regression
#' @param dependentVariable a character vector of length one, containing the names of the dependant variable
#' @param independentVariable a character vector of length one, containing the names of the independant variable
#' @param round a numeric vector of length one, containing the round value to apply
#' @return a data.frame, containing the result of the regression model
#' @export
#' @examples
#' data("mtcars")
#' setUnivariableRegression(data = mtcars,
#'                          dependentVariable = "cyl",
#'                          independentVariable = "mpg")
setUnivariableRegression <- function(data, dependentVariable, independentVariable, round = 2){

  if(!is.vector(dependentVariable) | !is.character(dependentVariable) | length(dependentVariable) !=1){
    stop("dependentVariable must be a characher vector of length 1")
  }
  if(!is.vector(independentVariable) | !is.character(independentVariable) | length(independentVariable) !=1){
    stop("independentVariable must be a characher vector of length 1")
  }
  if(!is.data.frame(data) | nrow(data) < 10){
    stop("data must be a data.frame with minimum 10 rows")
  }
  if(any(!c(dependentVariable, independentVariable) %in% colnames(data))){
    stop("dependentVariable and independentVariable must be colnames of data")
  }
  if(!is.factor(data[, independentVariable]) & !is.numeric(data[, independentVariable])){
    stop("independentVariable must be a numeric or factor variable")
  }

  ## LOGISTIC REGRESSION
  if(is.factor(data[, dependentVariable])){
    resultFit <- setUnivariableLogisticRegression(data = data,
                                     dependentVariable = dependentVariable,
                                     independentVariable = independentVariable,
                                     round = round)
  }
  ## LINEAR REGRESSION
  else if(is.numeric(data[, dependentVariable])){
    resultFit <- setUnivariableLinearRegression(data = data,
                                   dependentVariable = dependentVariable,
                                   independentVariable = independentVariable,
                                   round = round)

  }

  row.names(resultFit) <- NULL
  return(resultFit)
}


#' @title Perform univariable regression models
#' @description A fonction that allow to perform multivariable linear and logistic regression and
#' produce a data.frame to display
#' @param data a data.frame, containing the data to be used in the regression
#' @param dependentVariable a character vector of length one, containing the names of the dependant variable
#' @param independentVariables a character vector containing the names of the independant variables
#' @param round a numeric vector of length one, containing the round value to apply
#' @return a data.frame, containing the result of the regression model
#' @export
#' @examples
#' data("mtcars")
#' setMultivariableRegression(data = mtcars,
#'                            dependentVariable = "cyl",
#'                            independentVariables = c("mpg", "drat")
setMultivariableRegression <- function(data, dependentVariable, independentVariables, round = 2){

  if(!is.vector(dependentVariable) | !is.character(dependentVariable) | length(dependentVariable) !=1){
    stop("dependentVariable must be a characher vector of length 1")
  }
  if(!is.vector(independentVariables) | !is.character(independentVariables)){
    stop("independentVariables must be a characher")
  }
  if(!is.data.frame(data) | nrow(data) < 10){
    stop("data must be a data.frame with minimum 10 rows")
  }
  if(any(!c(dependentVariable, independentVariables) %in% colnames(data))){
    stop("dependentVariable and independentVariables must be colnames of data")
  }
  if(!checkIfNumericOrFactor(data[, independentVariables])){
    stop("independentVariables must be a numeric or factor variable")
  }

  ## LOGISTIC REGRESSION
  if(is.factor(data[, dependentVariable])){

    ## get the fit
    fit <- tryCatch({
      glm(formula = getFormulaFromVariableName(
        dependentVariable = dependentVariable,
        independentVariables = independentVariables
      ),
      data = data,
      family = binomial())
    }, warning = function(w){
      stop("Congergence problem")
    })

    # get coefficients of the fit
    summaryFit <- tryCatch({
      exp(summary(fit)$coefficients)
    }, warning = function(w){
      stop("Congergence problem")
    })

    # get confint of the fit
    confint <- tryCatch({
      suppressMessages(exp(confint(fit)))
    }, warning = function(w){
      stop("Congergence problem")
    })

    multivariableDataOutput <- extractDataFromMultivariableLogisticModel(
      data = data,
      dependentVariable = dependentVariable,
      independentVariables = independentVariables,
      round = round,
      fit = fit,
      summaryFit = summaryFit,
      confint = confint)

    row.names(multivariableDataOutput) <- NULL

    return(multivariableDataOutput)
  }


  ## LINEAR REGRESSION
  else if(is.numeric(data[, dependentVariable])){

    ## get the fit
    fit <- tryCatch({
      lm(formula = getFormulaFromVariableName(
        dependentVariable = dependentVariable,
        independentVariables = independentVariables
      ),
      data = data)
    }, warning = function(w){
      stop("Congergence problem")
    })

    # get coefficients of the fit
    summaryFit <- tryCatch({
      summary(fit)$coefficients
    }, warning = function(w){
      stop("Congergence problem")
    })

    # get confint of the fit
    confint <- tryCatch({
      confint(fit)
    }, warning = function(w){
      stop("Congergence problem")
    })

    multivariableDataOutput <- extractDataFromMultivariableLinearModel(
      data = data,
      dependentVariable = dependentVariable,
      independentVariables = independentVariables,
      round = round,
      fit = fit,
      summaryFit = summaryFit,
      confint = confint)

    row.names(multivariableDataOutput) <- NULL

    return(multivariableDataOutput)
  }
}
