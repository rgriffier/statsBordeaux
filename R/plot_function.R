#' @title A convenient method to get box plot from numeric variable.
#' @description A convenient method to get box plot from numeric variable.
#' @param data a data.frame, containing the numeric variable to describe with boxplot
#' @param variable a character vector, containing the name of the variables to describe
#' @param group a character vector of length one, containing the name of the
#' group which need to be used to describe the numeric variable. Depault to NULL
#' @param legend.position a characher vector of length one, containing the legend position.
#' Must be in 'right', 'left', 'top', 'bottom, 'none'. Default to 'right'.
#' @param legend.width a numeric vector of length one, containing the legend width. Default to 50
#' @return a ggplot2 plot
#' @import ggplot2
#' @import stringr
#' @export
#' @examples
#' data(mtcars)
#' mtcars$am <- as.factor(mtcars$am)
#' getBoxPlot(data = mtcars, variable = "mpg", group = "am")
getBoxPlot <- Vectorize(function(data, variable, group = NULL, legend.position = "right", legend.width = 30){

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
    return(NULL)
  }
  if(!is.null(group)){
    if(!is.vector(group) | !is.character(group) | length(group) != 1){
      stop("group must be a character vector of length one.")
    }
    if(!group %in% colnames(data)){
      stop("group must be the name of one column in data.")
    }
    if(!is.factor(data[, group])){
      stop("group must be a qualitative variable in data.")
    }
  }
  if(!is.vector(legend.position) | !is.character(legend.position) | length(legend.position) != 1){
    stop("legend.position must be a character vector of length one.")
  }
  if(!legend.position %in% c('right', 'left', 'top', 'bottom', 'none')){
    stop("legend.position must be in 'right', 'left', 'top', 'bottom' or 'none'.")
  }

  ylab <- attr(data[, variable], "var_label")
  ylab <- ifelse(!is.null(ylab), ylab, variable)
  ylab <- stringr::str_wrap(ylab, width = legend.width)

  if(!is.null(group)){

    labs <- attr(data[, group], "var_label")
    labs <- ifelse(!is.null(labs), labs, group)
    labs <- stringr::str_wrap(labs, width = legend.width)

    plot <- ggplot2::ggplot(data) +
      ggplot2::geom_boxplot(ggplot2::aes(y = get(variable), fill = get(group))) +
      ggplot2::theme_minimal() +
      ggplot2::xlab("") +
      ggplot2::ylab(ylab) +
      ggplot2::labs(fill = labs) +
      ggplot2::scale_fill_discrete(drop = FALSE) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::theme(legend.position = legend.position,
                     legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "blank"),
                     legend.title = ggplot2::element_text(face = "bold"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), face="bold"))
  } else {
    plot <- ggplot2::ggplot(data) +
      ggplot2::geom_boxplot(ggplot2::aes(y = get(variable)), fill = "lightblue") +
      ggplot2::theme_minimal() +
      ggplot2::xlab("") +
      ggplot2::ylab(ylab) +
      ggplot2::scale_x_continuous(limits = c(-2,2)) +
      ggplot2::theme(legend.position = "none",
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), face="bold"))
  }
  return(plot)
}, vectorize.args = "variable", SIMPLIFY = FALSE)


#' @title A convenient method to get bar plot from qualitative variable.
#' @description A convenient method to get bar plot from qualitative variable.
#' @param data a data.frame, containing the qualitative variable to describe with barplot
#' @param variable a character vector, containing the name of the variables to describe
#' @param group a character vector of length one, containing the name of the
#' group which need to be used to describe the qualitative variable. Depault to NULL
#' @param legend.position a characher vector of length one, containing the legend position.
#' Must be in 'right', 'left', 'top', 'bottom, 'none'. Default to 'right'.
#' @param legend.width a numeric vector of length one, containing the legend width. Default to 50
#' @param na.rm a boolean vector of length one. If TRUE, missig value of variable will be remove.
#' Default to TRUE
#' @return a ggplot2 plot
#' @import ggplot2
#' @import stringr
#' @export
#' @examples
#' data(mtcars)
#' mtcars$am <- as.factor(mtcars$am)
#' mtcars$vs <- as.factor(mtcars$vs)
#' getBarPlot(data = mtcars, variable = "vs", group = "am")
getBarPlot <- Vectorize(function(data, variable, group = NULL, legend.position = "right", legend.width = 30, na.rm = TRUE){

  if(!is.data.frame(data)){
    stop("data must be a data.frame.")
  }
  if(!is.vector(variable) | !is.character(variable) | length(variable) != 1){
    stop("variable must be a character vector of length one.")
  }
  if(!variable %in% colnames(data)){
    stop("variable must be the name of one column in data.")
  }
  if(!is.factor(data[, variable])){
    return(NULL)
  }
  if(!is.null(group)){
    if(!is.vector(group) | !is.character(group) | length(group) != 1){
      stop("group must be a character vector of length one.")
    }
    if(!group %in% colnames(data)){
      stop("group must be the name of one column in data.")
    }
    if(!is.factor(data[, group])){
      stop("group must be a qualitative variable in data.")
    }
  }
  if(!is.vector(legend.position) | !is.character(legend.position) | length(legend.position) != 1){
    stop("legend.position must be a character vector of length one.")
  }
  if(!legend.position %in% c('right', 'left', 'top', 'bottom', 'none')){
    stop("legend.position must be in 'right', 'left', 'top', 'bottom' or 'none'.")
  }
  if(!is.vector(na.rm) | !is.logical(na.rm) | length(na.rm) != 1){
    stop("na.rm must be a boolean vector of length one.")
  }

  dataPlot <- data
  if(!is.null(group)){
    dataPlot <- subset(data, !is.na(get(group)))
    dataPlot[, group] <- as.factor(as.character(dataPlot[, group]))
  }

  if(na.rm){
    dataPlot <- subset(dataPlot, !is.na(get(variable)))
    dataPlot[, variable] <- as.factor(as.character(dataPlot[, variable]))
  }

  # format big label
  labs <- attr(data[, variable], "var_label")
  labs <- ifelse(!is.null(labs), labs, variable)
  labs <- stringr::str_wrap(labs, width = legend.width)

  # format big modality
  attr <- attr(dataPlot[, variable], "var_label")
  dataPlot[variable] <- as.factor(stringr::str_wrap(as.character(dataPlot[,variable]), width = legend.width))
  attr(dataPlot[, variable], "var_label") <- attr

  if(!is.null(group)){
    # format big label
    xlab <- attr(data[, group], "var_label")
    xlab <- ifelse(!is.null(xlab), xlab, group)
    xlab <- stringr::str_wrap(xlab, width = legend.width)

    # format big modality
    attr <- attr(dataPlot[, group], "var_label")
    dataPlot[group] <- as.factor(stringr::str_wrap(as.character(dataPlot[,group]), width = legend.width))
    attr(dataPlot[, group], "var_label") <- attr

    plot <- ggplot2::ggplot(data = dataPlot, ggplot2::aes(x = get(variable), group = get(group), na.rm = TRUE)) +
      ggplot2::geom_bar(ggplot2::aes(y = ..prop.., fill = factor(..x..)), stat="count", na.rm = TRUE) +
      ggplot2::geom_text(ggplot2::aes(label = scales::percent(..prop..), y = ..prop.. ),
                         stat = "count", vjust = -.5, na.rm = TRUE) +
      ggplot2::ylab("Percent") +
      ggplot2::labs(fill = labs) +
      ggplot2::xlab(xlab) +
      ggplot2::scale_fill_discrete(labels = levels(dataPlot[, variable])) +
      ggplot2::facet_grid(~get(group)) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::theme(legend.position = legend.position,
                     legend.title = ggplot2::element_text(face = "bold"),
                     axis.title.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), face = "bold"),
                     axis.text.x = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(face = "bold"),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "blank"),
                     strip.text.x = ggplot2::element_text(face = "bold"))
  } else {
    plot <- ggplot2::ggplot(data = dataPlot, ggplot2::aes(x = get(variable), fill = get(variable), na.rm = TRUE)) +
      ggplot2::geom_bar(ggplot2::aes(y = (..count..)/sum(..count..)), stat = "count", na.rm = TRUE) +
      ggplot2::geom_text(ggplot2::aes(label = scales::percent((..count..)/sum(..count..)), y = (..count..)/sum(..count..)),
                         stat = "count", vjust = -.5, na.rm = TRUE) +
      ggplot2::theme_minimal() +
      ggplot2::ylab("Percent") +
      ggplot2::scale_fill_discrete(labels = levels(dataPlot[, variable]), drop = FALSE) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::labs(fill = labs) +
      ggplot2::theme(legend.position = legend.position,
                     legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "blank"),
                     legend.title = ggplot2::element_text(face = "bold"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), face = "bold"))
  }
  return(plot)
}, vectorize.args = "variable", SIMPLIFY = FALSE)


#' @title A convenient method to get plot from data.frame.
#' @description A convenient method to get plot from data.frame
#' @param data a data.frame, containing the numeric variable to describe with boxplot
#' @param variable a character vector, containing the name of the variables to describe
#' @param group a character vector of length one, containing the name of the
#' @param legend.position a characher vector of length one, containing the legend position.
#' Must be in 'right', 'left', 'top', 'bottom, 'none'. Default to 'right'.
#' @param legend.width a numeric vector of length one, containing the legend width. Default to 50
#' @param na.rm a boolean vector of length one. If TRUE, missig value of variable will be remove.
#' Default to TRUE
#' @return a ggplot2 plot
#' @export
#' @examples
#' data(mtcars)
#' mtcars$am <- as.factor(mtcars$am)
#' mtcars$vs <- as.factor(mtcars$vs)
#' mtcars %>% getGraphicalDescription(group = "am")
getGraphicalDescription <- function(data, variable = colnames(data), group = NULL, legend.position = "right", legend.width = 30, na.rm = TRUE){
  listPlot <- lapply(variable, function(currentVar){
    if(is.numeric(data[, currentVar])){
      plot <- getBoxPlot(data = data,
                         variable = currentVar,
                         group = group,
                         legend.position = legend.position,
                         legend.width = legend.width)
    } else if(is.factor(data[, currentVar])){
      plot <- getBarPlot(data = data,
                         variable = currentVar,
                         group = group,
                         legend.position = legend.position,
                         legend.width = legend.width,
                         na.rm = na.rm)
    }
  })
  listPlot[sapply(listPlot, is.null)] <- NULL
  return(listPlot)
}

#' @title Get density plot of numeric variable into data.frame
#' @description Get density plot of numeric variable into data.frame
#' @param data a data.frame
#' @return a list of plot
#' @export
#' @import ggplot2
#' @examples
#' data(mtcars)
#' getDensityPlot(mtcars)
getDensityPlot <- function(data){
  densityPlot <- lapply(colnames(data), function(x){
    if(!class(data[, x]) %in% c("numeric", "integer", "double")){
      return(NA)
    }
    plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = get(x))) +
      ggplot2::geom_density(na.rm = T, color="black", fill="lightblue", alpha = 0.5) +
      ggplot2::theme_minimal() +
      ggplot2::xlab(getLabelFromVariable(data[x])) +
      ggplot2::ylab("Density")
    return(plot)
  })
  return(densityPlot)
}
