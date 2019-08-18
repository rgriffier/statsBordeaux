#' @title A convenient method to get box plot from numeric variable.
#' @description A convenient method to get box plot from numeric variable.
#' @param data a data.frame, containing the numeric variable to describe with boxplot
#' @param variable a character vector of length one, containing the name of the
#' variable to describe
#' @param group a character vector of length one, containing the name of the
#' group which need to be used to describe the numeric variable. Depault to NULL
#' @param legend.position a characher vector of length one, containing the legend position.
#' Must be in 'right', 'left', 'top', 'bottom, 'none'. Default to 'right'.
#' @return a ggplot2 plot
#' @import ggplot2
#' @export
#' @examples
#' data(mtcars)
#' mtcars$am <- as.factor(mtcars$am)
#' getBoxPlot(data = mtcars, variable = "mpg", group = "am")
getBoxPlot <- function(data, variable, group = NULL, legend.position = 'right'){

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
  if(!is.vector(group) | !is.character(group) | length(group) != 1){
    stop("group must be a character vector of length one.")
  }
  if(!is.null(group)){
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
  labs <- attr(data[, group], "var_label")
  labs <- ifelse(!is.null(labs), labs, group)

  if(!is.null(group)){
    plot <- ggplot2::ggplot(data) +
      ggplot2::geom_boxplot(ggplot2::aes(y = get(variable), fill = get(group))) +
      ggplot2::theme_minimal() +
      ggplot2::xlab("") +
      ggplot2::ylab(ylab) +
      ggplot2::labs(fill = labs) +
      ggplot2::scale_fill_discrete(drop = FALSE) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::theme(legend.position = legend.position,
                     legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "dotted"),
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
}


#' @title A convenient method to get bar plot from qualitative variable.
#' @description A convenient method to get bar plot from qualitative variable.
#' @param data a data.frame, containing the qualitative variable to describe with barplot
#' @param variable a character vector of length one, containing the name of the
#' variable to describe
#' @param group a character vector of length one, containing the name of the
#' group which need to be used to describe the qualitative variable. Depault to NULL
#' @param legend.position a characher vector of length one, containing the legend position.
#' Must be in 'right', 'left', 'top', 'bottom, 'none'. Default to 'right'.
#' @return a ggplot2 plot
#' @import ggplot2
#' @export
#' @examples
#' data(mtcars)
#' mtcars$am <- as.factor(mtcars$am)
#' mtcars$vs <- as.factor(mtcars$vs)
#' getBarPlot(data = mtcars, variable = "vs", group = "am")
getBarPlot <- function(data, variable, group = NULL, legend.position = 'right'){

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
    stop("variable must be a qualitative variable in data.")
  }
  if(!is.vector(group) | !is.character(group) | length(group) != 1){
    stop("group must be a character vector of length one.")
  }
  if(!is.null(group)){
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

  if(!is.null(group)){
    xlab <- attr(data[, group], "var_label")
    xlab <- ifelse(!is.null(xlab), xlab, group)

    plot <- ggplot2::ggplot(data = subset(data, !is.na(get(variable))), ggplot2::aes(x = get(variable), group = get(group), na.rm = TRUE)) +
      ggplot2::geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", na.rm = TRUE) +
      ggplot2::geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),
                stat = "count", vjust = -.5, na.rm = TRUE) +
      ggplot2::ylab("Percent") +
      ggplot2::xlab(xlab) +
      ggplot2::facet_grid(~get(group)) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::theme(ggplot2::legend.position = "none",
                     ggplot2::legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "dotted"),
                     ggplot2::legend.title = ggplot2::element_text(face = "bold"),
                     ggplot2::axis.title.y.left = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), face = "bold"),
                     ggplot2::axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0), face = "bold"),
                     ggplot2::axis.text.x = ggplot2::element_text(face = "bold"))
  } else {
    xlab <- attr(data[, variable], "var_label")
    xlab <- ifelse(!is.null(variable), variable, group)

    plot <- ggplot2::ggplot(data = subset(data, !is.na(get(variable))), ggplot2::aes(x = get(variable), group = 1, na.rm = TRUE)) +
      ggplot2::geom_bar(ggplot2::aes(y = ..prop.., fill = factor(..x..)), stat = "count", na.rm = TRUE) +
      ggplot2::geom_text(ggplot2::aes(label = scales::percent(..prop..), y = ..prop.. ),
                         stat = "count", vjust = -.5, na.rm = TRUE) +
      ggplot2::theme_minimal() +
      ggplot2::ylab("Percent") +
      ggplot2::xlab(labs) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(face = "bold"),
                     axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0), face = "bold"),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), face = "bold"))
  }
  return(plot)
}