% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kable_function.R
\name{addKable}
\alias{addKable}
\title{A convenient method to add kable into a RMarkdown document}
\usage{
addKable(
  data_frame,
  caption = NULL,
  all_before = FALSE,
  var.width = 50,
  varColumn.size = "5cm",
  analysisColumn.size = "4cm"
)
}
\arguments{
\item{data_frame}{a data.frame, containing the data to add}

\item{caption}{a character vector of length one, containing the caption of the table}

\item{all_before}{a boolean vector of length one, indicatig if all column must be before strata column. Default to FALSE}

\item{var.width}{a numeric vector of length one, containing the witdh of the variable column}

\item{varColumn.size}{a character string telling how wide the variable column needs to be, e.g. "10cm", "3in" or "30em".}

\item{analysisColumn.size}{a character string telling how wide the analysis column(s) needs to be, e.g. "10cm", "3in" or "30em".}
}
\value{
a knitr::kable
}
\description{
A convenient method to add kable into a RMarkdown document
}
