% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistic_functions.R
\name{describeMetadata}
\alias{describeMetadata}
\title{Get a summary for the given data.frame}
\usage{
describeMetadata(data)
}
\arguments{
\item{data}{a data.frame or tbl_df}
}
\value{
a data.frame containing the metada description
}
\description{
Get a summary for the given data.frame
}
\examples{
data(mtcars)
describeMetadata(mtcars)
}
