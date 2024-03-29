% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boxhist.R
\name{boxhist}
\alias{boxhist}
\title{boxhist}
\usage{
boxhist(df, ...)
}
\arguments{
\item{df}{a data frame}

\item{...}{passes extra arguments to the function}
}
\value{
two dfs and plots
}
\description{
This function takes a df and returns two dfs (one numerical and one categorical). Also a list of potential outliers and plots (boxplot, histogram, and bar-plot)
}
\examples{
\dontrun{boxhist(df = ddt)}
}
