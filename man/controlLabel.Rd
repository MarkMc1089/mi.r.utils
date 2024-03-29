% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{controlLabel}
\alias{controlLabel}
\title{Construct accessible label}
\usage{
controlLabel(controlName, label)
}
\arguments{
\item{controlName}{Character, id of an html tag}

\item{label}{Character, displayed label}
}
\value{
HTML for label or NULL when label is NULL
}
\description{
Construct accessible label
}
\examples{
controlLabel("my_id", "My label:")
}
