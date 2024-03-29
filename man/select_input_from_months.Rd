% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{select_input_from_months}
\alias{select_input_from_months}
\title{Create select input with choices corresponding to months in data.}
\usage{
select_input_from_months(id, label, month_data, ...)
}
\arguments{
\item{id}{Character}

\item{label}{Display label for the control, or \code{NULL} for no label.}

\item{month_data}{Data}

\item{...}{Arguments passed to \code{selectInput()}.}
}
\value{
HTML
}
\description{
Create select input with choices corresponding to months in data.
}
\examples{
select_input_from_months(
  "month-select",
  "Month:",
  c("2023/01/01", "2023/02/01", "2023/03/01")
)
}
