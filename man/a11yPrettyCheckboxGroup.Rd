% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{a11yPrettyCheckboxGroup}
\alias{a11yPrettyCheckboxGroup}
\title{Accessibility compliant version of shinyWidgets::prettyCheckboxGroup}
\usage{
a11yPrettyCheckboxGroup(
  inputId,
  label,
  choices = NULL,
  selected = NULL,
  status = "default",
  shape = c("square", "curve", "round"),
  outline = FALSE,
  fill = FALSE,
  thick = FALSE,
  animation = NULL,
  icon = NULL,
  plain = FALSE,
  bigger = FALSE,
  inline = FALSE,
  width = NULL,
  choiceNames = NULL,
  choiceValues = NULL
)
}
\arguments{
\item{inputId}{The \code{input} slot that will be used to access the value.}

\item{label}{Display label for the control.}

\item{choices}{List of values to show checkboxes for. If elements of the list
are named then that name rather than the value is displayed to the user. If
this argument is provided, then \code{choiceNames} and \code{choiceValues} must not be provided,
and vice-versa. The values should be strings; other types (such as logicals and
numbers) will be coerced to strings.}

\item{selected}{The values that should be initially selected, if any.}

\item{status}{Add a class to the checkbox,
you can use Bootstrap status like 'info', 'primary', 'danger', 'warning' or 'success'.}

\item{shape}{Shape of the checkbox between \code{square}, \code{curve} and \code{round}.}

\item{outline}{Color also the border of the checkbox (\code{TRUE} or \code{FALSE}).}

\item{fill}{Fill the checkbox with color (\code{TRUE} or \code{FALSE}).}

\item{thick}{Make the content inside checkbox smaller (\code{TRUE} or \code{FALSE}).}

\item{animation}{Add an animation when checkbox is checked, a value between
\code{smooth}, \code{jelly}, \code{tada}, \code{rotate}, \code{pulse}.}

\item{icon}{Optional, display an icon on the checkbox, must be an icon created with \code{icon}.}

\item{plain}{Remove the border when checkbox is checked (\code{TRUE} or \code{FALSE}).}

\item{bigger}{Scale the checkboxes a bit bigger (\code{TRUE} or \code{FALSE}).}

\item{inline}{If \code{TRUE}, render the choices inline (i.e. horizontally).}

\item{width}{The width of the input, e.g. \verb{400px}, or \verb{100\%}.}

\item{choiceNames}{List of names to display to the user.}

\item{choiceValues}{List of values corresponding to \code{choiceNames}}
}
\value{
HTML
}
\description{
Rewrite of shinyWidgets::prettyCheckboxGroup for accessibility fixes.
}
\examples{
a11yPrettyCheckboxGroup("help-used", "Source of help", c("Helpdesk", "Online"))
}
