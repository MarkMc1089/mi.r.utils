#' Customised date range picker
#'
#' Creates a pair of text inputs which, when clicked on, bring up calendars that
#' the user can click on to select dates. This is a customised version of the
#' `{shiny::dateRangeInput}` that allows for a minimum view, such as months
#' only. It can also be hidden on initialisation.
#'
#' @param minviewmode Character such as "months" or "years".
#' @param init_hide Whether to show the element initially, default is FALSE.
#' @inherit shiny::dateRangeInput
#'
#' @return HTML
#'
#' @export
#'
#' @examples \dontrun{
#' library(shiny)
#'
#' ui <- fluidPage(
#'   dateRangeInputWithViewMode("daterange1", "Date range:",
#'                              start = "2001-01-01",
#'                              end   = "2010-12-31"),
#'
#'   # Default start and end is the current date in the client's time zone
#'   dateRangeInputWithViewMode("daterange2", "Date range:"),
#'
#'   # start and end are always specified in yyyy-mm-dd, even if the display
#'   # format is different
#'   dateRangeInputWithViewMode("daterange3", "Date range:",
#'                              start  = "2001-01-01",
#'                              end    = "2010-12-31",
#'                              min    = "2001-01-01",
#'                              max    = "2012-12-21",
#'                              format = "mm/dd/yy",
#'                              separator = " - "),
#'
#'   # Pass in Date objects
#'   dateRangeInputWithViewMode("daterange4", "Date range:",
#'                              start = Sys.Date()-10,
#'                              end = Sys.Date()+10),
#'
#'   # Use different language and different first day of week
#'   dateRangeInputWithViewMode("daterange5", "Date range:",
#'                              language = "de",
#'                              weekstart = 1),
#'
#'   # Start with decade view instead of default month view
#'   dateRangeInputWithViewMode("daterange6", "Date range:",
#'                              startview = "decade"),
#'
#'   # Set the minimum viewmode to days
#'   dateRangeInputWithViewMode("daterange7", "Date range:",
#'                              minviewmode = "days")
#' )
#'
#' shinyApp(ui, server = function(input, output) { })
#' }
# Begin Exclude Linting
dateRangeInputWithViewMode <- function(inputId,
                                       label = NULL,
                                       start = NULL,
                                       end = NULL,
                                       min = NULL,
                                       max = NULL,
                                       format = "yyyy-mm-dd",
                                       startview = "month",
                                       minviewmode = "months",
                                       weekstart = 0,
                                       language = "en",
                                       separator = " to ",
                                       width = NULL,
                                       init_hide = FALSE,
                                       autoclose = TRUE) {
  # End Exclude Linting
  # If start and end are date objects, convert to a string with yyyy-mm-dd
  # format Same for min and max
  if (inherits(start, "Date")) start <- format(start, "%Y-%m-%d")
  if (inherits(end, "Date")) end <- format(end, "%Y-%m-%d")
  if (inherits(min, "Date")) min <- format(min, "%Y-%m-%d")
  if (inherits(max, "Date")) max <- format(max, "%Y-%m-%d")

  maybe_hide <- if (init_hide) "display: none; " else ""

  attachDependencies(
    div(
      id = inputId,
      class = "shiny-date-range-input form-group shiny-input-container",
      style = if (!is.null(width)) {
        glue("width: {validateCssUnit(width)}; {maybe_hide}")
      } else {
        maybe_hide
      },
      controlLabel(inputId, label),
      # input-daterange class is needed for dropdown behaviour
      div(
        class = "input-daterange input-group",
        tags$input(
          class = "input-sm form-control",
          type = "text",
          `aria-labelledby` = paste0(inputId, "-label"),
          `data-date-language` = language,
          `data-date-weekstart` = weekstart,
          `data-date-format` = format,
          `data-date-start-view` = startview,
          `data-date-min-view-mode` = minviewmode,
          `data-min-date` = min,
          `data-max-date` = max,
          `data-initial-date` = start,
          `data-date-autoclose` = if (autoclose) "true" else "false"
        ),
        span(class = "input-group-addon", separator),
        tags$input(
          class = "input-sm form-control",
          type = "text",
          `aria-labelledby` = paste0(inputId, "-label"),
          `data-date-language` = language,
          `data-date-weekstart` = weekstart,
          `data-date-format` = format,
          `data-date-start-view` = startview,
          `data-date-min-view-mode` = minviewmode,
          `data-min-date` = min,
          `data-max-date` = max,
          `data-initial-date` = end,
          `data-date-autoclose` = if (autoclose) "true" else "false"
        )
      )
    ),
    datePickerDependency
  )
}


#' Helper that will return NULL if at least one of LHS and RHS is NA or NULL, or
#' it will return the RHS otherwise.
#'
#' @param LHS Anything
#' @param RHS Anything
#'
#' @return RHS or NULL
#'
#' @export
#'
#' @examples
#' 1 %AND% 1:3   # 1 2 3
#' NA %AND% 1:3 # NULL
`%AND%` <- function(LHS, RHS) { # Exclude Linting
  if (!all(is.null(LHS)) && !all(is.na(LHS))) {
    if (!all(is.null(RHS)) && !all(is.na(RHS))) {
      return(RHS)
    }
  }
  return(NULL)
}

#' Construct accessible label
#'
#' @param controlName Character, id of an html tag
#' @param label Character, displayed label
#'
#' @return HTML for label or NULL when label is NULL
#'
#' @export
#'
#' @examples
#' controlLabel("my_id", "My label:")
controlLabel <- function(controlName, label) { # Exclude Linting
  if (!is.null(controlName) & !is.na(controlName)) {
    label %AND%
      label(class = "control-label", `for` = controlName, label)
  } else NULL
}


#' Customised dependency based on `shiny:::datePickerDependency`
#'
#' Customised version of
#' https://github.com/rstudio/shiny/blob/master/R/input-date.R
#'
#' @return An object that can be included in a list of dependencies passed to
#'   `htmltools::attachDependencies()`.
#'
#' @noRd
datePickerDependency <- htmlDependency( # Exclude Linting
  "bootstrap-datepicker", "1.6.4", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/bootstrap-datepicker3.min.css",
  # Need to enable noConflict mode. See
  # https://github.com/rstudio/shiny/issues/1346.
  head = "<script>
           (function() {
             var datepicker = $.fn.datepicker.noConflict();
             $.fn.bsDatepicker = datepicker;
             $.fn.bsDatepicker.defaults.autoclose = true;
           })();
         </script>"
)


#' Create select input with choices corresponding to months in data.
#'
#' @param id Character
#' @param month_data Data
#' @inheritParams shiny::selectInput
#'
#' @return HTML
#'
#' @export
#'
#' @examples
#' select_input_from_months(
#'   "month-select",
#'   "Month:",
#'   c("2023/01/01", "2023/02/01", "2023/03/01")
#' )
select_input_from_months <- function(id, label, month_data, ...) {
  selectInput(
    id,
    label,
    c(setNames(
      as.character(
        sort(
          unique(month_data),
          decreasing = TRUE
        )
      ),
      date_Ymd_to_bY(
        sort(
          unique(month_data),
          decreasing = TRUE
        )
      )
    )),
  )
}


# Prepend + symbol to numbers
#' Title
#'
#' @param x Numeric
#' @inheritParams base::format
#'
#' @return Character
#' @export
#'
#' @examples
#' with_plus(3.142, digits = 2)
with_plus <- function(x, ...) {
  if (x > 0) {
    sprintf(
      fmt = "+%s",
      format(x, ...)
    )
  } else {
    x
  }
}


#' Helper that is similar to COALESCE in SQL. Will return LHS if it is not NULL
#' or NA. It will return the RHS otherwise.
#'
#' @param LHS Anything
#' @param RHS Anything
#'
#' @return LHS or RHS
#'
#' @noRd
#'
#' @examples
#' 1 %||% 2   # 1
#' NA %||% 2 # 2
`%||%` <- function(LHS, RHS) if (is.null(LHS)) RHS else LHS


#' Set number of decimal places of number and return as char.
#'
#' @param x Int
#' @param k Int
#'
#' @return Character
#' @export
#'
#' @examples
#' decimal_places(3.1, 3)
decimal_places <- function(x, k) {
  trimws(format(round(x, k), nsmall = k))
}


#' Set format of date to abbreviated month and full year.
#'
#' @param date_to_format Date
#'
#' @return Character
#' @export
#'
#' @examples
#' date_Ymd_to_bY(as.Date("2023/03/23"))
date_Ymd_to_bY <- function(date_to_format) { # Exclude Linting
  format(date_to_format, format = "%b %Y")
}


#' Get number of months between 2 dates. A partial month counts as one month.
#'
#' @param d1 Date or character parseable as YMD date, or a 2 element vector or
#'   list of the same.
#' @param d2 Date or character parseable as YMD date, or unspecified if using a
#'   vector or list for d1
#'
#' @return Int
#' @export
#'
#' @examples
#' month_diff("2023/02/28", "2023/05/01")
#' month_diff(c("2023/02/28", "2023/05/01"))
month_diff <- function(d1, d2 = NULL) {
  if (is.null(d2)) {
    d2 <- d1[[2]]
    d1 <- d1[[1]]
  }

  abs(12 * (as.yearmon(as.Date(d1)) - as.yearmon(as.Date(d2))))
}


#' Make colour ramp palette. Default start colour is NHS Dark Blue. To allow for
#' avoiding having white as last colour, n_colours + 1 are created.
#'
#' @param n_colours Int
#' @param first Character
#' @param last Character
#' @param include_white Boolean
#'
#' @return colorRampPalette
#' @export
#'
#' @examples
#' colour_ramp(3)
colour_ramp <- function(n_colours, first = "#003087", last = "#FFFFFF",
                        include_white = FALSE) {
  palette <- colorRampPalette(c(first, last))

  if (include_white) {
    c(palette(n_colours + 1))
  } else {
    c(palette(n_colours + 1)[1:n_colours])
  }
}


#' Create large standard error arrow if sig diff found
#'
#' You may want to avoid using this function, it is here for legacy use.
#'
#' @param base1 Int
#' @param per1 Numeric
#' @param base2 Int
#' @param per2 Numeric
#'
#' @return Character
#' @export
#'
#' @examples
#' per_s_error(101, 0.5, 99, 0.6)
per_s_error <- function(base1, per1, base2, per2) {
  # Return immediately if any arg is NA
  if (any(is.na(c(base1, per1, base2, per2)))) {
    return("")
  }

  pooled_per <-
    ((base1 * per1 / 100) + (base2 * per2 / 100)) / (base1 + base2)
  s_error <-
    sqrt((pooled_per * (1 - pooled_per)) * ((1 / base1) + (1 / base2)))
  sig_value <- abs((per1 / 100 - per2 / 100) / s_error)
  arrow <- ""
  if (sig_value > 1.93 && per2 < per1) {
    arrow <- "<font color = '#DA291C', size = '5'>&dArr;</font>"
  }
  if (sig_value > 1.93 && per2 > per1) {
    arrow <- "<font color = '#009639', size = '5'>&uArr;</font>"
  }
  arrow
}


#' Create small standard error arrow if sig diff found
#'
#' This is a vectorised replacement for per_s_error_small.
#' You may want to avoid using this function, it is here for legacy use.
#' Also, https://www.dummies.com/education/math/statistics/how-to-compare-two-population-proportions/  # Exclude Linting
#' Why use 1.93? It seems this corresponds to a confidence interval of ~94.64%.
#' Should it not be 1.96?
#'
#' @param base1 Int
#' @param per1 Numeric
#' @param base2 Int
#' @param per2 Numeric
#'
#' @return Character
#' @export
#'
#' @examples
#' base1 <- c(100, 90)
#' per1 <- c(0.5, 0.4)
#' base2 <- c(110, 0.6)
#' per2 <- c(120, 0.3)
#' per_s_error_vect(base1, per1, base2, per2)
per_s_error_vect <- function(base1, per1, base2, per2) {
  if (anyNA(c(base1, per1, base2, per2))) {
    return("")
  }

  pooled_per <- (base1 * per1 + base2 * per2) / (100 * (base1 + base2))
  s_error <- sqrt((pooled_per * (1 - pooled_per)) * ((1 / base1) + (1 / base2)))
  sig_value <- abs((per1 - per2) / (100 * s_error))

  if_else(
    sig_value > 1.93,
    if_else(
      per2 < per1,
      "<font color = '#DA291C', size = '3'>&#x25BC;</font>",
      "<font color = '#009639', size = '3'>&#x25B2;</font>"
    ),
    ""
  )
}


#' Create small standard error arrow if sig diff found
#'
#' You may want to avoid using this function, it is here for legacy use.
#'
#' @param base1 Int
#' @param per1 Numeric
#' @param base2 Int
#' @param per2 Numeric
#'
#' @return Character
#' @export
#'
#' @examples
#' per_s_error_small(101, 0.5, 99, 0.6)
per_s_error_small <- function(base1, per1, base2, per2) {
  # Return immediately if any arg is NA
  if (any(is.na(c(base1, per1, base2, per2)))) {
    return("")
  }

  base1 <- base1
  per1 <- per1
  base2 <- base2
  per2 <- per2
  pooled_per <-
    ((base1 * per1 / 100) + (base2 * per2 / 100)) / (base1 + base2)
  s_error <-
    sqrt((pooled_per * (1 - pooled_per)) * ((1 / base1) + (1 / base2)))
  sig_value <- abs((per1 / 100 - per2 / 100) / s_error)

  arrow <- ""
  if (sig_value > 1.93 && per2 < per1) {
    arrow <- "<font color = '#DA291C', size = '3'>&dArr;</font>"
  }
  if (sig_value > 1.93 && per2 > per1) {
    arrow <- "<font color = '#009639', size = '3'>&uArr;</font>"
  }
  arrow
}


#' Wrap given tags in a content box
#'
#' @param title Character
#' @param ... List of HTML tags
#'
#' @return HTML
#' @export
#'
#' @examples
#' content_box(
#'   "A box...",
#'   "...with some...",
#'   "...things inside."
#' )
content_box <- function(title, ...) {
  div(
    class = "sixteen wide column",
    style = "overflow: visible!important;",
    div(
      class = "sixteen wide column",
      div(
        class = "column",
        div(
          class = "ui segment",
          h2(title),
          br(),
          br(),
          ...
        )
      )
    )
  )
}


#' Style text as label
#'
#' @param label_text Character
#' @param ... HTML elements
#'
#' @return HTML
#' @export
#'
#' @examples
#' library(shiny)
#'
#' label("A label", p("A paragraph"), p("Another paragraph"))
label <- function(label_text, ...) {
  div(
    style = glue(
      "text-align:center;color:#333333;font-size:18px;font-weight:700;fill:#333333;"
    ),
    label_text,
    ...
  )
}


#' Create Shiny output widgets in 2 columns, with 2nd column initially hidden.
#'
#' This is used for the region selection functionality in MI dashboards. The 'B'
#' column is toggleable, based on whether there are any regions selected for
#' comparison.
#'
#' @param output_fun Function
#' @param output_name Character
#' @param ... Additional args passed to output_fun
#'
#' @return HTML
#' @export
#'
#' @examples
#' fluidRow_2cols_toggle_B(shiny::uiOutput, "some_output")
fluidRow_2cols_toggle_B <- function(output_fun, output_name, ...) { # Exclude Linting
  div(
    fluidRow(
      id = output_name,
      div(class = "groupA col-sm-12", output_fun(glue("{output_name}_A"), ...)),
      div(class = "groupB hidden", output_fun(glue("{output_name}_B"), ...))
    ),
    br(),
    br()
  )
}


#' Create Shiny input for selecting NPS groups
#'
#' @param id Character
#'
#' @return HTML
#' @export
#'
#' @examples
#' user_group_select("nps_group")
user_group_select <- function(id) {
  fluidRow(
    column(9),
    column(
      3,
      selectInput(
        id,
        "Select rating group:",
        c("All", "Detractor", "Passive", "Promoter"),
        selectize = FALSE
      )
    )
  )
}


#' Create Shiny input for selecting from multiple similar aspects in stacked
#' vertical charts
#'
#' @param id Character
#' @param choices Vector of characters
#'
#' @return HTML
#' @export
#'
#' @examples
#' aspect_radio_select("help-used", c("Helpdesk", "Online"))
aspect_radio_select <- function(id, choices) {
  a11yRadioButtons(
    id,
    "Aspect",
    choices = choices,
    inline = TRUE
  )
}


#' Create info tooltip to be used with tippy.js
#'
#' @param content HTML elements
#' @param style Character
#'
#' @return HTML
#' @export
#'
#' @examples
#' info_tooltip(label("A label", shiny::uiOutput("some_output")))
info_tooltip <- function(content, style = NULL) {
  if (is.null(style)) {
    style <- glue(
      "display: inline; padding-left: 1px; font-size: 1 rem; font-weight: 400"
    )
  }

  span(
    class = "info-tippy",
    style = style,
    `data-tippy-content` = content,
    HTML("&#x1F6C8;")
  )
}


#' Accessibility compliant Shiny radio buttons
#'
#' Rewrite of shiny::radioButtons for accessibility fixes
#'
#' @inheritParams shiny::radioButtons
#'
#' @return HTML
#' @export
#'
#' @examples
#' a11yRadioButtons("help-used", "Source of help", c("Helpdesk", "Online"))
a11yRadioButtons <- function(inputId, label, choices = NULL, selected = NULL,
                             inline = FALSE, width = NULL, choiceNames = NULL,
                             choiceValues = NULL) {
  # End Exclude Linting
  args <- do.call(":::", list("shiny", "normalizeChoicesArgs"))(choices, choiceNames, choiceValues)
  selected <- restoreInput(id = inputId, default = selected)
  selected <- if (is.null(selected)) args$choiceValues[[1]] else as.character(selected)
  if (length(selected) > 1) stop("The 'selected' argument must be of length 1")
  options <- do.call(":::", list("shiny", "generateOptions"))(
    inputId, selected, inline, "radio", args$choiceNames, args$choiceValues
  )
  divClass <- "form-group shiny-input-radiogroup shiny-input-container" # Exclude Linting
  if (inline) divClass <- paste(divClass, "shiny-input-container-inline") # Exclude Linting
  tags$fieldset(
    tags$legend(label),
    div(
      id = inputId,
      style = css(width = validateCssUnit(width)),
      class = divClass,
      role = "radiogroup",
      options
    )
  )
}


#' Accessibility compliant version of shinyWidgets:::generatePretty
#'
#' Rewrite of shinyWidgets:::generatePretty for accessibility fixes.
#'
#' @param type Type of input, such as checkbox
#' @inheritParams shinyWidgets::prettyCheckboxGroup
#'
#' @return HTML
#'
#' @noRd
# Begin Exclude Linting
a11yGeneratePretty <- function(inputId, selected, inline, type = "checkbox",
                               choiceNames, choiceValues, status = "primary",
                               shape = "square", outline = FALSE, fill = FALSE,
                               thick = FALSE, animation = NULL, icon = NULL,
                               plain = FALSE, bigger = FALSE) {
  # End Exclude Linting
  if (!is.null(icon)) {
    icon <- do.call(":::", list("shinyWidgets", "validateIcon"))(icon)
    icon$attribs$class <- paste("icon", icon$attribs$class)
  }
  options <- mapply(
    choiceValues,
    choiceNames,
    FUN = function(value, name) {
      inputTag <- tags$input( # Exclude Linting
        type = type,
        name = inputId,
        value = value,
        `aria-labelledby` = paste0(to_snake_case(value), "_label")
      )
      if (name == "") name <- "empty"
      label_style <- if (name == "empty") "display: none;" else ""
      if (value %in% selected) inputTag$attribs$checked <- "checked" # Exclude Linting
      if (inline) {
        div(
          class = "pretty",
          inputTag,
          class = if (is.null(icon)) "p-default",
          class = if (plain) "p-plain",
          class = if (bigger) "p-bigger",
          class = if (shape != "square") paste0("p-", shape),
          class = if (fill) "p-fill",
          class = if (thick) "p-thick",
          class = if (!is.null(animation)) paste0("p-", animation),
          class = if (!is.null(icon)) "p-icon",
          div(
            class = "state",
            class = if (status != "default") paste0("p-", status, if (outline) "-o"),
            if (!is.null(icon)) icon,
            tags$label(
              id = paste0(to_snake_case(value), "_label"),
              span(style = label_style, name)
            )
          )
        )
      } else {
        tagList(
          div(
            class = "pretty",
            inputTag,
            class = if (is.null(icon)) "p-default",
            class = if (plain) "p-plain",
            class = if (bigger) "p-bigger",
            class = if (shape != "square") paste0("p-", shape),
            class = if (fill) "p-fill",
            class = if (thick) "p-thick",
            class = if (!is.null(animation)) paste0("p-", animation),
            class = if (!is.null(icon)) "p-icon",
            div(
              class = "state",
              class = if (status != "default") paste0("p-", status, if (outline) "-o"),
              if (!is.null(icon)) icon,
              tags$label(
                id = paste0(to_snake_case(value), "_label"),
                span(style = label_style, name)
              )
            )
          ),
          div(style = "height:3px;")
        )
      }
    },
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  div(
    if (!inline) div(style = "height:7px;"),
    class = "shiny-options-group",
    options
  )
}


#' Accessibility compliant version of shinyWidgets::prettyCheckboxGroup
#'
#' Rewrite of shinyWidgets::prettyCheckboxGroup for accessibility fixes.
#'
#' @inheritParams shinyWidgets::prettyCheckboxGroup
#'
#' @return HTML
#' @export
#'
#' @examples
#' a11yPrettyCheckboxGroup("help-used", "Source of help", c("Helpdesk", "Online"))
# Begin Exclude Linting
a11yPrettyCheckboxGroup <- function(inputId, label, choices = NULL,
                                    selected = NULL, status = "default",
                                    shape = c("square", "curve", "round"),
                                    outline = FALSE, fill = FALSE, thick = FALSE,
                                    animation = NULL, icon = NULL, plain = FALSE,
                                    bigger = FALSE, inline = FALSE, width = NULL,
                                    choiceNames = NULL, choiceValues = NULL) {
  # End Exclude Linting
  status <- match.arg(
    status,
    c("default", "primary", "success", "info", "danger", "warning")
  )
  shape <- match.arg(shape)
  if (is.null(choices) && is.null(choiceNames) && is.null(choiceValues)) {
    choices <- character(0)
  }
  args <- do.call(":::", list("shinyWidgets", "normalizeChoicesArgs"))(
    choices, choiceNames, choiceValues
  )
  selected <- restoreInput(id = inputId, default = selected)
  if (!is.null(selected)) selected <- as.character(selected)
  options <- a11yGeneratePretty(
    inputId = inputId, selected = selected, inline = inline, type = "checkbox",
    choiceNames = args$choiceNames, choiceValues = args$choiceValues,
    status = status, shape = shape, outline = outline, fill = fill, thick = thick,
    animation = animation, icon = icon, plain = plain, bigger = bigger
  )
  divClass <- "form-group shiny-input-checkboxgroup shiny-input-container" # Exclude Linting
  if (inline) divClass <- paste(divClass, "shiny-input-container-inline") # Exclude Linting
  checkgroupTag <- div( # Exclude Linting
    id = inputId,
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    class = divClass,
    tags$fieldset(tags$legend(style = "display: none;", label), options)
  )
  do.call(":::", list("shinyWidgets", "attachShinyWidgetsDep"))(checkgroupTag, "pretty")
}


#' Accessibility compliant version of shinyWidgets::updatePrettyCheckboxGroup
#'
#' Rewrite of shinyWidgets::updatePrettyCheckboxGroup for accessibility fixes.
#'
#' @inheritParams shinyWidgets::updatePrettyCheckboxGroup
#'
#' @return HTML
#' @export
#'
#' @examples \dontrun{
#' updateA11yPrettyCheckboxGroup(
#'   inputId = "help-used",
#'   choices = c("Helpdesk", "Online", "Some new help channel")
#' )
#' }
# Begin Exclude Linting
updateA11yPrettyCheckboxGroup <- function(session = getDefaultReactiveDomain(),
                                          inputId, label = NULL, choices = NULL,
                                          selected = NULL, inline = FALSE,
                                          choiceNames = NULL, choiceValues = NULL,
                                          prettyOptions = list()) {
  # End Exclude Linting
  updateA11yPrettyOptions(
    session, inputId, label, choices, selected, inline, "checkbox",
    choiceNames, choiceValues, prettyOptions
  )
}


#' Accessibility compliant version of shinyWidgets:::updatePrettyOptions
#'
#' Rewrite of shinyWidgets:::updatePrettyOptions for accessibility fixes.
#'
#' @param session	The session object passed to function given to shinyServer.
#' @param inputId	The id of the input object.
#' @param label	The label to set for the input object.
#' @param  choices	The choices to set for the input object, updating choices will
#'  reset parameters like status, shape, ... on the checkboxes, you can re-specify
#'   (or change them) in argument prettyOptions.
#' @param selected The value to set for the input object.
#' @param inline	If TRUE, render the choices inline (i.e. horizontally).
#' @param type One of 'checkbox' or 'radio'.
#' @param choiceNames	The choices names to set for the input object.
#' @param choiceValues The choices values to set for the input object.
#' @param prettyOptions Arguments passed to prettyCheckboxGroup for styling checkboxes.
#'  This can be needed if you update choices.
#'
#' @return HTML
#'
#' @noRd
# Begin Exclude Linting
updateA11yPrettyOptions <- function(session = getDefaultReactiveDomain(), inputId,
                                    label = NULL, choices = NULL, selected = NULL,
                                    inline = FALSE, type = NULL, choiceNames = NULL,
                                    choiceValues = NULL, prettyOptions = list()) {
  # End Exclude Linting
  if (is.null(type)) {
    stop("Please specify the type ('checkbox' or 'radio')")
  }
  args <- do.call(":::", list("shinyWidgets", "normalizeChoicesArgs"))(
    choices, choiceNames, choiceValues,
    mustExist = FALSE
  )
  if (!is.null(selected)) {
    selected <- as.character(selected)
  }

  options <- if (!is.null(args$choiceValues)) {
    doRenderTags(
      a11yGeneratePretty(
        inputId = session$ns(inputId),
        selected = selected,
        inline = inline,
        type = type,
        choiceNames = args$choiceNames,
        choiceValues = args$choiceValues,
        status = prettyOptions$status %||% "default",
        shape = prettyOptions$shape %||% "square",
        outline = prettyOptions$outline %||% FALSE,
        fill = prettyOptions$fill %||% FALSE,
        thick = prettyOptions$thick %||% FALSE,
        animation = prettyOptions$animation,
        icon = prettyOptions$icon,
        plain = prettyOptions$plain %||% FALSE,
        bigger = prettyOptions$bigger %||% FALSE
      )
    )
  }
  message <- do.call(":::", list("shinyWidgets", "dropNulls"))(
    list(label = label, options = options, value = selected)
  )
  session$sendInputMessage(inputId, message)
}


#' Accessibility compliant version of shinyBS::bsCollapse
#'
#' Rewrite of shinyWidgets::bsCollapse for accessibility fixes.
#'
#' @inheritParams shinyBS::bsCollapse
#'
#' @return HTML
#' @export
#'
#' @examples
#' library(shinyBS)
#'
#' a11yBsCollapse(
#'   id = "collapseExample",
#'   open = "Panel 2",
#'   bsCollapsePanel(
#'     "Panel 1",
#'     "This is a panel with just text and has the default style."
#'   ),
#'   bsCollapsePanel(
#'     "Panel 2",
#'     "This panel is open initially and has a 'success' style.",
#'     style = "success"
#'   )
#' )
a11yBsCollapse <- function(..., id = NULL, multiple = FALSE, open = NULL) { # Exclude Linting
  tagQuery(
    bsCollapse(
      id = id,
      multiple = multiple,
      open = open,
      ...
    )
  )$
    find(".panel-heading")$
    each(function(x, i) {
    x$children[[1]] <- h3(class = "panel-title", x$children[[1]]$children)
  })$
    allTags()
}


#' Selectively suppress warnings using a given function to check for content in
#' warning messages, or a character string to look for in the warning message
#'
#' The purpose of this is to prevent known, i.e. expected, warnings from
#' appearing. This is helpful as you then know if you see red text when running
#' some code that it is something unexpected!
#'
#' @param .expr Code
#' @param .f Function or character string
#' @param ... Vector of characters
#'
#' @return Used for side effects
#' @export
#'
#' @examples
#' suppress_warnings({
#'     as.numeric(c("18", "30", "50+", "345,678"))
#'   },
#'   endsWith,
#'   "by coercion"
#' )
suppress_warnings <- function(.expr, .f, ...) {
  eval.parent(substitute(
    withCallingHandlers(.expr, warning = function(w) {
      cm <- conditionMessage(w)
      cond <- if (is.character(.f)) grepl(.f, cm) else .f(cm, ...)
      if (cond) {
        invokeRestart("muffleWarning")
      }
    })
  ))
}


#' Calculate whether NPS score has significantly changed between 2 samples.
#'
#' @description A margin of error is calculated for each sample, from the number
#' of promoters, neutrals (i.e. passives) and detractors. The standard error of
#' their difference is estimated using the Pythagorean formula, and the absolute
#' difference of the two samples is compared to this multiplied by the critical
#' value (aka z*-value).
#'
#' The return value is in (-1, 0, +1), according to whether a significant
#' decrease is found, no significant change, or a significant increase,
#' respectively. If the total for a sample is 0, then 0 is returned.
#'
#' Formula is based on the one found in this blog post:
#' (https://www.genroe.com/blog/how-to-calculate-margin-of-error-and-other-stats-for-nps/5994).
#'
#' @param p_0 Number of Promoters in latest sample
#' @param n_0 Number of Neutrals in latest sample
#' @param d_0 Number of Detractors in latest sample
#' @param p_1 Number of Promoters in oldest sample
#' @param n_1 Number of Neutrals in oldest sample
#' @param d_1 Number of Detractors in oldest sample
#' @param z_val Critical value multiplier; 1.96 by default for a 95% confidence
#' interval. See [this table](http://www.ltcconline.net/greenl/courses/201/estimation/smallConfLevelTable.htm)
#' for further values of z_val for common confidence intervals.
#'
#' @return A value in (-1, 0, +1); see notes above.
#' @export
#'
#' @examples
#' # Test with a 99% confidence interval
#' nps_moe_test(123, 456, 789, 321, 654, 987, z_val = 2.58)
nps_moe_test <- function(p_0, n_0, d_0,
                         p_1, n_1, d_1,
                         z_val = 1.96) {
  if (NA %in% c(p_0, n_0, d_0, p_1, n_1, d_1)) {
    return(0)
  }

  t_0 <- p_0 + n_0 + d_0
  if (t_0 == 0) {
    return(0)
  }
  nps_0 <- (p_0 - d_0) / t_0
  t_1 <- p_1 + n_1 + d_1
  if (t_1 == 0) {
    return(0)
  }
  nps_1 <- (p_1 - d_1) / t_1

  var_0 <- ((1 - nps_0)^2 * p_0 + nps_0^2 * n_0 + (-1 - nps_0)^2 * d_0) / t_0
  var_1 <- ((1 - nps_1)^2 * p_1 + nps_1^2 * n_1 + (-1 - nps_1)^2 * d_1) / t_1

  se_0 <- sqrt(var_0 / t_0)
  se_1 <- sqrt(var_1 / t_1)

  if (abs(nps_0 - nps_1) > z_val * sqrt(se_0^2 + se_1^2)) {
    if (nps_0 > nps_1) {
      return(1)
    }
    return(-1)
  } else {
    0
  }
}
