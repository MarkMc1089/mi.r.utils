
#' Date range picker allowing for a custom minimum view, such as months only
#'
#' @param minviewmode Character such as "months" or "years".
#' @param init_hide Whether to show the element initially, default is FALSE.
#' @inheritParams shiny::dateRangeInput
#'
#' @return HTML
#' @export
#'
#' @examples \dontrun{
#'
#' }
# Begin Exclude Linting
dateRangeMonthsInput <- function(inputId, label = NULL, start = NULL, end = NULL, min = NULL, max = NULL,
                                 format = "yyyy-mm-dd", startview = "month", minviewmode = "months",
                                 weekstart = 0, language = "en", separator = " to ", width = NULL,
                                 init_hide = FALSE, autoclose = TRUE) {
  # End Exclude Linting
  # If start and end are date objects, convert to a string with yyyy-mm-dd format
  # Same for min and max
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
      # input-daterange class is needed for dropdown behavior
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
          `data-date-min-view-mode` = minviewmode, # added manually
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
          `data-date-min-view-mode` = minviewmode, # added manually
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

`%AND%` <- function(x, y) { # Exclude Linting
  if (!is.null(x) && !is.na(x)) {
    if (!is.null(y) && !is.na(y)) {
      return(y)
    }
  }
  return(NULL)
}

controlLabel <- function(controlName, label) { # Exclude Linting
  if (!is.null(label)) {
    label %AND%
      label(class = "control-label", `for` = controlName, label)
  }
}

# the datePickerDependency is taken from
# https://github.com/rstudio/shiny/blob/master/R/input-date.R
datePickerDependency <- htmlDependency( # Exclude Linting
  "bootstrap-datepicker", "1.6.4", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/bootstrap-datepicker3.min.css",
  # Need to enable noConflict mode. See https://github.com/rstudio/shiny/issues/1346.
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
#' @param init_hide Boolean
#'
#' @return HTML
#' @export
#'
#' @examples \dontrun{
#'
#' }
select_input_from_months <- function(id, month_data, init_hide = FALSE) {
  maybe_hide <- if (init_hide) {
    "display: none; "
  } else {
    ""
  }

  s <- selectInput(
    id,
    "",
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

  s$attribs[["style"]] <- maybe_hide

  s
}

# Prepend + symbol to numbers
#' Title
#'
#' @param x Numeric
#' @param ... Character
#'
#' @return Character
#' @export
#'
#' @examples \dontrun{
#'
#' }
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


# Use like x %||% y. If x is null, return y. If x is not null, return x.
`%||%` <- function(x, y) if (is_null(x)) y else x


#' Set number of decimal places of number and return as char.
#'
#' @param x Int
#' @param k Int
#'
#' @return Character
#' @export
#'
#' @examples \dontrun{
#'
#' }
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
#' @examples \dontrun{
#'
#' }
date_Ymd_to_bY <- function(date_to_format) { # Exclude Linting
  format(date_to_format, format = "%b %Y")
}


#' Get number of months between 2 dates. A partial month counts as one month.
#'
#' @param d1 Date or character parseable as YMD date
#' @param d2 Date or character parseable as YMD date
#'
#' @return Int
#' @export
#'
#' @examples \dontrun{
#'
#' }
month_diff <- function(d1, d2 = NULL) {
  if (is.null(d2)) {
    d2 <- d1[2]
    d1 <- d1[1]
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
#' @examples \dontrun{
#'
#' }
colour_ramp <- function(n_colours, first = "#003087", last = "#FFFFFF",
                        include_white = FALSE) {
  palette <- colorRampPalette(c(first, last))

  if (include_white) c(palette(n_colours + 1)) else c(palette(n_colours + 1)[1:n_colours])
}


#' Create large standard error arrow if sig diff found
#'
#' @param base1 Int
#' @param per1 Numeric
#' @param base2 Int
#' @param per2 Numeric
#'
#' @return Character
#' @export
#'
#' @examples \dontrun{
#'
#' }
per_s_error <- function(base1, per1, base2, per2) {
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
    arrow <- "<font color = '#DA291C', size = '5'>&dArr;</font>"
  }
  if (sig_value > 1.93 && per2 > per1) {
    arrow <- "<font color = '#009639', size = '5'>&uArr;</font>"
  }
  arrow
}


#' https://www.dummies.com/education/math/statistics/how-to-compare-two-population-proportions/  # Exclude Linting
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
#' @examples \dontrun{
#'
#' }
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
#' @param base1 Int
#' @param per1 Numeric
#' @param base2 Int
#' @param per2 Numeric
#'
#' @return Character
#' @export
#'
#' @examples \dontrun{
#'
#' }
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
#' @examples \dontrun{
#'
#' }
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
#' @examples \dontrun{
#'
#' }
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
#' @param output_fun Function
#' @param output_name Character
#'
#' @return HTML
#' @export
#'
#' @examples \dontrun{
#'
#' }
fluidRow_2cols_toggle_B <- function(output_fun, output_name) { # Exclude Linting
  fluidRow(
    id = output_name,
    div(class = "groupA col-sm-12", output_fun(glue("{output_name}_A"))),
    div(class = "groupB hidden", output_fun(glue("{output_name}_B")))
  )
}


#' Create Shiny input for selecting NPS groups
#'
#' @param id Character
#'
#' @return HTML
#' @export
#'
#' @examples \dontrun{
#'
#' }
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


#' Create info tooltip to be used with tippy.js
#'
#' @param content HTML elements
#' @param style Character
#'
#' @return HTML
#' @export
#'
#' @examples \dontrun{
#'
#' }
info_tooltip <- function(content,
                         style = glue("display: inline; padding-left: 1px; font-size: \\
                                      1 rem; font-weight: 400")) {
  span(
    class = "info-tippy",
    style = style,
    `data-tippy-content` = content,
    HTML("&#x1F6C8;")
  )
}


#' Accessibility compliant Shiny radio buttons
#'
#' @inheritParams shiny::radioButtons
#'
#' @return HTML
#' @export
#'
#' @examples \dontrun{
#'
#' }
# Begin Exclude Linting
a11yRadioButtons <- function(inputId, label, choices = NULL, selected = NULL,
                             inline = FALSE, width = NULL, choiceNames = NULL,
                             choiceValues = NULL) {
  # End Exclude Linting
  args <- shiny:::normalizeChoicesArgs(choices, choiceNames, choiceValues)
  selected <- restoreInput(id = inputId, default = selected)
  selected <- if (is.null(selected)) args$choiceValues[[1]] else as.character(selected)
  if (length(selected) > 1) stop("The 'selected' argument must be of length 1")
  options <- shiny:::generateOptions(
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
#' @param type Type of input, such as checkbox
#' @inheritParams shinyWidgets::prettyCheckboxGroup
#'
#' @return HTML
#' @export
#'
#' @examples \dontrun{
#'
#' }
# Begin Exclude Linting
a11yGeneratePretty <- function(inputId, selected, inline, type = "checkbox",
                               choiceNames, choiceValues, status = "primary",
                               shape = "square", outline = FALSE, fill = FALSE,
                               thick = FALSE, animation = NULL, icon = NULL,
                               plain = FALSE, bigger = FALSE) {
  # End Exclude Linting
  if (!is.null(icon)) {
    icon <- shinyWidgets:::validateIcon(icon)
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
#' @inheritParams shinyWidgets::prettyCheckboxGroup
#'
#' @return HTML
#' @export
#'
#' @examples \dontrun{
#'
#' }
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
  args <- shinyWidgets:::normalizeChoicesArgs(choices, choiceNames, choiceValues)
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
  shinyWidgets:::attachShinyWidgetsDep(checkgroupTag, "pretty")
}


#' Accessibility compliant version of shinyBS::bsCollapse
#'
#' @inheritParams shinyBS::bsCollapse
#'
#' @return HTML
#' @export
#'
#' @examples \dontrun{
#'
#' }
a11yBsCollapse <- function(id, open, ...) { # Exclude Linting
  tagQuery(
    bsCollapse(
      id = id,
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
#' warning messages
#'
#' @param .expr Code
#' @param .f Function
#' @param ... Vector of characters
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
suppress_warnings <- function(.expr, .f, ...) {
  eval.parent(substitute(
    withCallingHandlers(.expr, warning = function(w) {
      cm <- conditionMessage(w)
      cond <-
        if (is.character(.f)) grepl(.f, cm) else as_function(.f)(cm, ...)
      if (cond) {
        invokeRestart("muffleWarning")
      }
    })
  ))
}
