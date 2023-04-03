
#' Create string showing month range
#'
#' @param month_range List of dates
#'
#' @return Character
#' @export
#'
#' @examples \dontrun{
#'
#' }
month_range_indicator <- function(month_range) {
  if (month_range[1] == month_range[2]) {
    format(month_range[1], format = "%B %Y")
  } else {
    glue("{date_Ymd_to_bY(month_range[1])} - {date_Ymd_to_bY(month_range[2])}")
  }
}


#' Time series chart
#'
#' @param data Data
#' @param is_percentage Boolean
#' @param units Character
#' @param show_prev_fy Boolean
#' @param prev_fy_start Date
#' @param prev_fy_end Date
#' @param this_fy_start Date
#' @param this_fy_end Date
#'
#' @return Highcharts object
#' @export
#'
#' @examples \dontrun{
#'
#' }
timeseries <- function(data, is_percentage = FALSE, units = "",
                       show_prev_fy = TRUE, prev_fy_start, prev_fy_end,
                       this_fy_start, this_fy_end) {
  data <- data %>%
    mutate(
      units = units,
      Value = as.numeric(.data$Value)
    ) %>%
    suppressWarnings()

  if (is_percentage) data <- data %>% mutate(Value = .data$Value * 100)

  hc <- highchart()

  if (show_prev_fy) {
    data_prev_fy <- data %>%
      filter(between(.data$Month, prev_fy_start, prev_fy_end)) %>%
      mutate(Month = .data$Month %m+% years(1))

    hc <- hc %>%
      hc_add_series(
        type = "line",
        name = "Previous financial year",
        color = "#41B6E6",
        data = data_prev_fy,
        hcaes(
          x = .data$Month,
          y = .data$Value,
          u = .data$units
        )
      )
  }

  data_this_fy <- data %>% filter(between(.data$Month, this_fy_start, this_fy_end))

  hc %>%
    hc_add_series(
      type = "line",
      name = "Current financial year",
      color = "#003087",
      data = data_this_fy,
      hcaes(
        x = .data$Month,
        y = .data$Value,
        u = .data$units
      )
    ) %>%
    hc_xAxis(
      type = "datetime",
      units = list(
        list("month", list(1))
      ),
      crosshair = list(
        enabled = TRUE
      ),
      labels = list(
        rotation = -45,
        step = 1,
        formatter = JS("
          function() {
            return Highcharts.dateFormat('%b %y', this.value);
          }
        ")
      )
    ) %>%
    hc_tooltip(
      formatter = JS(
        "function () {
          // The first returned item is the header, subsequent items are the
          // points
          var options = {year: 'numeric', month: 'short'};
          var dtf = new Intl.DateTimeFormat('en-EN', options);

          return ['<b>' + dtf.format(this.x) + '</b><br>'].concat(
            this.points ?
              this.points.map(function (point) {

                var value = point.point.y
                if (value > 1000000) {
                  value = Highcharts.numberFormat((value / 1000000), 2) + 'M'
                } else if (value > 1000) {
                  value = Highcharts.numberFormat((value / 1000), 2) + 'K';
                } else {
                  value = Highcharts.numberFormat(value, 2);
                }

                var units = point.point.u
                if (units !== '') {units = ' ' + units}

                return '<span>' + point.series.name + ': <b>' + value + '</b>' +
                  units + '</span><br>';
                }) : []
          );
        }"
      ),
      shared = TRUE,
      useHTML = TRUE
    ) %>%
    hc_plotOptions(
      line = list(
        marker = list(
          radius = 4.5,
          lineWidth = 0.75,
          lineColor = "#000",
          symbol = "circle"
        )
      ),
      series = list(
        dataLabels = list(
          enabled = FALSE,
          style = list(
            color = "black",
            fontWeight = "bolder",
            fontSize = "15px"
          )
        )
      )
    )
}


#' Use for NPS and NES line charts over time (monthly and 3 month rolling
#' average); measure should be either "NPS" or "NES"
#'
#' @param data Data
#' @param measure_column Character
#' @param month_col Character
#' @param three_month_rolling Boolean
#' @param last_valid_month_range_selection List of dates
#' @param data_prefix Character
#' @param measure_aux Data
#' @param targets Data
#' @param low_base Int
#' @param title Character
#' @param subtitle Character
#'
#' @return Highcharts object
#' @export
#'
#' @examples \dontrun{
#'
#' }
line_chart <- function(data, measure_column, month_col, three_month_rolling = FALSE,
                       last_valid_month_range_selection, # Exclude Linting
                       data_prefix = NULL, measure_aux = NULL, targets = NULL,
                       low_base = 100, title = NULL, subtitle = NULL) {
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  if (!is.null(measure_aux)) {
    validate(
      need(
        nrow(measure_aux %>%
          filter(
            between(
              !!sym(month_col),
              last_valid_month_range_selection[1],
              last_valid_month_range_selection[2]
            )
          )) > 0,
        "No data for current filter selection!"
      )
    )
  }

  if (three_month_rolling) {
    validate(
      need(
        month_diff(
          min(data[[month_col]]) %m-% months(2), max(data[[month_col]])
        ) >= 3,
        "Less than 3 months of data!"
      )
    )
  }

  nps_groups <- c(
    "Promoter",
    "Passive",
    "Detractor"
  )

  nes_responses <- c(
    "Extremely easy"             = "Extremely easy",
    "Very easy"                  = "Very easy",
    "Fairly easy"                = "Fairly easy",
    "Neither easy nor difficult" = "Neither easy nor difficult",
    "Fairly difficult"           = "Fairly difficult",
    "Very difficult"             = "Very difficult",
    "Extremely difficult"        = "Extremely difficult"
  )

  if (!is.null(data_prefix)) {
    data <- data %>%
      rename_with(
        ~ str_split(.x, glue("{data_prefix}_"), 2) %>%
          map(~ .x[2]) %>%
          unlist(),
        c(!!month_col, !!measure_column)
      )

    month_col <- str_split(month_col, glue("{data_prefix}_"), 2)[[1]][2]
    measure_column <- str_split(measure_column, glue("{data_prefix}_"), 2)[[1]][2]
  }

  measure <- switch(measure_column,
    "nps" = "NPS",
    "nes" = "NES"
  )

  chart <- data %>%
    {
      if (!is.null(measure_aux)) {
        select(., !!month_col) %>%
          unique() %>%
          left_join(
            measure_aux
          )
      } else {
        select(
          .,
          !!month_col,
          !!measure_column,
        ) %>%
          na.omit(
            !!measure_column
          ) %>%
          filter(
            !!sym(measure_column) != "",
          ) %>%
          mutate(
            group = case_when(
              !!sym(measure_column) %in% c(c(9:10), nes_responses[1:2]) ~ "Promoter",
              !!sym(measure_column) %in% c(c(7:8), nes_responses[3:4]) ~ "Passive",
              !!sym(measure_column) %in% c((1:6), nes_responses[5:7]) ~ "Detractor"
            )
          ) %>%
          group_by(
            !!sym(month_col),
            .data$group
          ) %>%
          count() %>%
          ungroup() %>%
          full_join(
            do.call(
              expand.grid,
              setNames(
                list(unique(data[[month_col]]), nps_groups),
                list(month_col, "group")
              )
            ),
            by = c(month_col, "group")
          ) %>%
          replace_na(list(n = 0)) %>%
          complete(
            .data$group,
            nesting(!!sym(month_col)),
            fill = list(n = 0)
          ) %>%
          select(
            !!month_col,
            .data$group,
            n
          ) %>%
          spread(
            .data$group,
            n
          )
      }
    } %>%
    {
      if (!is.null(targets)) {
        arrange(., !!month_col) %>%
          left_join(
            targets,
            by = month_col
          )
      } else {
        .
      }
    } %>%
    mutate(
      base = .data$Detractor + .data$Passive + .data$Promoter,
      prevDetractor = lag(.data$Detractor, 1),
      prevPassive = lag(.data$Passive, 1),
      prevPromoter = lag(.data$Promoter, 1),
    ) %>%
    {
      if (three_month_rolling) {
        mutate(
          .,
          prevDetractor2 = lag(.data$Detractor, 2),
          prevPassive2 = lag(.data$Passive, 2),
          prevPromoter2 = lag(.data$Promoter, 2),
          det3MR = .data$Detractor + .data$prevDetractor + .data$prevDetractor2,
          pas3MR = .data$Passive + .data$prevPassive + .data$prevPassive2,
          pro3MR = .data$Promoter + .data$prevPromoter + .data$prevPromoter2,
          prevdet3MR = lag(.data$det3MR, 1),
          prevpas3MR = lag(.data$pas3MR, 1),
          prevpro3MR = lag(.data$pro3MR, 1),
          base3MR = .data$Detractor + .data$Passive + .data$Promoter +
            .data$prevDetractor + .data$prevPassive + .data$prevPromoter +
            .data$prevDetractor2 + .data$prevPassive2 + .data$prevPromoter2,
          Score3MR = round(
            ((.data$pro3MR - .data$det3MR) / .data$base3MR) * 100
          )
        )
      } else {
        mutate(
          .,
          !!sym(measure_column) := round(
            ((.data$Promoter - .data$Detractor) / base) * 100
          )
        )
      }
    } %>%
    {
      if (three_month_rolling) {
        filter(
          .,
          !is.na(.data$Score3MR)
        )
      } else {
        .
      }
    }

  chart$score_sig <- ""

  if (three_month_rolling) {
    for (i in seq_len(nrow(chart))) {
      chart$score_sig[i] <- nps_moe_test(
        chart$pro3MR[i],
        chart$pas3MR[i],
        chart$det3MR[i],
        chart$prevpro3MR[i],
        chart$prevpas3MR[i],
        chart$prevdet3MR[i]
      )
    }

    x <- sym(month_col)
    y <- sym("Score3MR")
    base <- sym("base3MR")
    name <- glue("{measure} (3MR)")
  } else {
    for (i in seq_len(nrow(chart))) {
      chart$score_sig[i] <- nps_moe_test(
        chart$Promoter[i],
        chart$Passive[i],
        chart$Detractor[i],
        chart$prevPromoter[i],
        chart$prevPassive[i],
        chart$prevDetractor[i]
      )
    }

    x <- sym(month_col)
    y <- sym(measure_column)
    base <- sym("base")
    name <- measure
  }

  if (!is.null(targets)) {
    targets <- sym(glue("{measure}_Target"))
  }

  chart <- chart %>%
    mutate(
      score_sig = case_when(
        .data$score_sig == -1 ~ "<font color = '#DA291C', size = '3'>&#x25BC;</font>",
        .data$score_sig == +1 ~ "<font color = '#009639', size = '3'>&#x25B2;</font>",
        TRUE ~ ""
      ),
      maybe_lb = case_when(
        !!base < low_base ~ "*",
        TRUE ~ ""
      )
    )

  highchart() %>%
    hc_add_series(
      type = "line",
      name = as_name(name),
      color = "#005EB8",
      data = chart,
      hcaes(
        x = !!x,
        y = !!y,
        base = !!base,
        lb = .data$maybe_lb,
        sig = .data$score_sig
      ),
      dataLabels = list(
        enabled = TRUE,
        formatter = JS(
          "function () { return this.point.y + this.point.lb + this.point.sig; }"
        ),
        useHTML = TRUE
      )
    ) %>%
    {
      if (!is.null(targets)) {
        hc_add_series(
          .,
          name = "Target",
          color = "black",
          data = chart,
          type = "line",
          hcaes(
            x = !!x,
            y = !!targets,
            base = !!base,
            lb = .data$maybe_lb,
          ),
          dataLabels = list(
            enabled = FALSE
          ),
          marker = list(
            enabled = FALSE
          ),
          dashStyle = "dash"
        )
      } else {
        .
      }
    } %>%
    hc_xAxis(
      type = "datetime",
      minTickInterval = 30 * 24 * 3600 * 1000,
      labels = list(
        rotation = -45,
        step = 1,
        formatter = JS("
              function() {
                return Highcharts.dateFormat('%b %y', this.value);
              }
            ")
      )
    ) %>%
    hc_yAxis(
      max = 100,
      min = -100
    ) %>%
    hc_tooltip(
      style = list(
        zIndex = 10000
      ),
      xDateFormat = "%b '%y",
      valueDecimals = 0,
      pointFormat = paste(
        "<b>{series.name}:</b> {point.y}<br><b>Base: </b>{point.base}{point.lb}<br>"
      ),
      backgroundColor = "#E8EDEE",
      useHTML = TRUE
    ) %>%
    hc_title(
      text = title
    ) %>%
    hc_subtitle(
      text = subtitle
    ) %>%
    hc_plotOptions(
      series = list(
        events = list(
          legendItemClick = JS("function() {return false;}")
        )
      )
    ) %>%
    hc_legend(
      title = list(
        text = glue("* indicates low base size (< {low_base})"),
        style = list(
          fontSize = 14,
          fontWeight = "normal"
        )
      )
    )
}


#' Use for NPS groups barcharts (monthly and 3 month rolling average)
#'
#' @param data Data
#' @param three_month_rolling Boolean
#' @param low_base Int
#' @param month_col Character
#'
#' @return Highcharts object
#' @export
#'
#' @examples \dontrun{
#'
#' }
nps_group_chart <- function(data, three_month_rolling = FALSE, low_base = 100,
                            month_col) {
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  if (three_month_rolling) {
    validate(
      need(
        month_diff(
          min(data[[month_col]]) %m-% months(2), max(data[[month_col]])
        ) >= 3,
        "Less than 3 months of data!"
      )
    )
  }

  nps_groups <- c(
    "Promoter",
    "Passive",
    "Detractor"
  )

  chart <- data %>%
    select(
      !!month_col,
      .data$nps
    ) %>%
    na.omit(
      .data$nps
    ) %>%
    filter(
      .data$nps != ""
    ) %>%
    mutate(
      group = case_when(
        .data$nps %in% c(1:6) ~ "Detractor",
        .data$nps %in% c(7:8) ~ "Passive",
        .data$nps %in% c(9:10) ~ "Promoter"
      )
    ) %>%
    group_by(
      !!sym(month_col),
      .data$group
    ) %>%
    count() %>%
    ungroup() %>%
    complete(
      .data$group,
      nesting(!!sym(month_col)),
      fill = list(n = 0)
    ) %>%
    full_join(
      do.call(
        expand.grid,
        setNames(
          list(unique(data[[month_col]]), nps_groups),
          list(month_col, "group")
        )
      ),
      by = c(month_col, "group")
    ) %>%
    replace_na(list(n = 0)) %>%
    select(
      !!month_col,
      .data$group,
      n
    ) %>%
    spread(
      .data$group,
      n
    ) %>%
    mutate(
      across(
        -!!month_col,
        ~ round(100 * . / (.data$Detractor + .data$Passive + .data$Promoter)),
        .names = "{.col}_perc"
      )
    )

  chart <- chart %>%
    {
      if (three_month_rolling) {
        mutate(
          .,
          prevDetrator = lag(.data$Detractor, 1),
          prevPassive = lag(.data$Passive, 1),
          prevPromoter = lag(.data$Promoter, 1),
          prevDetrator2 = lag(.data$Detractor, 2),
          prevPassive2 = lag(.data$Passive, 2),
          prevPromoter2 = lag(.data$Promoter, 2),
          det3MR = .data$Detractor + .data$prevDetrator + .data$prevDetrator2,
          pas3MR = .data$Passive + .data$prevPassive + .data$prevPassive2,
          pro3MR = .data$Promoter + .data$prevPromoter + .data$prevPromoter2,
          prevdet3MR = lag(.data$det3MR, 1),
          prevpas3MR = lag(.data$pas3MR, 1),
          prevpro3MR = lag(.data$pro3MR, 1),
          base3MR = .data$Detractor + .data$Passive + .data$Promoter +
            .data$prevDetrator + .data$prevPassive + .data$prevPromoter +
            .data$prevDetrator2 + .data$prevPassive2 + .data$prevPromoter2,
          Score3MR = round(
            ((.data$pro3MR / .data$base3MR) - (.data$det3MR / .data$base3MR)) * 100
          ),
          Detractor = .data$det3MR,
          Passive = .data$pas3MR,
          Promoter = .data$pro3MR
        ) %>%
          filter(
            !is.na(.data$Score3MR)
          )
      } else {
        .
      }
    } %>%
    filter(
      !!sym(month_col) %in% sort(
        unique(data[[month_col]]),
        decreasing = TRUE
      )[1:3]
    )

  means <- data %>%
    select(
      !!month_col,
      .data$nps
    ) %>%
    na.omit(
      .data$nps
    ) %>%
    filter(
      .data$nps != ""
    ) %>%
    group_by(
      !!sym(month_col)
    ) %>%
    summarise(
      base = n(),
      mean = as.numeric(format(round(mean(.data$nps), 1), nsmall = 1))
    )
  means <- means %>%
    ungroup() %>%
    {
      if (three_month_rolling) {
        mutate(
          .,
          prevBase1 = lag(.data$base, 1),
          prevMean1 = lag(.data$mean, 1),
          prevBase2 = lag(.data$base, 2),
          prevMean2 = lag(.data$mean, 2),
        ) %>%
          filter(
            !!sym(month_col) %in% sort(
              unique(data[[month_col]]),
              decreasing = TRUE
            )[1:3]
          ) %>%
          mutate(
            mean = (
              (.data$mean * .data$base) +
                (.data$prevMean1 * .data$prevBase1) +
                (.data$prevMean2 * .data$prevBase2)
            ) /
              (.data$base + .data$prevBase1 + .data$prevBase2),
            mean = as.numeric(format(round(.data$mean, 1), nsmall = 1)),
            base = (.data$base + .data$prevBase1 + .data$prevBase2),
            label = paste0(
              format(!!sym(month_col), format = "%b %Y"),
              "<br><b>Mean score: ", .data$mean, "</b>",
              "<br><b>Base: ", .data$base, ifelse(.data$base < low_base, "*", ""), "</b>"
            )
          ) %>%
          select(
            month_col,
            .data$base,
            .data$mean,
            .data$label
          )
      } else {
        filter(
          .,
          !!sym(month_col) %in% sort(
            unique(data[[month_col]]),
            decreasing = TRUE
          )[1:3]
        ) %>%
          mutate(
            label = paste0(
              format(!!sym(month_col), format = "%b %Y"),
              "<br><b>Mean score: ", .data$mean, "</b>",
              "<br><b>Base: ", .data$base, ifelse(.data$base < low_base, "*", ""), "</b>"
            )
          )
      }
    }

  chart <- chart %>%
    left_join(
      means,
      by = month_col
    )

  highchart() %>%
    hc_add_series(
      type = "column",
      data = chart,
      color = "#DA291C",
      name = "Detractors (scored 1-6)",
      hcaes(
        x = .data$label,
        y = .data$Detractor,
        perc = .data$Detractor_perc
      ),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.perc} %"
      )
    ) %>%
    hc_add_series(
      type = "column",
      data = chart,
      color = "#ED8B00",
      name = "Passives (scored 7-8)",
      hcaes(
        x = .data$label,
        y = .data$Passive,
        perc = .data$Passive_perc
      ),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.perc} %"
      )
    ) %>%
    hc_add_series(
      type = "column",
      data = chart,
      color = "#009639",
      name = "Promoters (scored 9-10)",
      hcaes(
        x = .data$label,
        y = .data$Promoter,
        perc = .data$Promoter_perc
      ),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.perc} %"
      )
    ) %>%
    hc_xAxis(
      type = "category"
    ) %>%
    hc_tooltip(
      shared = TRUE,
      style = list(
        zIndex = 10000
      ),
      valueDecimals = 0,
      backgroundColor = "#E8EDEE",
      useHTML = FALSE
    ) %>%
    hc_legend(
      title = list(
        text = glue("* indicates low base size (< {low_base})"),
        style = list(
          fontSize = 14,
          fontWeight = "normal"
        )
      )
    )
}


#' Use for free text tables such as comments and reasons; give the column of
#' interest and the associated selectInput id to use for NPS grouping as strings.
#'
#' @param data Data
#' @param comment_column Character
#' @param month_col Character
#' @param group_select Character or NULL
#' @param comment_name_in_DT Character
#' @param data_prefix Character
#'
#' @return Datatable
#' @export
#'
#' @examples \dontrun{
#'
#' }
group_table <- function(data, comment_column, month_col, group_select = NULL,
                        comment_name_in_DT = "Reasons", data_prefix = NULL) { # Exclude Linting
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  if (!is.null(data_prefix)) {
    data <- data %>%
      rename_with(
        ~ str_split(.x, glue("{data_prefix}_"), 2) %>%
          map(~ .x[2]) %>%
          unlist(),
        c(!!month_col, !!comment_column, ends_with("_nps"))
      )

    month_col <- str_split(month_col, glue("{data_prefix}_"), 2)[[1]][2]
    comment_column <- str_split(comment_column, glue("{data_prefix}_"), 2)[[1]][2]
  }

  columns <- c(month_col, comment_column)
  if (!is.null(group_select)) columns <- c(columns, "nps")

  table <- data %>%
    select(
      all_of(columns)
    ) %>%
    na.omit() %>%
    filter(
      !!sym(comment_column) != ""
    ) %>%
    {
      if (!is.null(group_select)) {
        mutate(
          .,
          group = case_when(
            .data$nps %in% c(1:6) ~ "Detractor",
            .data$nps %in% c(7:8) ~ "Passive",
            .data$nps %in% c(9:10) ~ "Promoter"
          )
        )
      } else {
        .
      }
    }

  if (!is.null(group_select)) {
    if (group_select != "All") {
      table <- table %>%
        filter(
          .data$group == group_select
        )
    }
  }

  columns <- c(month_col, comment_column)
  if (!is.null(group_select)) columns <- c(columns, "group")

  table <- table %>%
    select(
      all_of(columns)
    ) %>%
    arrange(
      desc(!!month_col),
      !!comment_column
    ) %>%
    mutate(
      !!sym(month_col) := format(!!sym(month_col), format = "%b %Y")
    )

  colnames <- c("Month", comment_name_in_DT)
  if (!is.null(group_select)) colnames <- c(colnames, "Group")

  datatable(
    table,
    rownames = FALSE,
    colnames = colnames
  )
}


#' Use for vertical stacked barcharts showing percentages of some categorical feature.
#'
#' @param data Data
#' @param columns Vector of characters
#' @param month_col Character
#' @param radio_select Character
#' @param cols_start_with Boolean
#' @param responses Variable
#' @param keep_undisclosed Boolean
#' @param colours Vector of characters
#' @param legend_title Character
#' @param low_base Int
#' @param show_mean Boolean
#'
#' @return Plotly object
#' @export
#'
#' @examples \dontrun{
#'
#' }
stacked_vertical <- function(data, columns, month_col, radio_select = NULL,
                             cols_start_with = TRUE, responses,
                             keep_undisclosed = FALSE, colours = NULL,
                             legend_title = NULL, low_base = 100,
                             show_mean = FALSE) {
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  column_prefix <- columns

  if (!is.null(radio_select)) {
    columns <- radio_select
  } else {
    if (cols_start_with) columns <- names(data %>% select(starts_with(columns)))
  }

  filter_out <- c("", "Undisclosed")
  if (keep_undisclosed) filter_out <- c("")

  options <- data %>%
    select(
      !!month_col,
      all_of(columns)
    ) %>%
    pivot_longer(
      -!!month_col,
      names_to = "question_column",
      values_drop_na = TRUE
    ) %>%
    filter(!.data$value %in% filter_out) %>%
    mutate(
      question_column = case_when(
        length(unique(.data$question_column)) == 1 ~ .data$value,
        TRUE ~ .data$question_column
      )
    )

  if (is.null(colours)) {
    colours <- colour_ramp(length(unique(responses)))
  }

  if (!isFALSE(show_mean)) {
    means <- data %>%
      select(
        !!month_col,
        !!show_mean
      ) %>%
      na.omit(
        .data$nps
      ) %>%
      filter(
        .data$nps != ""
      ) %>%
      group_by(
        !!sym(month_col)
      ) %>%
      summarise(
        base = n(),
        mean = as.numeric(format(round(mean(.data$nps), 1), nsmall = 1))
      )
  }

  ggplotly(
    options %>%
      mutate(
        question_column = factor(
          .data$question_column,
          levels = names(responses) %||% responses,
          labels = responses
        )
      ) %>%
      group_by(
        !!sym(month_col),
        .data$question_column
      ) %>%
      count() %>%
      na.omit() %>%
      group_by(
        !!sym(month_col)
      ) %>%
      mutate(
        base_for_month = sum(.data$n),
        percentage = .data$n / sum(.data$n) * 100
      ) %>%
      select(
        !!month_col,
        .data$question_column,
        .data$percentage,
        .data$base_for_month
      ) %>%
      {
        if (!isFALSE(show_mean)) {
          left_join(
            .,
            means,
            by = month_col
          )
        } else {
          mutate(., mean = NA)
        }
      } %>%
      ggplot(
        aes(
          x = !!sym(month_col),
          y = .data$percentage,
          fill = .data$question_column,
          text = {
            if (!isFALSE(show_mean)) {
              glue(
                "{date_Ymd_to_bY(.data[[month_col]])} ",
                "({base_for_month}{ifelse(base_for_month < low_base, '*', '')})\n",
                "Mean score {mean}\n",
                "{question_column}\n",
                "{decimal_places(percentage, 0)}%"
              )
            } else {
              glue(
                "{date_Ymd_to_bY(.data[[month_col]])} ",
                "({base_for_month}{ifelse(base_for_month < low_base, '*', '')})\n",
                "{question_column}\n",
                "{decimal_places(percentage, 0)}%"
              )
            }
          }
        )
      ) +
      geom_col() +
      labs(
        y = "Percentage of respondents (%)",
        x = NULL,
        fill = legend_title
      ) +
      scale_x_date(
        date_breaks = "1 month",
        expand = c(0, 0),
        date_labels = "%B '%y"
      ) +
      scale_fill_manual(
        values = setNames(
          colours[seq_len(length(responses))],
          responses
        )
      ) +
      theme_classic() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1
        )
      ),
    tooltip = "text"
  ) %>%
    config(
      displayModeBar = FALSE
    ) %>%
    layout(
      legend = list(orientation = "h", x = 0.5, y = -0.6, xanchor = "center"),
      annotations = list(
        x = 0.5, y = -0.5, xanchor = "center",
        text = glue("* indicates low base size (< {low_base})"),
        showarrow = FALSE, xref = "paper", yref = "paper",
        font = list(size = 12)
      )
    )
}


#' Use for horizontal stacked barcharts showing percentages of some categorical
#' feature. These show a full range bar and a 3 month rolling bar for the last
#' month in the selected range.
#'
#' @param data Data
#' @param question_column Character
#' @param month_col Character
#' @param colours Vector of characters
#' @param last_valid_month_range_selection List of dates
#' @param responses Variable
#' @param keep_undisclosed Boolean
#' @param legend_names Variable
#' @param low_base Int
#'
#' @return Highcharts object
#' @export
#'
#' @examples \dontrun{
#'
#' }
stacked_horizontal <- function(data, question_column, month_col, colours,
                               last_valid_month_range_selection, # Exclude Linting
                               responses, keep_undisclosed = FALSE, legend_names,
                               low_base = 100) {
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  filter_out <- c("", "Undisclosed")
  if (keep_undisclosed) filter_out <- c("")

  question <- data %>%
    select(
      !!month_col,
      !!question_column
    ) %>%
    na.omit() %>%
    filter(!(!!sym(question_column) %in% filter_out))

  question <- question %>%
    group_by(
      !!sym(month_col)
    )

  chart <- question %>%
    select(month_col) %>%
    unique()

  for (name in names(responses)) {
    summ <- question %>%
      summarise(
        !!sym(name) := sum(.data[[question_column]] == responses[name])
      )

    chart <- chart %>%
      left_join(
        summ,
        by = "month_col"
      )
  }

  all_months <- chart %>%
    filter(
      between(
        !!sym(month_col),
        last_valid_month_range_selection[1],
        last_valid_month_range_selection[2]
      )
    ) %>%
    as.data.frame() %>%
    summarise(across(-month_col, sum)) %>%
    mutate(
      base = sum(across(!starts_with("month_col"))),
      !!sym(month_col) := glue("All in selected time frame<br>(base: ", .data$base, ")"),
      across(
        !starts_with(c(month_col, "base")),
        ~ 100 * . / .data$base,
        .names = "percent_{.col}"
      )
    )

  for (name in names(responses)) {
    all_months <- all_months %>%
      mutate(
        "{name}_significant" := "" # Exclude Linting
      )
  }

  chart <- chart %>%
    mutate(
      base = sum(across(!starts_with("month_col"))),
      across(
        !starts_with(c(month_col, "base")),
        ~ 100 * . / .data$base,
        .names = "percent_{.col}"
      )
    ) %>%
    ungroup()

  for (name in names(chart)[-c(0:length(responses) + 1)]) {
    chart <- chart %>%
      mutate(
        "prev_{name}" := lag(!!sym(name)) # Exclude Linting
      )
  }

  for (name in names(responses)) {
    chart <- chart %>%
      mutate(
        !!glue("{name}_significant") := per_s_error_vect( # Exclude Linting
          .data$prev_base,
          !!sym(glue("prev_percent_{name}")),
          .data$base,
          !!sym(glue("percent_{name}"))
        )
      )
  }

  rolling <- data %>%
    filter(
      !!sym(month_col) %in% sort(
        unique(data[[month_col]]),
        decreasing = TRUE
      )[1:3]
    ) %>%
    select(
      month_col,
      !!question_column
    ) %>%
    na.omit() %>%
    filter(
      !!sym(question_column) != ""
    ) %>%
    group_by(
      !!sym(question_column)
    ) %>%
    count() %>%
    ungroup()

  rolling_base <- sum(rolling$n)

  vals_list <- sapply(
    names(responses),
    function(x) {
      sum(
        rolling$n[rolling[[question_column]] == responses[x]]
      ) / rolling_base * 100
    }
  )

  rolling_percentages <- setNames(
    vals_list,
    names(responses)
  )

  rolling_base <- ifelse(rolling_base < low_base, glue("{rolling_base}*"), rolling_base)

  rolling_data <- data.frame(
    !!sym(month_col) := paste0(
      "3 Month Rolling to ",
      date_Ymd_to_bY(last_valid_month_range_selection[2]),
      " <br>(base: ",
      rolling_base, ")"
    )
  )

  rolling_data <- rolling_data %>%
    mutate(
      base = rolling_base
    )

  for (name in names(responses)) {
    rolling_data <- rolling_data %>%
      mutate(
        "percent_{name}" := rolling_percentages[[name]] # Exclude Linting
      )
  }

  chart <- all_months %>%
    bind_rows(rolling_data)

  hc <- highchart()

  i <- 1
  for (response in names(responses)) {
    hc <- hc %>%
      hc_add_series(
        name = legend_names[[responses[[response]]]],
        color = colours[i],
        data = chart,
        type = "bar",
        hcaes(
          x = !!sym(month_col),
          y = round(!!sym(glue("percent_{response}")))
        )
      )

    i <- i + 1
  }

  hc <- hc %>%
    hc_xAxis(
      type = "category"
    ) %>%
    hc_plotOptions(
      series = list(
        stacking = "normal",
        dataLabels = list(
          backgroundColor = "rgba(255,255,255,0.6)",
          borderRadius = 5,
          padding = 2,
          enabled = TRUE,
          inside = TRUE,
          style = list(
            textOutline = "none",
            color = "black",
            fontWeight = "bolder",
            fontSize = "15px"
          ),
          formatter = JS(
            "function() {
              var yval = this.point.y.toFixed(0);
              return yval;
            }"
          ),
          useHTML = TRUE
        )
      )
    ) %>%
    hc_yAxis(
      max = 100
    ) %>%
    hc_legend(
      title = list(
        text = glue("* indicates low base size (< {low_base})"),
        style = list(
          fontSize = 14,
          fontWeight = "normal"
        )
      ),
      reversed = TRUE,
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal"
    ) %>%
    hc_tooltip(
      shared = FALSE,
      valueDecimals = 0,
      backgroundColor = "#E8EDEE",
      useHTML = TRUE,
      pointFormat = "<b>Percentage: {point.y}%</b><br>"
    )
}


#' Pie chart showing percentage of unique responses. Use for non-rating style
#' questions with 3 or less possible responses only.
#'
#' @param data Data
#' @param question_column Character
#' @param colours Vector of characters
#' @param chart_title Character
#' @param low_base Int
#' @param responses Variable
#' @param keep_undisclosed Boolean
#'
#' @return Highcharts object
#' @export
#'
#' @examples \dontrun{
#'
#' }
pie <- function(data, question_column, colours = NULL, chart_title = NULL,
                low_base = 100, responses, keep_undisclosed = FALSE) {
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  filter_out <- c("", "Undisclosed")
  if (keep_undisclosed) filter_out <- c("")

  chart <- data %>%
    select(
      !!question_column
    ) %>%
    na.omit() %>%
    filter(
      !(!!sym(question_column) %in% filter_out)
    )

  if (is.null(colours)) {
    colours <- colour_ramp(length(unique(responses)))
  }

  chart <- chart %>%
    mutate(
      !!sym(question_column) := factor(
        !!sym(question_column),
        levels = names(responses) %||% responses,
        labels = responses
      )
    ) %>%
    group_by(
      !!sym(question_column),
      .drop = FALSE
    ) %>%
    summarise(
      n = n()
    ) %>%
    mutate(
      per = round(n / sum(n) * 100)
    ) %>%
    arrange(
      !!sym(question_column)
    ) %>%
    mutate(
      colour = colours[which(levels(!!sym(question_column)) == !!sym(question_column))]
    )

  base <- sum(chart$n)
  base <- ifelse(base < low_base, glue("{base}*"), base)

  highchart() %>%
    hc_add_series(
      name = "Response",
      data = chart,
      type = "pie",
      hcaes(
        x = !!sym(question_column),
        y = n,
        color = .data$colour
      )
    ) %>%
    {
      if (!is.null(chart_title)) {
        hc_title(
          .,
          text = chart_title
        )
      } else {
        .
      }
    } %>%
    hc_subtitle(
      text = paste0("Base: ", base)
    ) %>%
    hc_colors(
      unname(colours)
    ) %>%
    hc_tooltip(
      shared = TRUE,
      style = list(
        zIndex = 10000
      ),
      pointFormat = "<b>Percentage:</b> {point.per}%<br><b>Count: </b>{point.n}<br>",
      valueDecimals = 0,
      backgroundColor = "#E8EDEE",
      useHTML = FALSE
    ) %>%
    hc_legend(
      title = list(
        text = glue("* indicates low base size (< {low_base})"),
        style = list(
          fontSize = 14,
          fontWeight = "normal"
        )
      ),
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal"
    ) %>%
    hc_plotOptions(
      pie = list(
        dataLabels = list(
          enabled = FALSE
        ),
        showInLegend = TRUE
      )
    )
}


#' Use for horizontal barcharts showing percentages of some categorical
#' feature. Use for non-rating style questions with more than 3 responses only.
#'
#' @param data Data
#' @param columns Vector of characters
#' @param month_col Character
#' @param cols_start_with Boolean
#' @param arrange_desc Boolean
#' @param responses Variable
#' @param keep_undisclosed Boolean
#' @param colour Character
#' @param chart_title Character
#' @param low_base Int
#'
#' @return Highcharts object
#' @export
#'
#' @examples \dontrun{
#'
#' }
horizontal_bar <- function(data, columns, month_col, cols_start_with = FALSE,
                           arrange_desc = TRUE, responses, keep_undisclosed = FALSE,
                           colour = "#005EB8", chart_title = NULL, low_base = 100) {
  validate(need(nrow(data) > 0, "No data for current filter selection!"))

  column_prefix <- columns
  if (cols_start_with) columns <- names(data %>% select(starts_with(columns)))

  filter_out <- c("", "Undisclosed")
  if (keep_undisclosed) filter_out <- c("")

  options <- data %>%
    select(
      !!month_col,
      all_of(columns)
    ) %>%
    pivot_longer(
      -!!month_col,
      names_to = "question_column",
      values_drop_na = TRUE
    ) %>%
    filter(!.data$value %in% filter_out) %>%
    mutate(
      question_column = case_when(
        length(unique(.data$question_column)) == 1 ~ .data$value,
        TRUE ~ .data$question_column
      )
    )

  chart <- options %>%
    mutate(
      question_column = factor(
        .data$question_column,
        levels = names(responses) %||% responses,
        labels = responses
      )
    ) %>%
    group_by(
      .data$question_column
    ) %>%
    select(
      .data$question_column
    ) %>%
    count() %>%
    ungroup() %>%
    mutate(
      percentage = n / sum(n) * 100
    ) %>%
    select(
      .data$question_column,
      .data$percentage,
      .data$n
    ) %>%
    {
      if (arrange_desc) {
        arrange(
          .,
          desc(.data$percentage)
        )
      } else {
        arrange(
          .,
          .data$question_column
        )
      }
    }

  base <- sum(chart$n)
  base <- ifelse(base < low_base, glue("{base}*"), base)

  highchart() %>%
    hc_add_series(
      name = "Responses (%)",
      type = "bar",
      color = colour,
      data = chart,
      hcaes(
        x = .data$question_column,
        y = round(.data$percentage)
      ),
      showInLegend = TRUE
    ) %>%
    hc_yAxis(
      tickInterval = 5,
      max = max(chart$percentage),
      showLastLabel = FALSE
    ) %>%
    hc_xAxis(type = "category") %>%
    hc_legend(
      title = list(
        text = glue("* indicates low base size (< {low_base})"),
        style = list(
          fontSize = 14,
          fontWeight = "normal"
        )
      ),
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal"
    ) %>%
    {
      if (!is.null(chart_title)) {
        hc_title(
          .,
          text = chart_title
        )
      } else {
        .
      }
    } %>%
    hc_subtitle(text = paste0("Base: ", base)) %>%
    hc_tooltip(
      shared = TRUE,
      style = list(
        zIndex = 10000
      ),
      pointFormat = paste(
        "<b>Percentage of group:</b> {point.y}%<br><b>Count in group: </b>{point.n}<br>"
      ),
      valueDecimals = 0,
      backgroundColor = "#E8EDEE",
      useHTML = FALSE
    )
}


#' Use for displaying % of coded responses.
#'
#' @param data Data
#' @param coded_column Character
#' @param total_column Character
#' @param colour Character
#' @param chart_title Character
#' @param arrange_desc Boolean
#' @param low_base Int
#' @param top_ranks Int
#'
#' @return Highcharts object
#' @export
#'
#' @examples \dontrun{
#'
#' }
coding_horizontal_bar <- function(data, coded_column, total_column,
                                  colour = "#005EB8",
                                  chart_title = NULL, arrange_desc = TRUE,
                                  low_base = 100, top_ranks = 7) {
  chart <- data %>%
    na.omit() %>%
    select(
      !!coded_column,
      !!total_column
    ) %>%
    group_by(
      !!sym(coded_column)
    ) %>%
    mutate(
      !!sym(total_column) := sum(!!sym(total_column))
    ) %>%
    unique() %>%
    ungroup() %>%
    mutate(
      percentage = !!sym(total_column) / sum(!!sym(total_column)) * 100
    ) %>%
    select(
      !!coded_column,
      n = !!total_column,
      .data$percentage
    ) %>%
    {
      if (arrange_desc) {
        arrange(
          .,
          desc(.data$percentage)
        )
      } else {
        .
      }
    } %>%
    filter(
      .data$percentage > 1
    ) %>%
    head(top_ranks)

  base <- sum(chart$n)
  base <- ifelse(base < low_base, glue("{base}*"), base)

  highchart() %>%
    hc_add_series(
      name = "Coded Responses (%)",
      type = "bar",
      color = colour,
      data = chart,
      hcaes(
        x = !!coded_column,
        y = round(.data$percentage)
      )
    ) %>%
    hc_yAxis(
      tickInterval = 5,
      max = max(chart$percentage),
      showLastLabel = FALSE
    ) %>%
    hc_xAxis(type = "category") %>%
    hc_legend(
      title = list(
        text = glue("* indicates low base size (< {low_base})"),
        style = list(
          fontSize = 14,
          fontWeight = "normal"
        )
      ),
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal"
    ) %>%
    hc_yAxis(
      title = list(
        text = "% responses"
      )
    ) %>%
    {
      if (!is.null(chart_title)) {
        hc_title(
          .,
          text = chart_title
        )
      } else {
        .
      }
    } %>%
    hc_subtitle(
      text = paste0("Base: ", base)
    ) %>%
    hc_tooltip(
      shared = TRUE,
      style = list(
        zIndex = 10000
      ),
      pointFormat = "<b>Percentage:</b> {point.y}%<br><b>Count: </b>{point.n}<br>",
      valueDecimals = 0,
      backgroundColor = "#E8EDEE",
      useHTML = TRUE
    )
}
