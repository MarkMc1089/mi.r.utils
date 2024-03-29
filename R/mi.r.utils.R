#' \code{mi.r.utils} package
#'
#' Collection Of Functions, Particularly Ones Used In Shiny Apps
#'
#' @docType package
#' @name mi.r.utils
#'
#' @importFrom shiny tags validateCssUnit selectInput fluidRow column tagList
#' @importFrom shiny restoreInput need validate withTags div h2 h3 span br p
#' @importFrom shiny getDefaultReactiveDomain radioButtons uiOutput
#' @importFrom glue glue glue_collapse
#' @importFrom htmltools attachDependencies htmlDependency tagQuery HTML css
#' @importFrom htmltools doRenderTags
#' @importFrom zoo as.yearmon
#' @importFrom grDevices colorRampPalette
#' @importFrom shinyBS bsCollapse
#' @importFrom dplyr mutate filter left_join case_when group_by count ungroup distinct
#' @importFrom dplyr full_join arrange summarise all_of bind_rows n between desc
#' @importFrom dplyr select across if_else starts_with ends_with pull add_count lag
#' @importFrom dplyr rename_with transmute if_any rename
#' @importFrom highcharter highchart hc_add_series hc_xAxis hc_tooltip hc_plotOptions
#' @importFrom highcharter hc_yAxis hc_title hc_subtitle hc_legend hc_colors hcaes
#' @importFrom lubridate %m+% %m-% now years
#' @importFrom stats na.omit setNames
#' @importFrom rlang := sym as_name is_null .data squash
#' @importFrom tidyr replace_na complete nesting spread pivot_longer
#' @importFrom DT datatable
#' @importFrom plotly ggplotly config layout
#' @importFrom ggplot2 ggplot aes geom_col labs scale_x_date element_blank
#' @importFrom ggplot2 scale_fill_manual theme_classic theme element_text
#' @importFrom utils head
#' @importFrom htmlwidgets JS
#' @importFrom fs dir_create
#' @importFrom stringr str_replace str_replace_all str_split_fixed str_split str_count
#' @importFrom snakecase to_snake_case to_sentence_case
#' @importFrom withr with_locale
#' @importFrom skimr skim
#' @importFrom rstudioapi navigateToFile
#' @importFrom purrr walk2 map map2 imap set_names reduce keep
#' @importFrom shinyWidgets prettyCheckboxGroup
#' @importFrom tibble deframe
NULL

## quiets concerns of R CMD check re. the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
