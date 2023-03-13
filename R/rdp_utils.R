#' Title
#'
#' @param files Vector of filepaths
#' @param backup_dirs Vector of filepaths
#' @param overwrite Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
check_overwrite <- function(files, backup_dirs, overwrite) {
  .check_overwrite <- function(.file, .backup_dir, overwrite) {
    if (file.exists(.file)) {
      if (overwrite) {
        file_copy <- file.copy(.file, .backup_dir, overwrite)

        warning(
          glue(
            "Overwriting existing file: {.file}. Previous file saved to backup \\
            folder {.backup_dir}."
          ),
          call. = FALSE
        )
      } else {
        stop(glue("File {.file} already exists...aborting"))
      }
    }
  }

  mapply(.check_overwrite, files, backup_dirs, MoreArgs = list(overwrite = overwrite))
}


#' Title
#'
#' @param app_type Character
#' @param data_prefix Character
#' @param month_col Character
#' @param region_col Vector of filepaths
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
add_reactive_data <- function(app_type, data_prefix, month_col, region_col) {
  app_r <- "app.R"
  app_lines <- readLines(app_r)

  reactive_data_temp <- readLines(
    system.file(
      "projects", app_type, "templates", "reactive_data_template.txt",
      package = "projecthooks"
    )
  )
  reactive_data_temp <- c(reactive_data_temp, "", "")
  reactive_data_temp <- glue(
    glue_collapse(reactive_data_temp, sep = "\n"),
    .trim = FALSE
  )

  target <- glue("  filtered_{data_prefix}_data <- reactiveValues(")
  if (is.na(match(target, app_lines))) {
    file_insert_lines(app_r, reactive_data_temp, "  ### Reactive data above")
  }
}


#' Title
#'
#' @param app_type Character
#' @param app_title Character
#' @param data_prefix Character
#' @param month_col Character
#' @param region_col Character
#' @param backup_dir Filepath
#' @param overwrite Boolean
#' @param open Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
create_app_r <- function(app_type, app_title, data_prefix, month_col, region_col,
                         backup_dir = NULL, overwrite = FALSE, open = FALSE) {
  app_r <- "app.R"
  check_overwrite(app_r, backup_dir, overwrite)

  app_r_temp <- readLines(
    system.file(
      "projects", app_type, "templates", "app_template.txt",
      package = "projecthooks"
    )
  )
  app_r_temp <- glue(glue_collapse(app_r_temp, sep = "\n"), .trim = FALSE)

  writeLines(app_r_temp, app_r)

  add_reactive_data(app_type, data_prefix, month_col, region_col)

  if (open) navigateToFile(app_r)
}


#' Title
#'
#' @param app_type Character
#' @param data_prefix Character
#' @param month_col Character
#' @param region_col Character
#' @param main_title Character
#' @param subtitle Character
#' @param backup_dir Filepath
#' @param overwrite Boolean
#' @param open Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
create_ui_r <- function(app_type, data_prefix, month_col, region_col,
                        main_title, subtitle, backup_dir = NULL,
                        overwrite = FALSE, open = FALSE) {
  ui_dir <- dir_create("ui")
  ui_r <- "ui/ui.R"

  check_overwrite(ui_r, backup_dir, overwrite)

  ui_r_temp <- readLines(
    system.file(
      "projects", app_type, "templates", "ui", "ui_template.txt",
      package = "projecthooks"
    )
  )
  ui_r_temp <- glue(glue_collapse(ui_r_temp, sep = "\n"), .trim = FALSE)

  writeLines(ui_r_temp, ui_r)

  if (open) navigateToFile(ui_r)
}


#' Title
#'
#' @param lines Vector of characters
#' @param to_insert Vector of characters
#' @param insert_at Int
#'
#' @return Vector of characters
#' @export
#'
#' @examples \dontrun{
#'
#' }
insert_lines_at <- function(lines, to_insert, insert_at) {
  # edge cases: inserting at the end or the beginning (or past the end)
  if (is.na(insert_at) || insert_at >= length(lines)) {
    return(c(lines, to_insert))
  } else if (insert_at == 0) {
    return(c(to_insert, lines))
  }

  pre <- lines[1:insert_at]
  post <- lines[(insert_at + 1):length(lines)]
  return(c(pre, to_insert, post))
}


#' Title
#'
#' @param .file Filepath
#' @param .lines Vector of characters
#' @param .match Character or NA
#' @param .nth_match Int
#' @param open Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
file_insert_lines <- function(.file, .lines, .match = NA,
                              .nth_match = 1, open = FALSE) {
  lines <- readLines(.file)
  insert_at <- which(lines == .match)[.nth_match] - 1
  lines <- insert_lines_at(lines, .lines, insert_at)

  writeLines(lines, .file)

  if (open) navigateToFile(.file, insert_at + 1)
}


#' Title
#'
#' @param lines Vector of characters
#' @param to_remove Character
#'
#' @return Vector of characters or NULL
#' @export
#'
#' @examples \dontrun{
#'
#' }
remove_line <- function(lines, to_remove) {
  remove_at <- match(to_remove, lines)
  if (!is.na(remove_at)) {
    pre <- lines[1:remove_at - 1]
    post <- if (remove_at < length(lines)) lines[(remove_at + 1):length(lines)] else c()

    return(c(pre, post))
  }

  return(invisible(NULL))
}


#' Title
#'
#' @param .file Filepath
#' @param .match Character or NA
#' @param open Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
file_remove_line <- function(.file, .match = NA, open = FALSE) {
  lines <- readLines(.file)
  lines <- remove_line(lines, .match)
  if (!is.null(lines)) writeLines(lines, .file)

  if (open) navigateToFile(.file)
}


#' Title
#'
#' @param app_type Character
#' @param page_name Character
#' @param page_menu_name Character
#' @param backup_dirs Vector of filepaths
#' @param overwrite Boolean
#' @param open Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
add_page <- function(app_type, page_name, page_menu_name = page_name,
                     backup_dirs = NULL, overwrite = FALSE, open = FALSE) {
  page_file_name <- to_snake_case(page_name)
  page_dir <- dir_create(file.path("ui", "pages"))
  page_r <- glue("{page_dir}/{page_file_name}.R")
  server_dir <- dir_create("server")
  server_r <- glue("{server_dir}/{page_file_name}_outputs.R")

  check_overwrite(
    c(page_r, server_r), backup_dirs, overwrite
  )

  app_r <- "app.R"
  ui_r <- "ui/ui.R"

  app_lines <- readLines(app_r)
  ui_lines <- readLines(ui_r)

  not_first_page <- startsWith(
    app_lines[[match("  ### Page outputs above", app_lines) - 1]],
    "  source"
  )

  visibility <- "visible"
  if (not_first_page) visibility <- "hidden"

  page_r_temp <- readLines(
    system.file(
      "projects", app_type, "templates", "ui", "ui_page_template.txt",
      package = "projecthooks"
    )
  )
  page_r_temp <- glue(glue_collapse(page_r_temp, sep = "\n"))

  file.create(page_r)
  writeLines(page_r_temp, page_r)
  if (open) navigateToFile(page_r)

  file.create(server_r)
  if (open) navigateToFile(server_r)

  target <- glue('  source("./server/{page_file_name}_outputs.R", local = TRUE)')
  if (is.na(match(target, app_lines))) {
    file_insert_lines(app_r, target, "  ### Page outputs above", open = open)
  }

  target <- glue('source("./ui/pages/{page_file_name}.R", local = TRUE)')
  if (is.na(match(target, ui_lines))) {
    file_insert_lines(ui_r, target, "### Source pages above", open = open)
  }

  target <- glue("  {page_file_name}_page,")
  if (is.na(match(target, ui_lines))) {
    file_insert_lines(ui_r, target, "  ### Add pages above", open = open)
  }

  target <- glue(
    "        a(class = 'navigation homeitem item', '{page_menu_name}', ",
    "`data-value` = '{page_file_name}'),"
  )
  if (is.na(match(target, ui_lines))) {
    file_insert_lines(ui_r, target, "        ### Add page links above", open = open)
  }
}


#' Title
#' @param app_type Character
#' @param pages Vector of characters
#' @param page_menu_names Vector of characters
#' @param backup_dirs Vector of filepaths
#' @param overwrite Boolean
#' @param open Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
add_pages <- function(app_type, pages, page_menu_names = pages,
                      backup_dirs = NULL, overwrite = FALSE, open = FALSE) {
  walk2(
    pages,
    page_menu_names,
    add_page,
    app_type = app_type,
    backup_dirs = backup_dirs, overwrite = overwrite, open = open
  )
}


#' Title
#'
#' @param page_name Character
#' @param page_menu_name Character
#' @param backup_dirs Vector of filepaths
#' @param open Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
remove_page <- function(page_name, page_menu_name = page_name,
                        backup_dirs = NULL, open = FALSE) {
  page_file_name <- to_snake_case(page_name)
  page_r <- glue("ui/pages/{page_file_name}.R")
  server_r <- glue("server/{page_file_name}_outputs.R")

  ui_exists <- file.exists(page_r)
  server_exists <- file.exists(server_r)

  if (is.null(backup_dirs)) {
    backup_dir <- dir_create(
      file.path("backup", str_replace_all(str_replace(now(), " ", "_"), ":", "."))
    )
    ui_backup_dir <- dir_create(file.path(backup_dir, "ui"))
    pages_backup_dir <- dir_create(file.path(ui_backup_dir, "pages"))
    server_backup_dir <- dir_create(file.path(backup_dir, "server"))

    backup_dirs <- c(pages_backup_dir, server_backup_dir)
  }

  if (ui_exists) {
    file.copy(page_r, backup_dirs[1], overwrite = TRUE)
  }
  if (server_exists) {
    file.copy(server_r, backup_dirs[2], overwrite = TRUE)
  }

  unlink(page_r)
  unlink(server_r)

  if (any(c(ui_exists, server_exists))) {
    both <- all(c(ui_exists, server_exists))
    maybe <- if (both) {
      list(" & ", "s", "They have", "respectively")
    } else {
      list("", "", "It has", "")
    }
    names(maybe) <- c("and", "s", "they_have", "respectively")
    files <- glue(
      "{if (ui_exists) page_r else ''}{maybe$and}{if (server_exists)
      server_r else ''}"
    )
    backups <- glue(
      "{if (ui_exists) pages_backup_dir else ''}{maybe$and}{if (server_exists)
      server_backup_dir else ''}{maybe$respectively}"
    )
    message(
      glue(
        "File{maybe$s} deleted: {files}.
        {maybe$they_have} been saved to the backup folder{maybe$s} {backups}."
      )
    )
  }

  app_r <- "app.R"
  ui_r <- "ui/ui.R"

  target <- glue('  source("./server/{page_file_name}_outputs.R", local = TRUE)')
  file_remove_line(app_r, target, open = open)

  target <- glue('source("./ui/pages/{page_file_name}.R", local = TRUE)')
  file_remove_line(ui_r, target, open = open)

  target <- glue("  {page_file_name}_page,")
  file_remove_line(ui_r, target, open = open)

  target <- glue(
    "      a(class = 'navigation homeitem item', '{page_menu_name}', ",
    "`data-value` = '{page_file_name}'),"
  )
  file_remove_line(ui_r, target, open = open)
}


#' Title
#'
#' @param pages Vector of characters
#' @param page_menu_names Vector of characters
#' @param backup_dirs Vector of filepaths
#' @param open Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
remove_pages <- function(pages, page_menu_names = pages,
                         backup_dirs = NULL, open = FALSE) {
  walk2(
    pages,
    page_menu_names,
    remove_page,
    backup_dirs = backup_dirs,
    open = open
  )
}


#' Title
#'
#' @param app_type Character
#' @param output_name Character
#' @param render_func Function
#' @param output_func Function
#' @param args Character
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
create_output <- function(app_type, output_name, render_func, output_func, args) {
  if (tolower(output_func) == "custom") {
    output_func <- "{CUSTOM_FUNC}"
    args <- ""
  } else {
    args_names <- names(formals(output_func))
    args_split <- str_split_fixed(args, ", ", n = length(args_names) + 1)
    length(args_split) <- length(args_names)
    args_to_check <- paste(args_names, "=", args_split)
    args_to_combine <- args_to_check[!endsWith(args_to_check, "NA")]
    args <- paste(args_to_combine, collapse = ", ")
  }

  output_temp <- readLines(
    system.file(
      "projects", app_type, "templates", "server", "output_template.txt",
      package = "projecthooks"
    )
  )
  if (output_func == "{CUSTOM_FUNC}") {
    comments <- rep("# ", length(output_temp))
    output_temp <- paste0(comments, output_temp)
  }

  glue(glue_collapse(output_temp, sep = "\n"), "\n\n")
}


#' Title
#'
#' @param app_type Character
#' @param page Filepath
#' @param output_name Character
#' @param render_func Function
#' @param output_func Function
#' @param args Vector of characters
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
add_output <- function(app_type, page, output_name, render_func, output_func, args) {
  output <- create_output(app_type, output_name, render_func, output_func, args)

  file_insert_lines(file.path("server/", page), output, .match = NA)
}

# TODO: refactor and rename like create/add_output
#' Title
#'
#' @param app_type Character
#' @param page Character
#' @param label Character
#' @param output_name Character
#' @param ui_func Function
#' @param section_num Int
#' @param use_comment_group Boolean
#' @param use_aspect_radio Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
add_ui_element <- function(app_type, page, label, output_name, ui_func, section_num,
                           use_comment_group, use_aspect_radio) {
  if (use_comment_group) {
    element_temp <- readLines(
      system.file(
        "projects", app_type, "templates", "ui", "ui_comment_group_template.txt",
        package = "projecthooks"
      )
    )
    group_select_id <- glue("{output_name}_group")
  } else if (use_aspect_radio) {
    element_temp <- readLines(
      system.file(
        "projects", app_type, "templates", "ui",
        "ui_stacked_vertical_radio_template.txt",
        package = "projecthooks"
      )
    )
    radio_select_id <- glue("{output_name}_radio")
    radio_select_choices <- glue("{output_name}_choices")
  } else {
    element_temp <- readLines(
      system.file(
        "projects", app_type, "templates", "ui", "ui_element_template.txt",
        package = "projecthooks"
      )
    )
  }

  if (ui_func == "CUSTOM_OUTPUT") {
    len_et <- length(element_temp)
    comments <- rep("# ", len_et - 1)
    element_temp <- c(
      paste0(comments, element_temp[1:(len_et - 1)]),
      element_temp[len_et]
    )
  }

  new_element <- glue(glue_collapse(element_temp, sep = "\n"), .trim = FALSE)
  target <- "  ),"

  file_insert_lines(page, new_element, target, section_num)
}

# TODO: refactor and rename like create/add_output
#' Title
#'
#' @param app_type Character
#' @param page Character
#' @param section Character
#' @param label Character
#' @param output_name Character
#' @param ui_func Function
#' @param section_num Int
#' @param use_comment_group Boolean
#' @param use_aspect_radio Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
add_ui <- function(app_type, page, section, label, output_name, ui_func, section_num,
                   use_comment_group, use_aspect_radio) {
  # section may exist already
  page_file_name <- to_snake_case(page)
  page_r <- glue("ui/pages/{page_file_name}.R")
  lines <- readLines(glue("ui/pages/{page_file_name}.R"))
  target <- glue("    \"{section}\",")

  if (is.na(match(target, lines))) {
    # section does not exist, so add
    section_temp <- readLines(
      system.file(
        "projects", app_type, "templates", "ui", "ui_section_template.txt",
        package = "projecthooks"
      )
    )
    new_section <- glue(glue_collapse(section_temp, sep = "\n"), .trim = FALSE)
    target <- glue("  htmlOutput(\"{page_file_name}trigger\")")

    file_insert_lines(page_r, new_section, target)
    lines <- readLines(glue("ui/pages/{page_file_name}.R"))
  }

  add_ui_element(
    app_type, page_r, label, output_name, ui_func,
    section_num, use_comment_group, use_aspect_radio
  )
}


#' Title
#'
#' @param .data Data
#' @param .property Character
#'
#' @return Vector of type corresponding to the data
#' @export
#'
#' @examples \dontrun{
#'
#' }
pull_value <- function(.data, .property) {
  .data %>%
    filter(.data$property == .property) %>%
    pull(.data$value)
}


# The sparkline histograms don't work in English locale, so temporarily switch to
# Chinese locale. Normally, the sleep would not be needed, but I find a small pause
# is required or by the time the summary prints, the locale is set back!
# Also, when factor columns have missing values a warning is generated, which we don't
# need to know.
#' Title
#'
#' @param .data Data
#' @param .data_meta Data
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
summarise_data <- function(.data, .data_meta) {
  with_locale(c("LC_CTYPE" = "Chinese"), {
    suppress_warnings(
      {
        print(skim(.data))
      },
      endsWith,
      "value(s) of \"\" that have been converted to \"empty\"."
    )
    Sys.sleep(0.01)
  })
}


#' Title
#'
#' @param data Data
#' @param levels_meta Data
#'
#' @return Vector of characters
#' @export
#'
#' @examples \dontrun{
#'
#' }
create_levels <- function(data, levels_meta) {
  level_names <- levels_meta %>%
    select(.data$output_name) %>%
    group_by(.data$output_name) %>%
    add_count()

  multi_col_level_names <- level_names %>%
    filter(n > 1) %>%
    pull(.data$output_name) %>%
    unique()

  single_col_level_names <- setdiff(level_names$output_name, multi_col_level_names)

  levels <- lapply(data, unique) %>%
    lapply(setdiff, c("", "Undisclosed")) %>%
    lapply(sort)

  check_undisclosed <- levels_meta %>%
    transmute(
      .data$output_name,
      keep_undisclosed = if_else(!is.na(.data$arg7) & .data$arg7, TRUE, FALSE)
    ) %>%
    distinct() %>%
    deframe()

  levels <- map(levels, ~ paste0("  \"", .x, "\"", " = \"", .x, "\""))
  levels <- imap(
    levels,
    ~ `if`(
      length(.x) == 1,
      str_replace(.x, "(?<=([\"']))(?:(?=(\\\\?))\\2.)*?(?=\\1)", .y),
      .x
    )
  )
  levels <- imap(
    levels,
    ~ `if`(
      all(endsWith(.x, "\"\"")),
      {
        warning(
          glue("Option text inferred in levels for {.y}. Please check before using."),
          call. = FALSE
        )
        str_replace(
          .x,
          "(?<=([\"']))(?=\\1)",
          to_sentence_case(str_split(.y, "option_")[[1]][2])
        )
      },
      .x
    )
  )

  multi_col_levels <- map(
    multi_col_level_names,
    function(prefix) {
      levels %>%
        keep(startsWith(names(.), prefix)) %>%
        set_names(str_replace(names(.), names(.), prefix))
    }
  ) %>%
    squash()

  multi_col_levels <- map(
    set_names(multi_col_level_names),
    ~ unique(unlist(multi_col_levels[names(multi_col_levels) == .], use.names = FALSE))
  )

  levels <- c(levels[single_col_level_names], multi_col_levels)

  levels <- imap(
    levels,
    ~ `if`(
      check_undisclosed[.y],
      {
        c(.x, "  \"Undisclosed\" = \"Undisclosed\"")
      },
      .x
    )
  )

  levels <- map(levels, ~ paste0(.x, collapse = ",\n"))
  levels <- imap(levels, ~ paste0(.y, "_responses <- c(\n", .x, "\n)"))

  paste_lvls <- function(out, input) paste(out, input, sep = "\n\n")

  levels %>% reduce(paste_lvls)
}


#' Title
#'
#' @param app_type Character
#' @param data_prefix Character
#' @param data Data
#' @param levels_meta Data
#' @param backup_dir Filepath
#' @param overwrite Boolean
#' @param append Boolean
#' @param open Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
add_levels <- function(app_type, data_prefix, data, levels_meta, backup_dir = NULL,
                       overwrite = FALSE, append = FALSE, open = FALSE) {
  levels <- create_levels(data, levels_meta)

  global_dir <- dir_create("global")
  levels_r <- "global/levels.R"

  levels_r_temp <- readLines(
    system.file(
      "projects", app_type, "templates", "global", "levels_template.txt",
      package = "projecthooks"
    )
  )

  levels_r_temp <- glue(glue_collapse(levels_r_temp, sep = "\n"))

  if (!append) check_overwrite(levels_r, backup_dir, overwrite)

  if (append) {
    levels_r_temp <- c(
      readLines(levels_r),
      "",
      levels_r_temp
    )

    warning("Appending to existing file: levels.R", call. = FALSE)
  } else {
    standard_levels_r_temp <- readLines(
      system.file(
        "projects", app_type, "templates", "global", "standard_levels.txt",
        package = "projecthooks"
      )
    )

    levels_r_temp <- c(
      standard_levels_r_temp,
      "",
      levels_r_temp
    )
  }

  file.create(levels_r)
  writeLines(levels_r_temp, levels_r)
  if (open) navigateToFile(levels_r)
}


#' Title
#'
#' @param choices_meta Data
#'
#' @return Vector of characters
#' @export
#'
#' @examples \dontrun{
#'
#' }
create_choices <- function(choices_meta) {
  choices <- setNames(choices_meta$rename_to, choices_meta$output_name)
  choices <- map(choices, ~ paste0("  \"", .x, "\"", " = \"", .x, "\""))
  choices <- map(
    set_names(unique(names(choices))),
    ~ unique(unlist(choices[names(choices) == .], use.names = FALSE))
  )
  choices <- map(choices, ~ paste0(.x, collapse = ",\n"))
  choices <- imap(choices, ~ paste0(.y, "_choices <- c(\n", .x, "\n)"))

  paste_lvls <- function(out, input) paste(out, input, sep = "\n\n")

  choices %>% reduce(paste_lvls)
}


#' Title
#'
#' @param choices_meta Data
#' @param backup_dir Filepath
#' @param overwrite Boolean
#' @param append Boolean
#' @param open Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
add_choices <- function(choices_meta, backup_dir = NULL,
                        overwrite = FALSE, append = FALSE, open = FALSE) {
  choices <- create_choices(choices_meta)

  global_dir <- dir_create("global")
  choices_r <- "global/choices.R"

  if (!append) check_overwrite(choices_r, backup_dir, overwrite)

  if (append) {
    choices_r_temp <- c(
      readLines(choices_r),
      "",
      choices
    )

    warning("Appending to existing file: choices.R", call. = FALSE)
  } else {
    choices_r_temp <- choices
  }

  warning(
    glue("Choices created will have human unfriendly names. Please edit before using."),
    call. = FALSE
  )

  file.create(choices_r)
  writeLines(choices_r_temp, choices_r)
  if (open) navigateToFile(choices_r)
}


#' Title
#'
#' @param app_type Character
#' @param data_prefix Character
#' @param page_1 Character
#' @param survey_doc Character
#' @param backup_dir Filepath
#' @param overwrite Boolean
#' @param open Boolean
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
create_ui_outputs_r <- function(app_type, data_prefix, page_1, survey_doc,
                                backup_dir = NULL, overwrite = FALSE, open = FALSE) {
  server_dir <- dir_create("server")
  ui_outputs_r <- "server/ui_outputs.R"

  check_overwrite(ui_outputs_r, backup_dir, overwrite)

  ui_outputs_r_temp <- readLines(
    system.file(
      "projects", app_type, "templates", "server", "ui_outputs.txt",
      package = "projecthooks"
    )
  )
  ui_outputs_r_temp <- glue(glue_collapse(ui_outputs_r_temp, sep = "\n"), .trim = FALSE)

  writeLines(ui_outputs_r_temp, ui_outputs_r)

  if (nchar(survey_doc) > 0) add_survey_doc_output(app_type, data_prefix, survey_doc)

  if (open) navigateToFile(ui_outputs_r)
}

#' Title
#'
#' @param app_type Character
#' @param data_prefix Character
#' @param survey_doc Character
#'
#' @return Used for side effects
#' @export
#'
#' @examples \dontrun{
#'
#' }
add_survey_doc_output <- function(app_type, data_prefix, survey_doc) {
  survey_doc_r_temp <- readLines(
    system.file(
      "projects", app_type, "templates", "server", "survey_download_output_template.txt",
      package = "projecthooks"
    )
  )
  survey_doc_r_temp <- c(survey_doc_r_temp, "")
  survey_doc_r_temp <- glue(
    glue_collapse(survey_doc_r_temp, sep = "\n"),
    .trim = FALSE
  )

  ui_outputs_r <- "server/ui_outputs.R"
  ui_outputs_r_lines <- readLines(ui_outputs_r)

  target <- glue("output${data_prefix}_survey_download <- downloadHandler(")
  if (is.na(match(target, ui_outputs_r_lines))) {
    file_insert_lines(
      ui_outputs_r, survey_doc_r_temp, "### Survey download output above"
    )
  }
}
