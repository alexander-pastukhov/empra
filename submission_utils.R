library(tidyverse)
library(slider)
library(yaml)

#' Parse a YAML entry into a tibble
#'
#' Parses a YAML entry into a single row tibble. Assumes that the YAML entry has fields "title", "authors", "supervisor", "abstract", "content", "type".
#'
#' @param yaml_entry A list with fields "title", "authors", "supervisor", "abstract", "content" (optional), "type".
#' @param empra_year Year of the EMPRA
#'
#' @returns A tibble with columns "Title", "Authors", "FirstAuthor", "Supervisor", "Abstract", "Content", "Type", "Year".
entry_to_tibble <- function(yaml_entry, empra_year) {
  # format authors
  authors <- ""
  for(iauthor in 1:length(yaml_entry$authors)) {
    if (iauthor > 1) {
      if (iauthor == length(yaml_entry$authors)) {
        authors <- str_c(authors, " & ")
      } else {
        authors <- str_c(authors, ", ")
      }
    }
    authors <- str_c(authors, yaml_entry$authors[[iauthor]]$name, " ", yaml_entry$authors[[iauthor]]$lastname)
  }

  tibble(Title = yaml_entry$title,
         Authors = authors,
         FirstAuthor = yaml_entry$authors[[1]]$lastname,
         Supervisor = yaml_entry$supervisor,
         Abstract = yaml_entry$abstract,
         Content = ifelse(is.null(yaml_entry$content), NA, yaml_entry$content),
         Type = yaml_entry$type,
         Year = empra_year)
}


#' Load EMPRA submissions for a given year
#'
#' @param empra_year Year of the EMPRA
#'
#' @returns A list of lists
load_years_submissions <- function(empra_year) {
  yaml::read_yaml(sprintf("%d/submissions.yaml", empra_year)) |>
  purrr::map(~entry_to_tibble(., empra_year)) |>
  list_rbind() |>
  group_by(Type) |>
  arrange(FirstAuthor)
}


#' Format entry in HTML
#'
#' Format entry as HTML for the EMPRA website. The entry is formatted as a grid with two columns:
#' the first column contains the title, authors, supervisor, and abstract; the second column contains a link to the content (if available).
#'
#' @param entry A tibble with columns "Title", "Authors", "Supervisor", "Abstract", "Content", "Type", "Year".
#' @param page_language Language of the page, used to format the HTML code.
#'
#' @returns A character vector with the HTML code for the entry.
submission_html <- function(entry, page_language) {

  translations <- list("Abstract" = c("en" = "Abstract", "de" = "Abstrakt"))

  cat('::: {.grid}\n')
    cat('::: {.g-col-10}\n')
      cat("**", entry$Title, "**<br/>", entry$Authors, " (Supervisor: ", entry$Supervisor, ")<br/>", sep = "")
      cat("<details><summary>", translations[['Abstract']][page_language], "</summary>", entry$Abstract, "</details>\n")
    cat(':::\n')

    cat('::: {.g-col-2}\n')
      if (!is.na(entry$Content)) {
        cat('<a href="', entry$Year, "/", entry$Content, '" target="_blank">',
            sprintf('<img src="assets/icons/%s.svg" alt="Poster" style="width:30px;"/>', entry$Type),
            '</a>\n', sep="")
      }
    cat(':::\n')
  cat(':::\n')
}

#' List all submission of a given type
#'
#' @param submissions A tibble with columns "Title", "Authors", "Supervisor", "Abstract", "Content", "Type", "Year".
#' @param submission_type String, either "poster" or "talk"
#' @param page_language Language of the page, used to format the HTML code.
list_submissions <- function(submissions, submission_type, page_language) {
  submissions |>
    filter(Type == submission_type) |>
    slider::slide(~submission_html(., page_language)) ->
    hide_return_values
}
