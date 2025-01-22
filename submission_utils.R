library(tidyverse)

library(glue)
library(slider)
library(yaml)

translations <- list("Abstract" = c("en" = "Abstract", "de" = "Abstrakt"))


#' Format authors list
#'
#' @param authors_list A list of authors, each author is a list with fields "name", "lastname", and "empra_student" (optional).
#'
#' @returns A character vector with the formatted authors list.
format_authors_list <- function(authors_list) {
  # format authors
  authors <- ""
  for(iauthor in 1:length(authors_list)) {
    if (iauthor > 1) {
      if (iauthor == length(authors_list)) {
        authors <- str_c(authors, " & ")
      } else {
        authors <- str_c(authors, ", ")
      }
    }
    if ("empra_student" %in% names(authors_list[[iauthor]])) {
      authors <- str_c(authors, "**", authors_list[[iauthor]]$name, " ", authors_list[[iauthor]]$lastname, "**", sep = "")
    } else {
      authors <- str_c(authors, authors_list[[iauthor]]$name, " ", authors_list[[iauthor]]$lastname, sep = "")
    }
  }
  authors
}


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
        if (length(yaml_entry$authors) == 2) {
          authors <- str_c(authors, " & ")
        } else {
          authors <- str_c(authors, ", & ")
        }
      } else {
        authors <- str_c(authors, ", ")
      }
    }
    current_author <- unlist(str_split(yaml_entry$authors[[iauthor]], ", "))
    authors <- str_c(authors, current_author[2], " ", current_author[1])
  }

  supervisor <- unlist(str_split(yaml_entry$supervisor, ", "))


  tibble(Title = yaml_entry$title,
         Authors = authors,
         FirstAuthor = yaml_entry$authors[[1]],
         SupervisorLastname = yaml_entry$supervisor,
         Supervisor = glue("{supervisor[2]} {supervisor[1]}"),
         Abstract = yaml_entry$abstract,
         Content = ifelse(is.null(yaml_entry$content), NA, yaml_entry$content),
         Number = ifelse(is.null(yaml_entry$number), NA, yaml_entry$number),
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
  list_rbind()
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
  cat('::: {.grid}\n')
  cat('::: {.g-col-1}\n')
  cat(entry$Number, '\n')
  cat(':::\n')
  cat('::: {.g-col-9}\n')
      cat("**", entry$Title, "**<br/>", entry$Authors, "<br/>_Supervisor: ", entry$Supervisor, "_<br/>", sep = "")
      if (!is.na(entry$Abstract)) cat("<details><summary>", translations[['Abstract']][page_language], "</summary>", entry$Abstract, "</details>")
    cat('\n:::\n')

    cat('::: {.g-col-2}\n')
      if (!is.na(entry$Content)) {
        cat('<a href="', entry$Year, "/", entry$Content, '" target="_blank">',
            sprintf('<img src="assets/icons/%s.svg" alt="Poster" style="width:30px;"/>', entry$Type),
            '</a>\n', sep="")
      }
    cat(':::\n')
  cat(':::\n')
}


#' Make a LaTeX-safe version of a text
#'
#' @param text A character vector with the text to escape.
#'
#' @returns A character vector with the escaped text.
escape_latex <- function(text) {
  text <- gsub("&", "\\\\&", text)           # Escape '&'
  text <- gsub("%", "\\\\%", text)           # Escape '%'
  text <- gsub("\\$", "\\\\$", text)         # Escape '$'
  text <- gsub("#", "\\\\#", text)           # Escape '#'
  text <- gsub("_", "\\\\_", text)           # Escape '_'
  text <- gsub("\\{", "\\\\{", text)         # Escape '{'
  text <- gsub("\\}", "\\\\}", text)         # Escape '}'
  text <- gsub("~", "\\\\textasciitilde{}", text)  # Escape '~'
  text <- gsub("\\^", "\\\\textasciicircum{}", text)  # Escape '^'
  text
}

#' Title, authors, and supervisor in LaTeX
#'
#' @param entry A tibble with columns "Title", "Authors", "Supervisor", "Abstract", "Content", "Type", "Year".
#' @param page_language Language of the page, used to format the HTML code.
submission_pdf <- function(entry, page_language) {
  cat("\\noindent\n")  # Prevent indentation at the start
  cat("\\begin{minipage}{\\textwidth}\n")
  cat("\\begin{center}\n")
  cat("\\textbf{", escape_latex(entry$Title), "}\\\\\n", escape_latex(entry$Authors), "\\\\\n", sep="")
  cat("Supervisor: ", entry$Supervisor, "\\\\\n", sep="")
  cat("\\end{center}\n")
  cat("\\end{minipage}\n")  # End grouping for header lines
  if (!is.na(entry$Abstract)) {
    cat("\\noindent\n")  # Prevent indentation for the abstract
    cat(entry$Abstract, "\n\n")
  }
  cat("\\vspace{1cm}\n")
}

#' List all submission of a given type
#'
#' @param submissions A tibble with columns "Title", "Authors", "Supervisor", "Abstract", "Content", "Type", "Year".
#' @param submission_type String, either "poster" or "talk"
#' @param page_language Language of the page, used to format the HTML code.
list_submissions <- function(submissions, submission_type, page_language, format_function=submission_html) {
  subset <-
    submissions |>
    filter(Type == submission_type)

  # adding numbers, if they are not present
  if (sum(is.na(subset$Number)) == nrow(subset)) {
    subset <-
      subset |>
      arrange(SupervisorLastname, FirstAuthor) |>
      mutate(Number = glue("{str_to_upper(str_sub(submission_type, 1, 1))}{row_number()}"))
  }

  subset |>
    mutate(Index = as.integer(str_extract(Number, "\\d+"))) |>
    arrange(Index) |>
    slider::slide(~format_function(., page_language)) ->
    hide_return_values
}


#' Format a single publication
#'
#' @param entry A list with fields "title", "authors", "journal", "volume", "issue", "pages", "doi", "year".
#'
#' @returns tibble with columns "Year", "FirstAuthor", "Citation", "Abstract".
process_empra_publication <- function(entry) {
  tibble(Year = entry$year,
         FirstAuthor = entry$authors[[1]]$lastname,
         Citation = glue('{format_authors_list(entry$authors)} ({entry$year}) "{entry$title}"_{entry$journal}_, {entry$volume}({entry$issue}), {entry$pages}. doi: [{entry$doi}](https://doi.org/{entry$doi})'),
         Abstract = entry$abstract)
}


#' Format and print out all publications for a year
#'
#' @param year Year of the publications
#' @param citations A character vector with the formatted citations
#' @param page_language Language of the page, used to format the HTML code.
format_publications_for_year <- function(year, citations_df, page_language) {
  cat(glue("### {year}\n\n"))
  for(iC in 1:nrow(citations_df)) {
    cat(citations_df$Citation[iC], "\n\n")
    cat("<details><summary>", translations[['Abstract']][page_language], "</summary>", citations_df$Abstract[iC], "</details>\n")
  }
}


#' Load, format, and print out publications per year
#' @param page_language Language of the page, used to format the HTML code.
print_out_empra_publications <- function(page_language) {
  empra_publications <-
    yaml::read_yaml("empra-publications.yaml") |>
    purrr::map(~process_empra_publication(.)) |>
    list_rbind() |>
    mutate(Year = fct_rev(factor(Year))) |>
    arrange(Year, FirstAuthor) |>
    group_by(Year) |>
    group_walk(~format_publications_for_year(.y$Year[1], .x, page_language))
}


#' Format a single conference submission
#'
#' @param entry A list with fields "title", "authors", "conference", "volume", "issue", "pages", "doi", "year".
#'
#' @returns tibble with columns "Year", "FirstAuthor", "Citation", "Abstract".
process_empra_conference <- function(entry) {
  tibble(Year = entry$year,
         FirstAuthor = entry$authors[[1]]$lastname,
         Citation = glue('{format_authors_list(entry$authors)} ({entry$year}) "{entry$title}"_{entry$conference}_'),
         Abstract = entry$abstract)
}

#' Load, format, and print out conference submissions per year
#' @param page_language Language of the page, used to format the HTML code.
print_out_empra_conferences <- function(page_language) {
  yaml::read_yaml("empra-conferences.yaml") |>
    purrr::map(~process_empra_conference(.)) |>
    list_rbind() |>
    mutate(Year = fct_rev(factor(Year))) |>
    arrange(Year, FirstAuthor) |>
    group_by(Year) |>
    group_walk(~format_publications_for_year(.y$Year[1], .x, page_language))
}


convert_authors <- function(input) {
  # Split the input into lines
  lines <- unlist(strsplit(input, "\n"))

  # Initialize an empty vector for formatted results
  output <- c()

  # Loop through the lines and process them
  for (i in seq(1, length(lines), by = 2)) {
    # Extract the name and lastname lines
    name_line <- lines[i]
    lastname_line <- lines[i + 1]

    # Extract the actual values using regex
    name <- sub(".*name: ", "", name_line)
    lastname <- sub(".*lastname: ", "", lastname_line)

    # Format and append to the output
    formatted <- paste0("    - ", lastname, ", ", name)
    output <- c(output, formatted)
  }

  # Combine the output into a single string
  result <- paste0(output, collapse = "\n")

  # Print the result
  cat(result)
}
