library(tidyverse)

library(glue)
library(slider)
library(yaml)

translations <- list("Abstract" = c("en" = "Abstract", "de" = "Abstrakt"))


#' Convert from "lastname, firstname" to "firstname lastname"
#'
#' @param last_first_name A character vector with the name in the format "lastname, firstname"
#'
#' @returns A character vector with the name in the format "firstname lastname"
to_first_last_name <- function(last_first_name) {
  name_bits <- unlist(str_split(last_first_name, ", "))
  paste(name_bits[2], name_bits[1])
}


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
  latex_authors <- ""
  for(iauthor in 1:length(yaml_entry$authors)) {
    if (iauthor > 1) {
      if (iauthor == length(yaml_entry$authors)) {
        if (length(yaml_entry$authors) == 2) {
          authors <- str_c(authors, " & ")
          latex_authors <- str_c(latex_authors, " \\& ")
        } else {
          authors <- str_c(authors, ", & ")
          latex_authors <- str_c(latex_authors, ", \\& ")
        }
      } else {
        authors <- str_c(authors, ", ")
        latex_authors <- str_c(latex_authors, ", ")
      }
    }
    authors <- str_c(authors, to_first_last_name(yaml_entry$authors[[iauthor]]))
    latex_authors <- str_c(latex_authors, "\\index{", yaml_entry$authors[[iauthor]], "}", to_first_last_name(yaml_entry$authors[[iauthor]]))
  }

  tibble(Title = yaml_entry$title,
         Authors = authors,
         LatexAuthors = latex_authors,
         FirstAuthor = yaml_entry$authors[[1]],
         SupervisorLastname = yaml_entry$supervisor,
         Supervisor = to_first_last_name(yaml_entry$supervisor),
         Abstract = yaml_entry$abstract,
         Content = ifelse(is.null(yaml_entry$content), NA, yaml_entry$content),
         Number = ifelse(is.null(yaml_entry$number), NA, yaml_entry$number),
         OSF = ifelse(is.null(yaml_entry$osf), NA, yaml_entry$osf),
         Prize = ifelse(is.null(yaml_entry$prize), NA, yaml_entry$prize),
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

  # creating consistent numbers, when needed
  group_by(Type) |>
  mutate(NeedsNumber = sum(is.na(Number)) == n()) |>
  arrange(SupervisorLastname, FirstAuthor) |>
  mutate(Number = ifelse(NeedsNumber, glue("{str_to_upper(str_sub(Type, 1, 1))}{row_number()}"), Number)) |>
  ungroup() |>
  mutate(Index = as.integer(str_extract(Number, "\\d+"))) |>
  arrange(Type, Index)
}


#' HTML formatted program for a given year
#'
#' @param empra_year Year of the program
#' @param page_language Page language (either "en" or "de")
#'
#' @returns A character vector with the HTML code for the program.
program_html <- function(empra_year, page_language) {
  # load program from YAML
  program_as_list <- yaml::read_yaml(sprintf("%d/program.yaml", empra_year))$program

  # convert to flat structure for a given language
  for(ientry in 1:length(program_as_list)) {
    entry <- program_as_list[[ientry]]
    df_entry <- list()
    for(ifield in 1:length(entry)) {
      if (is.list(entry[[ifield]])) {
        for(lang_entry in entry[[ifield]]) {
          if (names(lang_entry) == page_language) {
            df_entry[[names(entry[ifield])]] <- lang_entry[[page_language]]
          }
        }
      } else {
        df_entry[[names(entry[ifield])]] <- entry[[ifield]]
      }
    }
    program_as_list[[ientry]] <- as_tibble(df_entry)
  }
  program_df <- list_rbind(program_as_list)

  # format program for html
  program_df |>
    relocate(time, title, place) |>
    rename("When" = time, "What" = title, "Where" = place) |>
    kableExtra::kable() |>
    kableExtra::row_spec(seq(1, nrow(program_df), 2), background="#E8ECF4") |>
    kableExtra::kable_styling( bootstrap_options = c("striped"))
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
  if (!is.na(entry$Prize)) {
    cat(sprintf('<br/><br/><img src="assets/icons/%s-prize.svg" alt="Award" style="width:30px;"/>\n', entry$Prize), sep="")
  }
  cat(':::\n')
  cat('::: {.g-col-9}\n')
      cat("**", entry$Title, "**<br/>", entry$Authors, "<br/>_Supervisor: ", entry$Supervisor, "_<br/>", sep = "")
      if (!is.na(entry$Abstract)) cat("<details><summary>", translations[['Abstract']][page_language], "</summary>", entry$Abstract, "</details>")
    cat('\n:::\n')

    cat('::: {.g-col-1}\n')
      if (!is.na(entry$Content)) {
        cat('<a href="', entry$Year, "/", entry$Content, '" target="_blank">',
            sprintf('<img src="assets/icons/poster.svg" alt="Poster" style="width:30px;"/>', entry$Type),
            '</a>\n', sep="")
      }
    cat(':::\n')
    cat('::: {.g-col-1}\n')
    if (!is.na(entry$OSF)) {
      cat('<a href="https://osf.io/', entry$OSF, '" target="_blank"> <img src="assets/icons/osf.svg" alt="Poster" style="width:30px;"/></a>\n', sep="")
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
  cat("\\marginnote{", entry$Number, "}\n")
  cat("\\begin{center}\n")
  cat("\\textbf{", escape_latex(entry$Title), "}\\\\\n", entry$LatexAuthors, "\\\\\n", sep="")
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
  submissions |>
    filter(Type == submission_type) |>
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



