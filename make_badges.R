library(tidyverse)

library(rsvg)
library(qpdf)
library(xml2)

empra_year <- 2025
source("submission_utils.R")

# Read SVG templates
badge <- list("student" = read_xml("assets/badges/badge.svg"),
              "supervisor" = read_xml("assets/badges/badge-organiizer.svg"))

# Read group names
group_entries <- yaml::read_yaml(sprintf("%d/program.yaml", empra_year))$groups
empra_groups <- map_chr(group_entries, ~.[[1]])
names(empra_groups) <- map_chr(group_entries, ~names(.))

# Read all authors
extract_authors <- function(yaml_entry) {
  bind_rows(
    tibble(Author = yaml_entry$authors,
           Supervisor = to_first_last_name(yaml_entry$supervisor),
           Role = "student"),
    tibble(Author = yaml_entry$supervisor,
           Supervisor = to_first_last_name(yaml_entry$supervisor),
           Role = "supervisor")) |>
    mutate(Group = empra_groups[Supervisor])
}
all_authors <-
  yaml::read_yaml(sprintf("%d/submissions.yaml", empra_year)) |>
  purrr::map(~extract_authors(.)) |>
  list_rbind() |>
  distinct() |>
  mutate(Length = str_length(Author)) |>
  arrange(desc(Length)) |>
  mutate(NameComponents = str_split(Author, ", "),
         FirstName = purrr::map_chr(NameComponents, ~.[2]),
         LastName = purrr::map_chr(NameComponents, ~.[1])) |>
  select(-NameComponents) |>
  mutate(Filename = str_replace_all(str_c(FirstName, " ", LastName), " ", "-"))

# create folder for badges
fs::dir_create(paste0("badges/", empra_year))

create_badge <- function(empra_year, author) {
  # Step 2: Locate and modify text for specific elements
  # Modify the text for the empra-year element
  xml_find_first(badge[[author$Role]], "//*[@id='empra-year']") |>  xml_set_text(paste("EMPRA", empra_year))

  # Modify the text for the empra-name element
  name_node <- xml_find_first(badge[[author$Role]], "//*[@id='empra-name']")
  xml_set_text(name_node, author$FirstName)
  # xml_set_attr(name_node, "text-anchor", "middle")  # Horizontal centering
  # xml_set_attr(name_node, "x", "160.933")              # Center at the middle of the container
  # xml_set_attr(name_node, "dominant-baseline", "middle")

  xml_find_first(badge[[author$Role]], "//*[@id='empra-lastname']") |> xml_set_text(author$LastName)


  # Modify the text for the empra-group element
  xml_find_first(badge[[author$Role]], "//*[@id='empra-group']") |>  xml_set_text(author$Group)

  # Step 3: Save the updated SVG file
  output_path <- paste0("badges/", empra_year, "/", author$Filename)
  write_xml(badge[[author$Role]], paste0(output_path, ".svg"))
  rsvg_pdf(paste0(output_path, ".svg"), paste0(output_path, ".pdf"))
}

walk(1:nrow(all_authors), ~create_badge(empra_year, all_authors[., ]))
# walk(1, ~create_badge(empra_year, all_authors[., ]))

pdf_files <- map_chr(all_authors$Filename, ~paste0("badges/", empra_year, "/", ., ".pdf"))
svg_files <- map_chr(all_authors$Filename, ~paste0("badges/", empra_year, "/", ., ".svg"))
pdf_combine(input = pdf_files, output = paste0("badges/", empra_year, "/_badges-", empra_year, ".pdf"))
fs::file_delete(pdf_files)
fs::file_delete(svg_files)
