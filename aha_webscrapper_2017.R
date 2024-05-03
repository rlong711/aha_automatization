
library(rvest)

## starts for year 2017

html_test <- read_html("https://aha.confex.com/aha/2017/webprogram/Session15462.html")

paper_titles <- html_test |>
  html_elements(css = ".persontitle a") |>
  html_text()

paper_titles

paper_institutions <- html_test |>
  html_elements(css = ".presenter .affiliation") |>
  html_text()

paper_institutions

chair_institution <- html_test |>
  html_elements(css = ".affiliation:nth-child(1)") |>
  html_text()

chair_institution

session_title <- html_test |>
  html_elements(css = ".subtitle") |>
  html_text() |>
  stringr::str_replace_all("\n|\t", "")

session_title

## Create Functions

session_title_paper <- function(link){
  html <- read_html(link)
  result <- html |>
    html_elements(css = ".subtitle") |>
    html_text() |>
    stringr::str_replace_all("\n|\t", "")

  return(result)
}

chair_institution_paper <- function(link){
  html <- read_html(link)
  result <- html |>
    html_elements(css = ".affiliation:nth-child(1)") |>
    html_text()

  return(result)
}

paper_titles <- function(link){
  html <- read_html(link)
  result <- html |>
    html_elements(css = ".persontitle a") |>
    html_text()

  return(result)
}

paper_institutions <- function(link){
  html <- read_html(link)
  result <- html |>
    html_elements(css = ".presenter .affiliation") |>
    html_text()

  return(result)
}

## Maping function to list of vectors

daynumber_year_sessiontitle <- purrr::map(vector_of_links, session_title_paper)

daynumber_year_chairinstitution <- purrr::map(vector_of_links, chair_institution_paper)

daynumber_year_paperinstitutions <- purrr::map(vector_of_links, paper_institutions)

daynumber_year_papertitles <- purrr::map(vector_of_links, paper_titles)


## Functions for Panel type

panel_title <- function(x){
  html <- read_html(x)

  result <- html |>
    html_elements(css = ".subtitle") |>
    html_text() |>
    stringr::str_replace_all("\n|\t", "")

  return(result)
}

panel_chair_institution <- function(x){
  html <- read_html(x)

  result <- html |>
    html_elements(css = ":nth-child(5) .affiliation") |>
    html_text()

  return(result)
}

panel_institution <- function(x){
  html <- read_html(x)

  result <- html |>
    html_elements(css = ":nth-child(6) .affiliation") |>
    html_text()

  return(result)
}





