

# Loading Packages

library(rvest)
library(purrr)
library(jsonlite)


## Session Title Function
session_titles <- function(html_link){
  html <- read_html(html_link)
  session_title <- html |>
    html_elements(css = ".subtitle") |>
    html_text() |>
    stringr::str_replace_all("\n|\t", "")
  return(session_title)
}

## Paper Title Function
paper_titles <- function(html_link){
  html <- read_html(html_link)
  result <- html |>
    html_elements(css = ".persontitle a") |>
    html_text() |>
    stringr::str_replace_all("\n|\t|\r", "")

  return(result)
}



#Institutions
institutions <- function(html_link){
  html <- read_html(html_link)
  result <- html |>
    html_elements(css = ".affiliation") |>
    html_text() |>
    stringr::str_replace_all("\n|\t|\r", "")
  return(result)
}




## Vectors of Links

day1_paper_2020 <- c("")



day1_panel_2020 <- c("")



day2_paper_2020 <- c("")




day2_panel_2020 <- c("")



day3_paper_2020 <- c("")




day3_panel_2020 <- c("")




day4_paper_2020 <- c("")



day4_panel_2020 <- c("")


## Combing Links

paper_2018 <- c(day1_paper_2018, day2_paper_2018, day3_paper_2018, day4_paper_2018)

panel_2018 <- c(day1_panel_2018, day2_panel_2018, day3_panel_2018, day4_panel_2018)

links_2018 <- c(paper_2018, panel_2018)

## Mapping Functions

sessiontitles_2018 <- purrr::map(links_2018, session_titles)

institution_2018 <- purrr::map(links_2018, institutions)

papertitle_2018 <- purrr::map(paper_2018, paper_titles)

## JSON File Creation

aha_2018 <- c(sessiontitles_2020, institution_2020, papertitle_2020)

file_2018 = toJSON(aha_2020, pretty = TRUE, auto_unbox = TRUE)

write(file_2018, "aha2018.json")








