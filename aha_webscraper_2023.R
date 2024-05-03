
library(rvest)
aha_2023 <- read_html("https://aha.confex.com/aha/2023/meetingapp.cgi/Program/1068")



local_html <- read_html("AHA Sessions.html")

session_titles_2023 <- local_html |>
  html_elements(css = ".SessionListTitle a") |>
  html_text() |>
  stringr::str_replace_all("\\t|\\n", "") 

session_titles_2023
 

session_titles_2023_clean <- 
  gsub("\\d", "", session_titles_2023)
session_titles_2023_clean

## Session Institutions 

aha_2023_instituitions_a <- read_html("Index_A.html")

aha_2023_institutions_a <- aha_2023_people |>
  html_elements(".affiliation") |>
  html_text()


aha_2023_institutions_a

## 

