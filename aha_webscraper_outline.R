
## Author: Raley Long

## Title: AHA Webscraper Format

## First, you need to install/load in necessary packages. If can't load a package, you probably just to need
## to make sure it is installed. You can do that by going to Console below and doing install.package("____")

library(rvest)
library(purrr)

## Next, run the code below to get the functions you need to extract session title, paper title, and institution

# Function to extract session titles. Takes in link with "____" around it (all functions must take the link within "____")
session_titles <- function(html_link){
  html <- read_html(html_link)
  session_title <- html |>
    html_elements(css = ".subtitle") |>
    html_text() |>
    stringr::str_replace_all("\n|\t", "")
  return(session_title)
}

## Function to extract title of the papers
paper_titles <- function(html_link){
  html <- read_html(html_link)
  result <- html |>
    html_elements(css = ".persontitle a") |>
    html_text() |>
    stringr::str_replace_all("\n|\t|\r", "")

  return(result)
}

## Function to extract institution associated w/ each paper (note that this only works for the paper type of session not the panel)
instituitions_paper <- function(html_link){
  html <- read_html(html_link)
  result <- html |>
    html_elements(css = ".presenter") |>
    html_text() |>
    stringr::str_replace_all(".*,", "")
  return(result)
}

## To make this more efficent, it is necessary to create a vector that contains all the links you
## want to run. When making these lists, make sure to name them with the day number and year. It is also
## important to make sure that each list only contains links of that specific type. Specifically, there should
## be two vectors of the links: one for panels and one for paper presentations. You should write which one it is
## (paper or panel) in the name for this list. This step is important because it means that the session title, paper titles
## institutions, chair institution, etc. are connected to the same index/number position. Example name could be....

day1_paper_links_2022 <- c("_____",
                           "______",
                           "_______",)
day1_panel_links_2022 <- c("____",
                           "_____",
                           "_____", ) #Note that last link does not need a comma after it and won't work if there is one.

## Once you have the vectors of the links you want to use, you will map these functions across the vectors of links you have.

sessionTitles_dayNumber_Year <- purrr::map(vector_of_links, session_titles)

paperTitles_dayNumber_Year <- purrr::map(vector_of_links, paper_titles)

institutionsPaper_dayNumber_Year <- purrr::map(vector_of_links, institutions_paper)

institutionsChair_dayNumber_Year <- purrr::map(vector_of_links, chair_institution)

institutionsPanel_dayNumber_Year <- purrr::map(vector_of_links, panel_institution)

## If you want to see what the results are, just call the name of what you created.



## Panel Code: Use the same methodology as above but use these functions for the panel links.

institutions_panel <- function(html_link){
  html <- read_html(html_link)
  result <- html |>
    html_elements(css = ":nth-child(5) .affiliation") |>
    html_text() |>
    stringr::str_replace_all("\n|\t|\r", "")
  return(result)
}



chair_institution_panel <- function(html_link){
  html <- read_html(html_link)
  result <- html |>
    html_elements(css = ":nth-child(4) .affiliation") |>
    html_text() |>
    stringr::str_replace_all("\n|\t|\r", "")
  return(result)
}




## Next, we must convert these objects to a larger JSON Object. To create this, combine all the lists
## you made for session titles, paper title, paper institutions, chair institution, and panel institution.
## to combine lists you can give it a new name like...

sessionTitles_2022 <- c("first_list_here", "second_list_here", "etc.")

## You can do this for each groping.

## Once those are grouped together, we are going to make it a JSON File.

library(jsonlite)

file_2022 <- toJSON(list_name, pretty = TRUE, auto_unbox = TRUE)




