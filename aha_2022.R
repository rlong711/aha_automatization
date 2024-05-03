
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

institutions_panel <- function(html_link){
  html <- read_html(html_link)
  result <- html |>
    html_elements(css = ".people") |>
    html_text() |>
    stringr::str_replace_all("\n|\t|\r", "")
  return(result)
}



## To make this more efficent, it is necessary to create a vector that contains all the links you
## want to run. When making these lists, make sure to name them with the day number and year. It is also
## important to make sure that each list only contains links of that specific type. Specifically, there should
## be two vectors of the links: one for panels and one for paper presentations. You should write which one it is
## (paper or panel) in the name for this list. This step is important because it means that the session title, paper titles
## institutions, chair institution, etc. are connected to the same index/number position. Example name could be....

day1_paper_links_2022 <- c("https://aha.confex.com/aha/2022/webprogram/Session22292.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22325.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22521.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22657.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23068.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22816.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22050.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22913.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22796.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22374.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22412.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22624.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22726.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22822.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22824.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22825.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22188.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22193.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22918.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22904.html")


day2_paper_links_2022 <- c("https://aha.confex.com/aha/2022/webprogram/Session23081.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22235.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22242.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22717.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22829.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22946.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22921.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22798.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22903.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22349.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22473.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22709.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22735.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22834.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22835.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22836.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22881.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22789.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22949.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23036.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22786.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22901.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22285.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22296.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22360.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22839.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22840.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22793.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22900.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22267.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22390.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22456.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22469.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22685.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22711.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22843.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22844.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22845.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22788.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22952.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22953.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22801.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22923.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22846.html")


day2_panel_links_2022 <- c("https://aha.confex.com/aha/2022/webprogram/Session23027.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22487.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22583.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22828.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22863.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23053.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23029.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22719.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23073.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22143.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22557.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22567.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22256.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22485.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23173.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22800.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22113.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22687.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22199.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22317.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22842.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22923.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22984.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23004.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23031.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23142.html")



day3_paper_links_2022 <- c("https://aha.confex.com/aha/2022/webprogram/Session22415.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22558.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22284.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22017.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22290.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22699.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22833.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22847.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22851.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23033.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23034.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23035.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22802.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22813.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22560.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22597.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22238.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22177.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22152.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22059.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22659.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22445.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22252.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22849.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22850.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22791.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23037.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23038.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22803.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22930.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22274.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22119.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22455.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22461.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22434.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22714.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22436.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22683.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22201.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22323.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22320.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22254.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22181.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23040.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22804.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22878.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22095.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22277.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22628.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22329.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22438.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22499.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22848.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22855.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22310.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23043.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23044.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23045.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22806.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22894.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22858.html")


day3_panel_links_2022 <- c("https://aha.confex.com/aha/2022/webprogram/Session23058.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22591.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22491.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23176.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23080.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22294.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22671.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22790.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22941.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22867.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23074.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23064.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22720.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23180.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22777.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22319.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22326.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22419.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22444.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22569.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23050.html")

day4_paper_links_2022 <- c("https://aha.confex.com/aha/2022/webprogram/Session22308.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22382.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22480.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22792.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23047.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22049.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22895.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22688.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22585.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22232.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22479.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22251.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22734.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23048.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23049.html")


day4_panel_links_2022 <- c("https://aha.confex.com/aha/2022/webprogram/Session22178.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22590.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23046.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23071.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22510.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22383.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23147.html")



day1_panel_links_2022 <- c("https://aha.confex.com/aha/2022/webprogram/Session22114.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22589.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22676.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22543.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session23075.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22595.html",
                           "https://aha.confex.com/aha/2022/webprogram/Session22935.html")

paper_links_2022 <- c(day1_paper_links_2022, day2_paper_links_2022, day3_paper_links_2022, day4_paper_links_2022)

panel_links_2022 <- c(day1_panel_links_2022, day2_panel_links_2022, day3_panel_links_2022, day4_panel_links_2022)

links_2022 <- c(paper_links_2022, panel_links_2022)




## Once you have the vectors of the links you want to use, you will map these functions across the vectors of links you have.

panelsessionTitles_2022 <- purrr::map(panel_links_2022, session_titles)

papersessionTitles_2022 <- purrr::map(paper_links_2022, session_titles)

paperTitles_2022 <- purrr::map(paper_links_2022, paper_titles)

institutionsPaper_2022 <- purrr::map(paper_links_2022, instituitions_paper)

institutionPanel_2022 <- purrr::map(panel_links_2022, institutions_panel)

aha_2022_paper <- list(session_titles = papersessionTitles_2022,
                       paper_titles = paperTitles_2022,
                       paper_institutions = institutionsPaper_2022)


aha_2022_panel <- list(session_titles = panelsessionTitles_2022,
                       panel_institutions = institutionPanel_2022)




library(jsonlite)

file_2022_paper = toJSON(aha_2022_paper, pretty = TRUE, auto_unbox = TRUE)

write(file_2022_paper, "aha2022_paper.json")

file_2022_panel = toJSON(aha_2022_panel, pretty = TRUE, auto_unbox = TRUE)

write(file_2022_panel, "aha2022_panel.json")












