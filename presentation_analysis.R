
#Some Brief Analysis of a Year for the Presentation....

## Year I'm going to do: 1923

## Necessary Libraries
library(tidyverse)
library(ggplot2)
library(infer)
library(purrr)
library(dplyr)
library(stringr)
library(plyr)
library(tidyr)
library(RColorBrewer)


# Reading in the Data
data <- read.csv("aha_1975.csv")

## Cleaning Data

print(colnames(data))

colnames(data) <- c("Year", "Session_Title", "Type", "Chair",
                    "Chair_Institution", "Participants",
                    "Paper_Titles", "Institution", "Start_Time",
                    "Day","Topical_Index", "AHA_Affiliated_Societies")
print(colnames(data))

## Topical Indexes

topical_indexes <- as.list(data$Topical_Index)

topical_indexes <- Filter(nzchar, topical_indexes)

print(topical_indexes)

split_flatten <- function(x) {
  unlist(strsplit(x, ","))
}

topical_indexes <- lapply(topical_indexes, split_flatten)

topical_indexes <- unlist(topical_indexes, recursive = FALSE)

trim_space <- function(x){
  trimws(x, "both")
}

topical_indexes <- lapply(topical_indexes, trim_space)

topical_indexes <- unlist(topical_indexes)

print(topical_indexes)

## Visualizing these Indexes

frequency <- table(topical_indexes)

frequency_df <- data.frame(String = names(frequency), Frequency = as.numeric(frequency))

print(frequency_df)

print(sum(frequency_df$Frequency))

important_indexes <- frequency_df %>%
  filter(Frequency > 5)

print(important_indexes)

par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))

palette <- brewer.pal(n = 18, name = "Set2")

pie(important_indexes$Frequency, labels = important_indexes$String,
    col = palette, main = "Frequency of Topical Indexes >= 5 for 1975", cex = 0.75)

legend("topright", legend = important_indexes$String, fill = Dark2(length(important_indexes$String)), cex = 0.7)



indexes <- unique(topical_indexes)
print(indexes)









