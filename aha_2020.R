

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

day1_paper_2020 <- c("https://aha.confex.com/aha/2020/webprogram/Session19064.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19325.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19468.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19483.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19506.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19576.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19611.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19649.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19684.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19721.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19979.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19989.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20071.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20101.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20206.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20313.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20329.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20330.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20565.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20595.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20567.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20391.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20681.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20680.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19758.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19246.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20117.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20500.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19850.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19346.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19546.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19646.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19683.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19702.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19795.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19946.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19970.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20111.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20118.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20182.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20213.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20277.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20311.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20347.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20596.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20598.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20599.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20570.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19835.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20507.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20683.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20682.html")



day1_panel_2020 <- c("https://aha.confex.com/aha/2020/webprogram/Session20462.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20430.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20591.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19028.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19284.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19796.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19994.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20891.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20516.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20679.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20455.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20632.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20402.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20490.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20512.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20742.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20420.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20740.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19105.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19192.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19525.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19652.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19728.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19804.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19865.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20048.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20597.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20630.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20502.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20743.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20664.html")



day2_paper_2020 <- c("https://aha.confex.com/aha/2020/webprogram/Session20419.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20426.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19231.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19433.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19586.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19635.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19648.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19706.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19871.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19943.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20041.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20085.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20165.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20247.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20299.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20325.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20358.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20556.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20575.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20612.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20601.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20574.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20393.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20652.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20708.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20687.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20686.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20397.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20503.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20469.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19235.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19265.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19473.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19563.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19773.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19806.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19856.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19889.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20090.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20133.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20167.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20300.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20306.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20323.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19401.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20576.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20602.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20577.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20571.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20651.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20514.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19719.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20711.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20693.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20695.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20694.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20398.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20407.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20497.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20414.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19115.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19257.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19318.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19440.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19549.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19791.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19834.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19848.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19901.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19920.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20022.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20084.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20162.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20229.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20236.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20303.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20312.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20043.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20558.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20447.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20515.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20508.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20648.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20401.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19243.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20646.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20654.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session18915.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19169.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19191.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19374.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19416.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19437.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19612.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19642.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19712.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19978.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20095.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20139.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19604.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20342.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20357.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20062.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19602.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20629.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20564.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20579.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20603.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20580.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20699.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20698.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20697.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20400.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19248.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20475.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20655.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20706.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20704.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20131.html")




day2_panel_2020 <- c("https://aha.confex.com/aha/2020/webprogram/Session20433.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19394.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19823.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20140.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20170.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20256.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20285.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20889.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20417.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19352.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20644.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20635.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20473.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20425.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20432.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19151.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19472.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19654.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20074.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20186.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20272.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20366.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20563.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20392.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20616.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20506.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20269.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20636.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20513.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20611.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20421.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19596.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19552.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19692.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20034.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20637.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20452.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20511.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20435.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20431.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20434.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19255.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19553.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19585.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19933.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20068.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20415.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20638.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20501.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20700.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20702.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20701.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20707.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20703.html")



day3_paper_2020 <- c("https://aha.confex.com/aha/2020/webprogram/Session20316.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session18886.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19239.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19372.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19403.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19732.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19816.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19911.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19992.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20288.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20297.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20307.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20040.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20244.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20608.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20584.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20605.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20394.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20685.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20709.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20404.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20504.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20557.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19187.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20375.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session18863.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19137.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19321.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19459.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19491.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19547.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19655.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19931.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20093.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20102.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20104.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20232.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20082.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20257.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20600.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20606.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20582.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20517.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19240.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20674.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19512.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20713.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20405.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20509.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19550.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20376.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20547.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20583.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20377.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19337.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19395.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19449.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19466.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19594.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19660.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19782.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19790.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19917.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19995.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20157.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20204.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20305.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19837.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20586.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20609.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20610.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20518.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19194.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20717.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20716.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20715.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20470.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20406.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19957.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20510.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19784.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19114.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19339.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19389.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19485.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19501.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20208.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19623.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19709.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19797.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19854.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19877.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19909.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19919.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19945.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20163.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20223.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20304.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19852.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20653.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20274.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20720.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20719.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20718.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20454.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20403.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20569.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20499.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20666.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20760.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20479.html")




day3_panel_2020 <- c("https://aha.confex.com/aha/2020/webprogram/Session20545.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20472.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20422.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20746.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19075.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19696.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20023.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20099.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20266.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20267.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20282.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20751.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20712.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20710.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19629.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19958.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19214.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19467.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19582.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19752.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19937.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20281.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20368.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20897.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20585.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20453.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20647.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20639.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20521.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20249.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20741.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20747.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19190.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19746.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19810.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20004.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20035.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20264.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20896.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20009.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20640.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20607.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20466.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20759.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20744.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20423.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20184.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20202.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20592.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19817.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20641.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20649.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20372.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20725.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20726.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20724.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20723.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20534.html")




day4_paper_2020 <- c("https://aha.confex.com/aha/2020/webprogram/Session19034.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19195.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19463.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19768.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19832.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19888.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19983.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19993.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20073.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20136.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20211.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20243.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20327.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20650.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20733.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20729.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19152.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19210.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19323.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19420.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19471.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19518.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19634.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19727.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19794.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19849.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19929.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20011.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20017.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20089.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20273.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20348.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20364.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20731.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20732.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20069.html")



day4_panel_2020 <- c("https://aha.confex.com/aha/2020/webprogram/Session18947.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20427.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20424.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19165.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19232.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19426.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19562.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19973.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20129.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20911.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20416.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20730.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20642.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20738.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20748.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19335.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19378.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session19867.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20390.html",
                     "https://aha.confex.com/aha/2020/webprogram/Session20878.html")


## Combing Links

paper_2020 <- c(day1_paper_2020, day2_paper_2020, day3_paper_2020, day4_paper_2020)

panel_2020 <- c(day1_panel_2020, day2_panel_2020, day3_panel_2020, day4_panel_2020)

links_2020 <- c(paper_2020, panel_2020)

## Mapping Functions

panelsessionTitles_2020 <- purrr::map(panel_2020, session_titles)

papersessionTitles_2020 <- purrr::map(paper_2020, session_titles)

paperTitles_2020 <- purrr::map(paper_2020, paper_titles)

institutionsPaper_2020 <- purrr::map(paper_2020, institutions)

institutionPanel_2020 <- purrr::map(panel_2020, institutions)

## List

aha_2020_paper <- list(session_titles = papersessionTitles_2020,
                       paper_titles = paperTitles_2020,
                       paper_institutions = institutionsPaper_2020)


aha_2020_panel <- list(session_titles = panelsessionTitles_2020,
                       panel_institutions = institutionPanel_2020)

## JSON Files

library(jsonlite)

file_2020_paper = toJSON(aha_2020_paper, pretty = TRUE, auto_unbox = TRUE)

write(file_2020_paper, "aha2020_paper.json")

file_2020_panel = toJSON(aha_2020_panel, pretty = TRUE, auto_unbox = TRUE)

write(file_2020_panel, "aha2020_panel.json")








