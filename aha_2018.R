

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

day1_paper_2018 <- c("https://aha.confex.com/aha/2018/webprogram/Session16794.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16057.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16574.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16513.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16140.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15839.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16082.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16049.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16193.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16441.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16254.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15937.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15906.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16299.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16340.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16425.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16345.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16177.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15769.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16181.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16468.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15883.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16020.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16397.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16601.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15904.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15770.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16142.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16338.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16388.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16477.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16282.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16383.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16377.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16227.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16406.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16370.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16214.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16209.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16286.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16628.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16462.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16578.html")




day1_panel_2018 <- c("https://aha.confex.com/aha/2018/webprogram/Session17024.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session17023.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16164.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16378.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15959.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16348.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16539.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16792.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16689.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16586.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16565.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16655.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16784.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16015.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16803.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16540.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16333.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16553.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16363.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15810.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16350.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16368.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16198.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session17048.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16593.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16693.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16685.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15929.html")


day2_paper_2018 <- c("https://aha.confex.com/aha/2018/webprogram/Session16439.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16543.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16202.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16250.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15953.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16491.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16160.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15913.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15796.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16260.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16457.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16506.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16066.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16145.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16171.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15829.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16263.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15869.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15943.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15917.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16736.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16746.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16747.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16129.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16458.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16692.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16599.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15828.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16617.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16713.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16577.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16652.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16213.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16488.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15775.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16277.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15872.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15980.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16168.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16148.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16279.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15772.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16220.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16243.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16008.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16552.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16563.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16544.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16698.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16749.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16750.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16762.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16605.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15850.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16508.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16631.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16777.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16602.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16677.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16708.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16670.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16799.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16396.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16217.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15820.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16341.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16537.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15878.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16086.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16104.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15751.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16536.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16151.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15804.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16012.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16019.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16018.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16063.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16554.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16752.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16753.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16755.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15893.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16726.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16107.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16675.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16705.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16680.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16697.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16638.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16683.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16584.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16570.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16381.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16796.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16401.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16294.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16287.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15933.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16320.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16247.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16290.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16265.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15819.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16339.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16355.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16542.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16376.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16323.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16489.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16460.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16547.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16238.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16702.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16748.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16764.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15952.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16024.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16372.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16411.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16637.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16673.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16711.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16650.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16651.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16653.html")




day2_panel_2018 <- c("https://aha.confex.com/aha/2018/webprogram/Session16403.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16813.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16246.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16428.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16559.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16067.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16409.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16541.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16248.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16452.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16125.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15877.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16059.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16954.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16727.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16674.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16688.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16687.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16032.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16654.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15949.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16659.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16781.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16420.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16332.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16230.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16580.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16701.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16716.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16782.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16802.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16538.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16581.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16562.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session17108.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16728.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16619.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16686.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16635.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16646.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16648.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16649.html")




day3_paper_2018 <- c("https://aha.confex.com/aha/2018/webprogram/Session16407.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15709.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16038.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16039.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16034.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16031.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16138.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16085.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16122.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16099.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16276.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16251.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16335.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16357.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16203.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16114.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16761.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16765.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16770.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16604.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16156.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16329.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16639.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16678.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16789.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16225.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16442.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16309.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16100.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16101.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15793.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15762.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15832.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15840.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16326.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16252.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16257.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16545.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16234.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16266.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16253.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16221.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16331.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16042.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16766.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16768.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16769.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16951.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16603.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16416.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15945.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16328.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15817.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16778.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16640.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16681.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16790.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16589.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16611.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16791.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16304.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16166.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16278.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16555.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16500.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16551.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16548.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16533.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15942.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16047.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16121.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16150.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16502.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16289.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16636.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16422.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16641.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16682.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16572.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16795.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16573.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16447.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16306.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16556.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16535.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16482.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16430.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16404.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16379.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16223.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16358.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16302.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16495.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16022.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16069.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16759.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16767.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16771.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16346.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16075.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16232.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16364.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16660.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16714.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16642.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session17114.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16608.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16662.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16665.html")


day3_panel_2018 <- c("https://aha.confex.com/aha/2018/webprogram/Session16647.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16667.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16779.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16103.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16801.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16314.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16167.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15982.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15985.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16249.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session17040.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16729.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16927.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16783.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15841.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16074.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15774.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16389.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16514.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session17119.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16730.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16625.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16690.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16597.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16296.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16669.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16567.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16466.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16760.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16427.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15833.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15939.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15966.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16204.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16534.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16709.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16048.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16415.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16712.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16560.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16798.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16780.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16786.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16575.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16531.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16185.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16319.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16433.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16222.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session17070.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16956.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16663.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16664.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16609.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16804.html")


day4_paper_2018 <- c("https://aha.confex.com/aha/2018/webprogram/Session16569.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16800.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16300.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15729.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15944.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15905.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15970.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16179.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16450.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16391.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16444.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16546.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16557.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16292.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16334.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15981.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16318.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16123.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session17107.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16658.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16633.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16668.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16715.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16643.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16576.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16788.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16308.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16960.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15925.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16215.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16078.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16224.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16165.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16183.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16436.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16395.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16367.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16155.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16163.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15967.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16205.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16097.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16240.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16644.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16676.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15862.html")



day4_panel_2018 <- c("https://aha.confex.com/aha/2018/webprogram/Session16732.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15948.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15965.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15986.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16108.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16130.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16568.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16797.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16237.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16785.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session15960.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16090.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16297.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16558.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16089.html",
                     "https://aha.confex.com/aha/2018/webprogram/Session16486.html")


## Combing Links

paper_2018 <- c(day1_paper_2018, day2_paper_2018, day3_paper_2018, day4_paper_2018)

panel_2018 <- c(day1_panel_2018, day2_panel_2018, day3_panel_2018, day4_panel_2018)

links_2018 <- c(paper_2018, panel_2018)

## Mapping Functions

panelsessionTitles_2018 <- purrr::map(panel_2018, session_titles)

papersessionTitles_2018 <- purrr::map(paper_2018, session_titles)

paperTitles_2018 <- purrr::map(paper_2018, paper_titles)

institutionsPaper_2018 <- purrr::map(paper_2018, institutions)

institutionPanel_2018 <- purrr::map(panel_2018, institutions)

## List

aha_2018_paper <- list(session_titles = papersessionTitles_2018,
                     paper_titles = paperTitles_2018,
                     paper_institutions = institutionsPaper_2018)


aha_2018_panel <- list(session_titles = panelsessionTitles_2018,
                     panel_institutions = institutionPanel_2018)

## JSON Files

library(jsonlite)

file_2018_paper = toJSON(aha_2018_paper, pretty = TRUE, auto_unbox = TRUE)

write(file_2018_paper, "aha2018_paper.json")

file_2018_panel = toJSON(aha_2018_panel, pretty = TRUE, auto_unbox = TRUE)

write(file_2018_panel, "aha2018_panel.json")








