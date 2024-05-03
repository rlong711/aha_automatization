

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

day1_paper_2016 <- c("https://aha.confex.com/aha/2016/webprogram/Session13458.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13026.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13085.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13161.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13192.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13237.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13268.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13302.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13341.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13355.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13369.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13483.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13501.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13541.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13574.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13583.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13586.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13588.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13589.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13634.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13637.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13641.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13647.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13655.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13790.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13805.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13817.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13927.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13928.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13929.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13930.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13777.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13742.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13912.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13320.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13869.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13924.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14011.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13870.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12766.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13070.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13153.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13155.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13208.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13246.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13263.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13368.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13475.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13488.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13517.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13537.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13557.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13614.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13630.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13650.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13657.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13787.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13810.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13934.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13935.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13937.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13354.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13877.html")




day1_panel_2016 <- c("https://aha.confex.com/aha/2016/webprogram/Session14068.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12760.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13472.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13721.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12904.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12723.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13356.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13366.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13527.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13643.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13645.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13936.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13860.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13853.html")




day2_paper_2016 <- c("https://aha.confex.com/aha/2016/webprogram/Session13383.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12758.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12927.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12935.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13013.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13222.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13245.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13331.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13336.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13344.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13361.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13395.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13399.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13444.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13492.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13514.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13544.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13594.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13599.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13612.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13666.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13786.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13789.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13820.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13941.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13942.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13949.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13950.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13779.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13751.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13422.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13902.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13792.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12783.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12791.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12830.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12854.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12980.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13083.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13235.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13389.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13406.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13408.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13409.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13435.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13449.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13466.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13556.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13568.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13822.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13808.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13816.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13819.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13600.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13951.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13953.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13954.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13994.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13741.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13439.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13901.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13793.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13975.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14019.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14012.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14081.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12649.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12923.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13012.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13176.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13204.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13209.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13214.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13284.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13343.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13390.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13443.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13487.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13515.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13552.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13570.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13639.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13659.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13663.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12944.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13957.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13958.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12981.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13995.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14034.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13610.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13370.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13867.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14035.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13918.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13794.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13795.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13874.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13748.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13783.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13796.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13718.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13720.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13715.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13727.html")





day2_panel_2016 <- c("https://aha.confex.com/aha/2016/webprogram/Session14008.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13813.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14006.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13825.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13473.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13915.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13807.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14014.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14077.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12753.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12982.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13377.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13401.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13662.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13669.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13959.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13234.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13923.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13914.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13854.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13933.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13882.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14017.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13740.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13503.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13292.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13363.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13479.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13601.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14154.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13242.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13864.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13811.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13955.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13956.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13952.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13964.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13714.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13733.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14071.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13716.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13722.html")




day3_paper_2016 <- c("https://aha.confex.com/aha/2016/webprogram/Session13889.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14013.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13277.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12752.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12806.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13011.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13034.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13166.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13206.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13265.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13295.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13309.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13373.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13416.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13427.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13445.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13448.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13462.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13587.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13602.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13642.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13523.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13270.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13126.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13266.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13872.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13961.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13962.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13963.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13781.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13752.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13904.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13906.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13917.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13863.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13797.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13731.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14074.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14010.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12776.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12878.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13076.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13165.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13276.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13337.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13338.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13360.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13429.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13482.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13509.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13569.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13651.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13654.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13664.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13510.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13379.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13312.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13968.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13969.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13971.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13978.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13753.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13150.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13903.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13857.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13701.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13884.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14075.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13879.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13760.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14015.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12784.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12945.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13090.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13116.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13199.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13228.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13340.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13372.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13413.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13431.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13438.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13485.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13511.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13539.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13617.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13646.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13648.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13665.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13668.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13006.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13322.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13806.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13815.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13818.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13981.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13982.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13621.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13754.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13755.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13430.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13858.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13856.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13746.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13799.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13891.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14076.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13732.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13866.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13946.html")



day3_panel_2016 <- c("https://aha.confex.com/aha/2016/webprogram/Session13920.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14005.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13554.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13644.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13661.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13821.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13960.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13892.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14067.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12838.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13966.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13211.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13272.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13437.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13484.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13273.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13993.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13824.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13798.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14059.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14009.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14025.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14000.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13461.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13653.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13979.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13980.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13868.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14070.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13719.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13726.html")


day4_paper_2016 <- c("https://aha.confex.com/aha/2016/webprogram/Session12856.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12920.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13096.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13129.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13133.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13231.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13247.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13299.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13402.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13442.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13500.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13529.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13560.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13603.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13611.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13615.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13633.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13658.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13660.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13785.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13788.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13802.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13804.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13983.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13985.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13986.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13905.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13871.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12816.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12922.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12964.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session12989.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13003.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13040.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13127.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13229.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13252.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13282.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13285.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13358.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13387.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13477.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13486.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13494.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13506.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13584.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13605.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13625.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13626.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13652.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13656.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13667.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13008.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13784.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13987.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13988.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13989.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13990.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13143.html")




day4_panel_2016 <- c("https://aha.confex.com/aha/2016/webprogram/Session14078.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14002.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14072.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13099.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13236.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13469.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13573.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13826.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13145.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13984.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14080.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14003.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session14069.html",
                     "https://aha.confex.com/aha/2016/webprogram/Session13425.html")



## Combing Links

paper_2016 <- c(day1_paper_2016, day2_paper_2016, day3_paper_2016, day4_paper_2016)

panel_2016 <- c(day1_panel_2016, day2_panel_2016, day3_panel_2016, day4_panel_2016)

links_2016 <- c(paper_2016, panel_2016)

## Mapping Functions

panelsessionTitles_2016 <- purrr::map(panel_links_2016, session_titles)

papersessionTitles_2016 <- purrr::map(paper_links_2016, session_titles)

paperTitles_2016 <- purrr::map(paper_links_2016, paper_titles)

institutionsPaper_2016 <- purrr::map(paper_links_2016, instituitions_paper)

institutionPanel_2016 <- purrr::map(panel_links_2016, institutions_panel)

## List

aha_2016_paper <- list(session_titles = papersessionTitles_2016,
                     paper_titles = paperTitles_2016,
                     paper_institutions = institutionsPaper_2016)


aha_2016_panel <- list(session_titles = panelsessionTitles_2016,
                     panel_institutions = institutionPanel_2016)

## JSON Files

library(jsonlite)

file_2016_paper = toJSON(aha_2016_paper, pretty = TRUE, auto_unbox = TRUE)

write(file_2016_paper, "aha2016_paper.json")

file_2016_panel = toJSON(aha_2016_panel, pretty = TRUE, auto_unbox = TRUE)

write(file_2016_panel, "aha2016_panel.json")








