

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

day1_paper_2019 <- c("https://aha.confex.com/aha/2019/webprogram/Session18201.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18433.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17143.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17466.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17584.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17750.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17811.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17915.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17991.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18017.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18073.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18117.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18151.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18289.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18493.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18606.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18028.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18575.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18576.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18464.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17416.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17735.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17762.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17839.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17919.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17950.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17951.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17977.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18068.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18072.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18205.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18264.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18325.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18297.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18495.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17899.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18084.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17757.html")


day1_panel_2019 <- c("https://aha.confex.com/aha/2019/webprogram/Session18796.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18640.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18397.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17341.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17449.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17467.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17520.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17622.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17783.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17978.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18499.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18496.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18649.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18450.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18392.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18641.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17913.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18015.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18175.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18292.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18369.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18387.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18829.html")


day2_paper_2019 <- c("https://aha.confex.com/aha/2019/webprogram/Session17254.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17414.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17472.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17788.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17812.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17831.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17854.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17925.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17990.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18145.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18153.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18158.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18167.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18543.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18544.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18215.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17287.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17796.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17674.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18434.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18611.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18478.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18513.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18486.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18389.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17724.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17848.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17850.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17881.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17900.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18098.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18246.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18284.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17635.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17631.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18225.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18545.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18547.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18522.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17828.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17897.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17817.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18479.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18511.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18258.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17354.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18453.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18331.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17525.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17628.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17808.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17862.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17917.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17985.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18040.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18046.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18091.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18099.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18174.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18255.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18381.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17636.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18797.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18523.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18548.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18546.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18550.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17557.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18475.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18435.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18538.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18480.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18465.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18452.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17307.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17455.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17763.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17786.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17791.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17887.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17963.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17987.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18222.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18250.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18340.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18551.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18552.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18553.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17702.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17481.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18155.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18416.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18436.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18481.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18515.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18600.html")


day2_panel_2019 <- c("https://aha.confex.com/aha/2019/webprogram/Session18390.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18432.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18460.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18399.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17326.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18119.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18206.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18353.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18366.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17637.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18834.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18542.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18508.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18394.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18398.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17395.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17478.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17658.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17805.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17842.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18027.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18067.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18172.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18383.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18525.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18506.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18831.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18620.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18687.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18071.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18352.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18359.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17741.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18509.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18614.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18487.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18541.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18759.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18459.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18632.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18633.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18638.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17327.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17353.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18094.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18256.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18337.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18346.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18379.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17801.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17491.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18489.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17428.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18503.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18517.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18582.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18583.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18584.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18585.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18589.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18586.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18587.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18588.html")


day3_paper_2019 <- c("https://aha.confex.com/aha/2019/webprogram/Session18448.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17333.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17376.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17543.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17543.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17782.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17873.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17918.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17965.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18064.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18104.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18111.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18203.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18303.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18317.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18559.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18561.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18520.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17901.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18032.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18333.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18613.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18462.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18505.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18615.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17351.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17417.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17463.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17485.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17551.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17609.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17659.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17798.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17853.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17893.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17989.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18038.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18052.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18562.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18565.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18521.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18473.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17916.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18488.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17547.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18148.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18238.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18294.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17775.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18610.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18482.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18504.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18616.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18601.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18407.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18617.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18621.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17436.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17550.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17686.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17794.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17818.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17851.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17867.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17912.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17962.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18143.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18336.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18191.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18308.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17727.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17920.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18510.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18483.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18455.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18625.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17489.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17882.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17992.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18018.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18051.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18075.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18081.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18101.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18164.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18213.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18270.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18310.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18324.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17868.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18147.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17474.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18484.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18512.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17866.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18747.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18443.html")


day3_panel_2019 <- c("https://aha.confex.com/aha/2019/webprogram/Session18430.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18430.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18560.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17426.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17797.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17846.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17988.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18060.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18078.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18240.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18358.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18734.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18639.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18645.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18440.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17512.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18118.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18139.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18204.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18334.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18049.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18839.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18456.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18431.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18624.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17150.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18643.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18744.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17319.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18354.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18114.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18229.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18835.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17914.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18622.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18393.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18636.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17298.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17359.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17666.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18086.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18218.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18380.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18268.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18530.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17562.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18592.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18593.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18595.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18647.html")



day4_paper_2019 <- c("https://aha.confex.com/aha/2019/webprogram/Session17258.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17865.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17971.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18089.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18142.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18154.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18187.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18190.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18216.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18269.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18316.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18321.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17826.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18290.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18597.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17524.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18451.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17480.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17573.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17627.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17642.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17713.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17803.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17820.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17864.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17922.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18034.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18162.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18217.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18323.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18373.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17829.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17928.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18598.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18410.html")


day4_panel_2019 <- c("https://aha.confex.com/aha/2019/webprogram/Session18449.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18396.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18768.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18648.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18458.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17996.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17997.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18135.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18211.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18347.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18811.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18037.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18502.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18439.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18642.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17332.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session17755.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18223.html",
                     "https://aha.confex.com/aha/2019/webprogram/Session18386.html")

## Combing Links

paper_2019 <- c(day1_paper_2019, day2_paper_2019, day3_paper_2019, day4_paper_2019)

panel_2019 <- c(day1_panel_2019, day2_panel_2019, day3_panel_2019, day4_panel_2019)

links_2019 <- c(paper_2019, panel_2019)

## Mapping Functions

panelsessionTitles_2019 <- purrr::map(panel_2019, session_titles)

papersessionTitles_2019 <- purrr::map(paper_2019, session_titles)

paperTitles_2019 <- purrr::map(paper_2019, paper_titles)

institutionsPaper_2019 <- purrr::map(paper_2019, institutions)

institutionPanel_2019 <- purrr::map(panel_2019, institutions)

## List

aha_2019_paper <- list(session_titles = papersessionTitles_2019,
                     paper_titles = paperTitles_2019,
                     paper_institutions = institutionsPaper_2019)


aha_2019_panel <- list(session_titles = panelsessionTitles_2019,
                     panel_institutions = institutionPanel_2019)

## JSON Files

library(jsonlite)

file_2019_paper = toJSON(aha_2019_paper, pretty = TRUE, auto_unbox = TRUE)

write(file_2019_paper, "aha2019_paper.json")

file_2019_panel = toJSON(aha_2019_panel, pretty = TRUE, auto_unbox = TRUE)

write(file_2019_panel, "aha2019_panel.json")








