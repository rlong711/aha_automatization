

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

day1_paper_2015 <- c("https://aha.confex.com/aha/2015/webprogram/Session12563.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12200.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11550.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11700.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12084.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12159.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11968.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12204.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12189.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11594.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12138.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11074.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11930.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12080.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12197.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11415.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11853.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11238.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12196.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11973.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11553.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11307.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12298.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12299.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12303.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12355.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12356.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12406.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11979.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12253.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11395.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11984.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12095.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12436.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12442.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12457.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11646.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12292.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11366.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11526.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12505.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12511.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12188.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12559.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11759.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11564.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12221.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12109.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11337.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11474.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11328.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11908.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11540.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11737.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12209.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12193.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12215.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11645.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11562.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11329.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12003.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12202.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12186.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12153.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12075.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11920.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11304.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12308.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12310.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12333.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12001.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12359.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12362.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12398.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11657.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11219.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12247.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11909.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11123.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11615.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12435.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12452.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11173.html")


day1_panel_2015 <- c("https://aha.confex.com/aha/2015/webprogram/Session12494.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11835.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12551.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11838.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12480.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12198.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12191.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11664.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12177.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12469.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12161.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11940.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12472.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12530.html")


day2_paper_2015 <- c("https://aha.confex.com/aha/2015/webprogram/Session12291.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11980.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11761.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11086.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11518.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12195.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12015.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12120.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11663.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11718.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11650.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12013.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12042.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11326.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11286.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11224.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11974.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11320.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11189.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12184.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11778.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11293.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11178.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11862.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11373.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12441.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11677.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12312.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12313.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12365.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12366.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12367.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12477.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11300.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11907.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11522.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12162.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11945.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12079.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11983.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12407.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12275.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11816.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12290.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11637.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12553.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12025.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session10879.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11764.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11076.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12216.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session10904.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11588.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12089.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11104.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11970.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11758.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12006.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11205.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11926.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12132.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12038.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11867.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12160.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11214.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11516.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12011.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11374.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12088.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12314.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12315.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12316.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12411.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12395.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11927.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12478.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11351.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12249.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12112.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11524.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12058.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11520.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12430.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11015.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12277.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12331.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12440.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11978.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12522.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12447.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12353.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12222.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12558.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11887.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11429.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12173.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11537.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11476.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12155.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11913.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11765.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11652.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11045.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11919.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session10877.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11998.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11965.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12016.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12175.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12219.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11312.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11442.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12467.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12508.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12318.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12319.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12326.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12378.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12383.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11934.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12479.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12251.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12029.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12550.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11402.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11376.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11644.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12437.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12338.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12339.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11041.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12337.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11565.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11918.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12297.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12301.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12305.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12311.html")


day2_panel_2015 <- c("https://aha.confex.com/aha/2015/webprogram/Session12566.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12501.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12485.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12499.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12028.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11098.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11762.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11408.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12573.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12486.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12498.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12497.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12192.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12211.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12214.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11116.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12371.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12451.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12390.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12451.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12390.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12352.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12481.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12500.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12513.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12492.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session10952.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11922.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11139.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12223.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12201.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11291.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12380.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12496.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12636.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12568.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12471.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12416.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12140.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12304.html")




day3_paper_2015 <- c("https://aha.confex.com/aha/2015/webprogram/Session12564.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12203.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11902.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12127.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11679.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12018.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11861.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12205.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12026.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11917.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11458.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11975.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11972.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11268.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12119.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12217.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11325.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11297.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11195.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11200.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11375.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12322.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12323.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12324.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12373.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12385.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12396.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12252.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11883.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11546.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11676.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11825.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12474.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12187.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11849.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11997.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11463.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11826.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11153.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11128.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11592.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12199.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12220.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11814.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12165.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11603.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11352.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11210.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12139.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12012.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11831.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11421.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11590.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11504.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11198.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12320.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12325.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12391.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12361.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12399.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12607.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11011.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11488.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11842.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11301.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11531.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12059.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12433.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12340.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12357.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12446.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12556.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12426.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11818.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12121.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11933.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12207.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11865.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12218.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11303.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12158.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12212.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11950.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12090.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11511.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11898.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11991.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12087.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12064.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12190.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12009.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11754.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11199.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11627.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11805.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12327.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12328.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12330.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12400.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12401.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12281.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11801.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11987.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12115.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12360.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session10967.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12453.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12557.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12341.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12394.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12410.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12243.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12404.html")



day3_panel_2015 <- c("https://aha.confex.com/aha/2015/webprogram/Session12483.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12122.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12108.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12510.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11209.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12172.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11678.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12560.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12476.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11800.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12343.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12484.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12129.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12225.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12502.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12163.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12185.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12224.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11959.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12470.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12482.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12507.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12493.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11216.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12571.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12208.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11470.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11638.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12344.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12386.html")





day4_paper_2015 <- c("https://aha.confex.com/aha/2015/webprogram/Session11551.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12565.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11398.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11549.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12005.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11931.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11843.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11728.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11179.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11350.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12111.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12194.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12100.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12043.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12213.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12069.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11731.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12141.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11966.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12024.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12130.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11901.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11630.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11850.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12332.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12334.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12408.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12372.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11760.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11682.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11499.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11501.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12439.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12473.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11948.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11582.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11854.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11692.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11607.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12022.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11732.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11570.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12124.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11916.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11576.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11851.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11661.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12040.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12170.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12083.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11660.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12147.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12176.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11629.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11855.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11723.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12335.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12336.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12414.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12415.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12091.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11220.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12061.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11746.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12438.html")




day4_panel_2015 <- c("https://aha.confex.com/aha/2015/webprogram/Session12504.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12609.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11435.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12210.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session10971.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11829.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12412.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12512.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11828.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12067.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12206.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session11647.html",
                     "https://aha.confex.com/aha/2015/webprogram/Session12413.html")



## Combing Links

paper_2015 <- c(day1_paper_2015, day2_paper_2015, day3_paper_2015, day4_paper_2015)

panel_2015 <- c(day1_panel_2015, day2_panel_2015, day3_panel_2015, day4_panel_2015)

links_2015 <- c(paper_2015, panel_2015)

## Mapping Functions

panelsessionTitles_20 <- purrr::map(panel_links_20, session_titles)

papersessionTitles_20 <- purrr::map(paper_links_20, session_titles)

paperTitles_20 <- purrr::map(paper_links_20, paper_titles)

institutionsPaper_20 <- purrr::map(paper_links_20, instituitions_paper)

institutionPanel_20 <- purrr::map(panel_links_20, institutions_panel)

## List

aha_20_paper <- list(session_titles = papersessionTitles_2022,
                     paper_titles = paperTitles_20,
                     paper_institutions = institutionsPaper_20)


aha_20_panel <- list(session_titles = panelsessionTitles_20,
                     panel_institutions = institutionPanel_20)

## JSON Files

library(jsonlite)

file_20_paper = toJSON(aha_20_paper, pretty = TRUE, auto_unbox = TRUE)

write(file_20_paper, "aha20_paper.json")

file_20_panel = toJSON(aha_20_panel, pretty = TRUE, auto_unbox = TRUE)

write(file_20_panel, "aha20_panel.json")








