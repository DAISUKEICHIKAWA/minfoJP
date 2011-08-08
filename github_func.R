github_func <- function(url){
  require(RCurl)
  eval(parse(text = getURL(url, ssl.verifypeer = FALSE)))
  }

#example
#u <- "https://raw.github.com/hcccowork/minfoJP/master/getHeikinyomei.R"
#your_function <- github_func(u)