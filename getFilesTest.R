getFiles <- function(trglist, dir=tempdir()){
  #wget csv/xls files from MHLW to temporary directory 

  tmpd <- dir #download directory
  
  wg <- "D:/wget-1.11.4-1-bin/bin/wget" #wget path

  lapply(trglist, function(x)shell(sprintf("cd %s & %s -r %s" ,tmpd, wg, x)))
  }


#test1