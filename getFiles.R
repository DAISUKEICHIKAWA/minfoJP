getFiles <- function(trglist, dir, wgetpath=NULL){
  #wget csv/xls files from MHLW to temporary directory 
  if(is.null(wgetpath)){
    stop("Please input wget-path ----> e.g. D:/wget-1.11.4-1-bin/bin")
    }else{
    lapply(trglist, function(x)shell(sprintf("cd %s & %s/wget --max-redirect=1 %s" ,dir , wgetpath, x)))
    }
  }