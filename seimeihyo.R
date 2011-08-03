getSeimei <- function(file){
  require(XLConnect)
  wb <- loadWorkbook(file)
  ws <- readWorksheet(wb, sheet=1, header=FALSE, startRow=15, startCol=2, endRow=141, endCol=14)
  res <- ws[,c(1, 3, 5, 7, 9, 11, 13)]
  res <- subset(res, complete.cases(res))
  colnames(res) <- c("x", "nqx", "lx", "ndx", "nLx", "Tx", "ex")
  invisible(res)
  }

library(ggplot2)
#from http://www.mhlw.go.jp/toukei/list/55-18.html

file1 <- "D:/downloads/seimei21otoko.xls"
file2 <- "D:/downloads/seimei22onna.xls"
male <- data.frame(getSeimei(file1), sex="male")
female <- data.frame(getSeimei(file2), sex="female")
data <- rbind(male, female)

ggplot(data, aes(x=x, y=ex, group=sex)) + geom_line(aes(color=sex)) + geom_point(aes(color=sex))