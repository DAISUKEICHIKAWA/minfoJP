library(XLConnect)

getSeimei <- function(file){
  require(XLConnect)
  wb  <- loadWorkbook(file)
  ws  <- readWorksheet(wb, sheet=1, header=FALSE, startRow=15, startCol=2, endRow=141, endCol=14)
  res <- ws[,c(1, 3, 5, 7, 9, 11, 13)]
  res <- subset(res, complete.cases(res))
  colnames(res) <- c("x", "nqx", "lx", "ndx", "nLx", "Tx", "ex")
  invisible(res)
  }

library(ggplot2)
#from http://www.mhlw.go.jp/toukei/list/55-18.html

file1  <- "C:/Documents and Settings/Issei/My Documents/Dropbox/iAnalysis/public data/seimeihyo/data/seimei22otoko.xls"
file2  <- "C:/Documents and Settings/Issei/My Documents/Dropbox/iAnalysis/public data/seimeihyo/data/seimei22onna.xls"
male   <- data.frame(getSeimei(file1), sex="’j«")
female <- data.frame(getSeimei(file2), sex="—«")
data   <- rbind(male, female)

ggplot(data, aes(x=x, y=ex, group=sex)) + geom_line(aes(color=sex)) + geom_point(aes(color=sex))



#------Ž€–S—¦
#---‘S”N—î
ggplot(data, aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex)) + geom_point(aes(color=sex))

#---›Î–¢–ž
ggplot(data[data$x < 20, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="20Î–¢–ž") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("”N—î") + ylab("Ž€–S—¦")
ggplot(data[data$x < 30, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="30Î–¢–ž") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("”N—î") + ylab("Ž€–S—¦")
ggplot(data[data$x < 40, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="40Î–¢–ž") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("”N—î") + ylab("Ž€–S—¦")
ggplot(data[data$x < 50, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="50Î–¢–ž") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("”N—î") + ylab("Ž€–S—¦")
ggplot(data[data$x < 60, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="60Î–¢–ž") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("”N—î") + ylab("Ž€–S—¦")
ggplot(data[data$x < 70, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="70Î–¢–ž") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("”N—î") + ylab("Ž€–S—¦")
ggplot(data[data$x < 80, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="80Î–¢–ž") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("”N—î") + ylab("Ž€–S—¦")
ggplot(data[data$x < 90, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="90Î–¢–ž") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("”N—î") + ylab("Ž€–S—¦")
ggplot(data, aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="‘S‘Ì") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("”N—î") + ylab("Ž€–S—¦")


#------¶‘¶”
#---‘S”N—î
ggplot(data, aes(x=x, y=lx, group=sex)) + geom_step(aes(color=sex), cex=1.5) + 
	scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("”N—î") + ylab("¶‘¶”ilj") + 
	scale_y_continuous(breaks = seq(2e+4, 1e+5, by=2e+4),
	labels = c("20,000", "40,000", "60,000", "80,000", "100,000"))



#------ŒvŽZƒ`ƒFƒbƒN
names(data)

#---Ž€–S—¦
data$nqx1 <- data$ndx / data$lx

#---¶‘¶”
data$lx1 <- data$lx - data$ndx

#---•½‹Ï—]–½
data$ex1 <- data$Tx / data$lx


