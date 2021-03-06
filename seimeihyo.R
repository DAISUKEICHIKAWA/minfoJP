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
male   <- data.frame(getSeimei(file1), sex="男性")
female <- data.frame(getSeimei(file2), sex="女性")
data   <- rbind(male, female)

ggplot(data, aes(x=x, y=ex, group=sex)) + geom_line(aes(color=sex)) + geom_point(aes(color=sex))



pdf("C:/Documents and Settings/Issei/My Documents/Dropbox/iAnalysis/public data/seimeihyo/results/seimeihyo1.pdf", family="Japan1GothicBBB", height=5)
  #------死亡率
  #---○歳未満
  ggplot(data[data$x < 20, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="生命表による死亡率の変化（20歳未満）") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("年齢") + ylab("死亡率")
  ggplot(data[data$x < 30, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="生命表による死亡率の変化（30歳未満）") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("年齢") + ylab("死亡率")
  ggplot(data[data$x < 40, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="生命表による死亡率の変化（40歳未満）") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("年齢") + ylab("死亡率")
  ggplot(data[data$x < 50, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="生命表による死亡率の変化（50歳未満）") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("年齢") + ylab("死亡率")
  ggplot(data[data$x < 60, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="生命表による死亡率の変化（60歳未満）") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("年齢") + ylab("死亡率")
  ggplot(data[data$x < 70, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="生命表による死亡率の変化（70歳未満）") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("年齢") + ylab("死亡率")
  ggplot(data[data$x < 80, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="生命表による死亡率の変化（80歳未満）") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("年齢") + ylab("死亡率")
  ggplot(data[data$x < 90, ], aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="生命表による死亡率の変化（90歳未満）") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("年齢") + ylab("死亡率")
  ggplot(data, aes(x=x, y=nqx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="全年齢") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("年齢") + ylab("死亡率")
  
  
  #------生存数
  #---全年齢
  ggplot(data, aes(x=x, y=lx, group=sex)) + geom_step(aes(color=sex), cex=1.5) + opts(title="生命表による生存数の変化") + 
  	scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("年齢") + ylab("生存数（人）") + 
  	scale_y_continuous(breaks = seq(2e+4, 1e+5, by=2e+4),
  	labels = c("20,000", "40,000", "60,000", "80,000", "100,000"))
  
  #------死亡数
  #---全年齢
  ggplot(data, aes(x=x, y=ndx, group=sex)) + geom_line(aes(color=sex), size=1.5, alpha=0.7) + geom_point(aes(color=sex), cex=3, alpha=0.7) + opts(title="生命表による死亡数の変化") + scale_colour_manual(values = c("#4600FF", "#FF00BF")) + xlab("年齢") + ylab("死亡数（人）")
dev.off()


#------計算チェック
names(data)

#---死亡率
data$nqx1 <- data$ndx / data$lx

#---生存数
data$lx1 <- data$lx - data$ndx

#---平均余命
data$ex1 <- data$Tx / data$lx

#---全死亡数
sum(data$ndx)



