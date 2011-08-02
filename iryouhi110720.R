library(XLConnect)
library(stringr)
library(ggplot2)

source("Q:/Pln/Analysis/999_code/iryouhifunc110727.R")

##############################入力

setwd("D:/downloads")

#wgetでダウンロード
shell(sprintf("cd %s & D:/wget-1.11.4-1-bin/bin/wget -r http://www.mhlw.go.jp/bunya/iryouhoken/iryouhoken03/xls/2005a.xls" ,getwd()))
shell(sprintf("cd %s & D:/wget-1.11.4-1-bin/bin/wget -r http://www.mhlw.go.jp/bunya/iryouhoken/iryouhoken03/xls/2005b.xls" ,getwd()))
shell(sprintf("cd %s & D:/wget-1.11.4-1-bin/bin/wget -r http://www.mhlw.go.jp/bunya/iryouhoken/iryouhoken03/xls/2005c.xls" ,getwd()))
shell(sprintf("cd %s & D:/wget-1.11.4-1-bin/bin/wget -r http://www.mhlw.go.jp/bunya/iryouhoken/iryouhoken03/xls/2010a.xls" ,getwd()))
shell(sprintf("cd %s & D:/wget-1.11.4-1-bin/bin/wget -r http://www.mhlw.go.jp/bunya/iryouhoken/iryouhoken03/xls/2010b.xls" ,getwd()))
shell(sprintf("cd %s & D:/wget-1.11.4-1-bin/bin/wget -r http://www.mhlw.go.jp/bunya/iryouhoken/iryouhoken03/xls/2010c.xls" ,getwd()))


#コード表→あらかじめ用意しておく
dseido <- read.csv("seido.csv")
dkikan <- read.csv("iryoukikan.csv")


#医療費・件数データ

#入院データ
iryouhi_i <- conv_iryoukikan("D:/downloads/2005a.xls", "医療費i")
kensuu_i <- conv_iryoukikan("D:/downloads/2005a.xls", "件数i")

iryouhi_i2 <- conv_iryoukikan("D:/downloads/2010a.xls", "医療費i")
kensuu_i2 <- conv_iryoukikan("D:/downloads/2010a.xls", "件数i")

iryouhi_i <- merge(iryouhi_i, iryouhi_i2, by=c("code1","code2"))
kensuu_i <- merge(kensuu_i, kensuu_i2, by=c("code1","code2"))


#入院外データ
iryouhi_o <- conv_iryoukikan("D:/downloads/2005a.xls", "医療費o")
kensuu_o <- conv_iryoukikan("D:/downloads/2005a.xls", "件数o")

iryouhi_o2 <- conv_iryoukikan("D:/downloads/2010a.xls", "医療費o")
kensuu_o2 <- conv_iryoukikan("D:/downloads/2010a.xls", "件数o")

iryouhi_o <- merge(iryouhi_o, iryouhi_o2, by=c("code1","code2"))
kensuu_o <- merge(kensuu_o, kensuu_o2, by=c("code1","code2"))


##############################処理

#総医療費
ir_raw_i <- calc_data(iryouhi_i, 0)
ir_raw_o <- calc_data(iryouhi_o, 0)
ir_raw_i$kubun <- "入院"
ir_raw_o$kubun <- "入院外"
ir_raw <- rbind(ir_raw_i, ir_raw_o)

#総件数
k_raw_i <- calc_data(kensuu_i, 0)
k_raw_o <- calc_data(kensuu_o, 0)
k_raw_i$kubun <- "入院"
k_raw_o$kubun <- "入院外"
k_raw <- rbind(k_raw_i, k_raw_o)

#1件当たり医療費
irp_i <- per_iryouhi(iryouhi_i, kensuu_i, 0)
irp_o <- per_iryouhi(iryouhi_o, kensuu_o, 0)
irp_i$kubun <- "入院"
irp_o$kubun <- "入院外"
irp <- rbind(irp_i, irp_o)


##############################出力

pdf(file=str_c("医療費", format(Sys.Date(), "%y%m%d"), ".pdf"), family="Japan1GothicBBB",paper="a4r",width=15,height=10)
ggplot(data=ir_raw, aes(x=variable, y=value, group=kikan)) + geom_line(aes(color=kikan)) + facet_wrap(~kubun, scales="free") + opts(title="総医療費")
ggplot(data=k_raw, aes(x=variable, y=value, group=kikan)) + geom_line(aes(color=kikan)) + facet_wrap(~kubun, scales="free") + opts(title="件数")
ggplot(data=irp, aes(x=variable, y=value, group=kikan)) + geom_line(aes(color=kikan)) + facet_wrap(~kubun, scales="free") + opts(title="1件あたり医療費")
dev.off()