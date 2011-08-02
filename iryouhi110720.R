library(XLConnect)
library(stringr)
library(ggplot2)

source("Q:/Pln/Analysis/999_code/iryouhifunc110727.R")

##############################����

setwd("D:/downloads")

#wget�Ń_�E�����[�h
shell(sprintf("cd %s & D:/wget-1.11.4-1-bin/bin/wget -r http://www.mhlw.go.jp/bunya/iryouhoken/iryouhoken03/xls/2005a.xls" ,getwd()))
shell(sprintf("cd %s & D:/wget-1.11.4-1-bin/bin/wget -r http://www.mhlw.go.jp/bunya/iryouhoken/iryouhoken03/xls/2005b.xls" ,getwd()))
shell(sprintf("cd %s & D:/wget-1.11.4-1-bin/bin/wget -r http://www.mhlw.go.jp/bunya/iryouhoken/iryouhoken03/xls/2005c.xls" ,getwd()))
shell(sprintf("cd %s & D:/wget-1.11.4-1-bin/bin/wget -r http://www.mhlw.go.jp/bunya/iryouhoken/iryouhoken03/xls/2010a.xls" ,getwd()))
shell(sprintf("cd %s & D:/wget-1.11.4-1-bin/bin/wget -r http://www.mhlw.go.jp/bunya/iryouhoken/iryouhoken03/xls/2010b.xls" ,getwd()))
shell(sprintf("cd %s & D:/wget-1.11.4-1-bin/bin/wget -r http://www.mhlw.go.jp/bunya/iryouhoken/iryouhoken03/xls/2010c.xls" ,getwd()))


#�R�[�h�\�����炩���ߗp�ӂ��Ă���
dseido <- read.csv("seido.csv")
dkikan <- read.csv("iryoukikan.csv")


#��Ô�E�����f�[�^

#���@�f�[�^
iryouhi_i <- conv_iryoukikan("D:/downloads/2005a.xls", "��Ô�i")
kensuu_i <- conv_iryoukikan("D:/downloads/2005a.xls", "����i")

iryouhi_i2 <- conv_iryoukikan("D:/downloads/2010a.xls", "��Ô�i")
kensuu_i2 <- conv_iryoukikan("D:/downloads/2010a.xls", "����i")

iryouhi_i <- merge(iryouhi_i, iryouhi_i2, by=c("code1","code2"))
kensuu_i <- merge(kensuu_i, kensuu_i2, by=c("code1","code2"))


#���@�O�f�[�^
iryouhi_o <- conv_iryoukikan("D:/downloads/2005a.xls", "��Ô�o")
kensuu_o <- conv_iryoukikan("D:/downloads/2005a.xls", "����o")

iryouhi_o2 <- conv_iryoukikan("D:/downloads/2010a.xls", "��Ô�o")
kensuu_o2 <- conv_iryoukikan("D:/downloads/2010a.xls", "����o")

iryouhi_o <- merge(iryouhi_o, iryouhi_o2, by=c("code1","code2"))
kensuu_o <- merge(kensuu_o, kensuu_o2, by=c("code1","code2"))


##############################����

#����Ô�
ir_raw_i <- calc_data(iryouhi_i, 0)
ir_raw_o <- calc_data(iryouhi_o, 0)
ir_raw_i$kubun <- "���@"
ir_raw_o$kubun <- "���@�O"
ir_raw <- rbind(ir_raw_i, ir_raw_o)

#������
k_raw_i <- calc_data(kensuu_i, 0)
k_raw_o <- calc_data(kensuu_o, 0)
k_raw_i$kubun <- "���@"
k_raw_o$kubun <- "���@�O"
k_raw <- rbind(k_raw_i, k_raw_o)

#1���������Ô�
irp_i <- per_iryouhi(iryouhi_i, kensuu_i, 0)
irp_o <- per_iryouhi(iryouhi_o, kensuu_o, 0)
irp_i$kubun <- "���@"
irp_o$kubun <- "���@�O"
irp <- rbind(irp_i, irp_o)


##############################�o��

pdf(file=str_c("��Ô�", format(Sys.Date(), "%y%m%d"), ".pdf"), family="Japan1GothicBBB",paper="a4r",width=15,height=10)
ggplot(data=ir_raw, aes(x=variable, y=value, group=kikan)) + geom_line(aes(color=kikan)) + facet_wrap(~kubun, scales="free") + opts(title="����Ô�")
ggplot(data=k_raw, aes(x=variable, y=value, group=kikan)) + geom_line(aes(color=kikan)) + facet_wrap(~kubun, scales="free") + opts(title="����")
ggplot(data=irp, aes(x=variable, y=value, group=kikan)) + geom_line(aes(color=kikan)) + facet_wrap(~kubun, scales="free") + opts(title="1���������Ô�")
dev.off()