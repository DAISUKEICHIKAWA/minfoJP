#API�ƃt�H�[���ƌ`�ԑf��͂ƃ��|�[�e�B���O���g��
#������́������̏Z�����t�H�[���ɓ����Ǝ����̏Z���t�߂̈�Ë@�ւ��o�͂���ă��|�[�g�ɂȂ�


library(RCurl)
library(bitops)
library(XML)

#�_�E�����[�h���Ă�����Ë@�փf�[�^��ǂݍ���(�����data1.tsv��100���̂�)
setwd("D:/My Dropbox/017_loc/medloc/20110301")
d <- read.delim("data1.tsv", nrows=100,stringsAsFactors=FALSE)

#google�W�I�R�[�f�B���O�𗘗p���Ĉܓx�ƌo�x���擾����

locationdata <- d$�Z��

#�擾�֐��̒�`
getGIS <- function(locationdata){
 data <-  NULL
 for(count in 1:length(locationdata)){
�@�@  location <- locationdata[count]
�@�@�@�@  if(Sys.getlocale("LC_CTYPE")=="Japanese_Japan.932"){
�@�@�@�@  Encodelocation <-paste(c("",charToRaw(iconv(location,"CP932","UTF-8"))),collapse="%")
�@�@�@�@  }else{
�@�@�@�@  Encodelocation <-paste(c("",charToRaw(location)),collapse="%")
�@�@�@�@  }
�@�@  url <- paste("http://maps.google.com/maps/api/geocode/xml?address=",Encodelocation,"&sensor=false", sep="")
�@�@  xml <- getURL(url)
�@�@  lat <-as.numeric(xmlValue(xmlRoot(xmlTreeParse(xml))[["result"]][["geometry"]][["location"]][["lat"]]))
�@�@  lon <-as.numeric(xmlValue(xmlRoot(xmlTreeParse(xml))[["result"]][["geometry"]][["location"]][["lng"]]))
�@�@  data0 <- data.frame(lat=lat, lon=lon, stringsAsFactors=FALSE)
�@�@  data <- rbind(data, data0)
   }
 return(data)
 }

latlon <- getGIS(locationdata)

d2 <- cbind(d, latlon)
d2 <- d2[!is.na(d2$lat), ]

#�����̏Z������͂��遨�ܓx�o�x�ւ̕ϊ�

myaddress <- "���R�s��蒬1����1��1��"
mylatlon <- getGIS(myaddress)

#���͈͂ɓ����Ë@�ւ𒊏o���遨����͕b�F�ܓx30m�o�x25m�Ɖ��ɒu����500m�ȓ��i0.06��0.08�j
#�߂�����25�@�֒��o����
#myrangelat <- 0.015
#myrangelon <- 0.02
#d3 <- d2[d2$lat >= (mylatlon$lat-myrangelat) & d2$lon >= (mylatlon$lon-myrangelon) & d2$lat <= (mylatlon$lat+myrangelat) & d2$lon <= (mylatlon$lon+myrangelon),]
distance <- ((d2$lat - mylatlon$lat)^2 + (d2$lon - mylatlon$lon)^2)^(1/2)
d3 <- d2[order(distance), ][1:25,]


###��Ë@�ւ�\������i��ȁA���ȁA��ǁj����Ë@�փ��X�g�{�n�}�iRgoogleMaps�Ŏ����̏Z���𒆐S�Ɂj

#google static map API�p�Ƀ}�[�J�[���쐬�i�����̏Z���̓}�[�J�[�̐F���ɂȂ�悤�Ɏw��j

library(RgoogleMaps)
mymarkers <- ""
 for(i in 1:nrow(d3)){
 loc <- paste(d3[i,"lat"],d3[i,"lon"], sep=",")
 lab <- paste("label:", LETTERS[i], sep="")
 m1 <- paste(lab, loc, sep="|")
 mymarkers <- paste(mymarkers, m1, sep="&markers=")
 }
 myloc <- paste(mylatlon, collapse=",")
 myloc <- paste("&markers=color:blue|", myloc, sep="")
 mymarkers <- paste(myloc, mymarkers, sep="")
m<-plotGoogleMaps(d3,filename='myMap.htm')
GetMap(center = mylatlon, zoom = 14, markers = mymarkers, destfile = "Mymap.png") #���b�Z�[�W���ł邪�C�ɂ���y������

###�}�b�v�ƏZ���𐮌`���ă��|�[�g��HTML�ŏo��

output <- d3[, c(6, 7, 8)] #�{�ݖ��A�X�֔ԍ��A�Z�����o�͗p�ɒ��o
rownames(output) <- LETTERS[1:nrow(d3)] #�}�[�J�[�̃A���t�@�x�b�g�ɍ��킹��
#�{�ݖ����N�G���ɂ���Yahoo�u���O�������g���ċɐ��]����������]�����Z�o
#�W�ԉȂ����Đԕ������ŋ�������



library(hwriter)
p <- openPage("report.html", charset="shift-jis") #��Ë@�փf�[�^�̕����R�[�h��shift-jis�ł��邽��
img <- hwriteImage('Mymap.png', br=FALSE, width=500)
doc <- hwrite(output)
hwrite(c(img, doc), p, border=0)

hwrite('<iframe src="GeoMapID3f302206.html" frameborder="0" width="470" height="330"></iframe>',p)

closePage(p)

#�V�F���X�N���v�g����u���E�U�N���������ďo�͂��m�F�iWindows����PATH���ʂ��Ă��邱�Ƃ��K�v�j
shell("report.html")

#
**hwrite�֐��ɂ���
+��{�`��hwrite(�I�u�W�F�N�g, �o�͐�t�@�C��)
+�����̃I�u�W�F�N�g���o�͂���ꍇ��c()�Ō������Ďw�肷��ƕ\�`���ŏo�͂����
+�o�͐���w�肵�Ȃ��ƃ^�O���o�͂����

+�����N��link="hoge.html"�Ƃ����`�Ŏw�肷��
+dim=c(2,1)�Ƃ����`�Ń��C�A�E�g�͎w��
+center=TRUE�Œ����z�u
+heading=������hr�^�O�ň͂܂��
+style�ŕ������蓙CSS style�͎w��
+br=TRUE�ŉ��s(br�^�O�ň͂܂��)
+div=TRUE�Ƃ��邱�Ƃ�div�^�O�ň͂܂��,class���w��\
+hwrite()�̒��œK����hoge=�Ȃ�Ƃ��Ǝw�肷��ƃ^�O�Ƃ��Ă��̂܂܏o�͂����̂Ŋ֐��͂����Ŏw�肷��
**���̑��̊֐��ɂ���
+hmakeTag("hoge", �I�u�W�F�N�g)�Ƃ��邱�ƂŃI�u�W�F�N�g��hoge�^�O�ň͂�html���o�͂����
+openPage(link.css = "hoge.css")��css�t�@�C�����w��ł���



#���i�K��HTML�Ńf�[�^���|�[�g�i�Œ�����`�����`�ŕ\������j
#���i�K���C���^���N�e�B�u����݂��遨�}�E�X�I�[�o�[�Ő��l���\������遨googlemap�łǂ��܂łł��邩

#googleVis�𗘗p���遨GeoMap�ō��R�[�h��JP

df <- data.frame(Postcode=c("EC3M 7HA", "EC2P 2EJ"),
                 Tip=c("Lloyd's", "Guildhall"))
 
M2 <- gvisMap(df, "Postcode", "Tip",
              options=list(showTip=TRUE, mapType='normal',
              enableScrollWheel=TRUE))

plot(M2) 

#googleVis��p���Ēn�}��h�蕪����

#�f�[�^�̓ǂݍ���
data <- read.csv("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/003_other/list110425.csv", as.is = TRUE)
data$�ΏێҐ�st <- cut(data$�ΏێҐ�, right=FALSE, breaks=c(1, 10, 50, 100, 500, Inf), labels=c("1-9", "10-49", "50-99", "100-499", "500-"))

sc_type <- data.frame(stringsAsFactors=FALSE, type=toupper(letters[1:11]), name=c("��w", "�Z��", "���Z", "���w�Z", "���w�Z", "�c�t��", "�{��w�Z��", "���̑��w�Z", "�{��", "����", "���w�Z"))
data <- merge(data, sc_type, all.x=TRUE)

#���ʃv���b�g
data_summary <- ddply(data, .(pref), summarise, sum_jushin=sum(��f�Ґ�), sum_taishou=sum(�ΏێҐ�))
jushinritsu <- 100 * round(data_summary$sum_jushin / data_summary$sum_taishou, 3)
pref <- read.csv("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/003_other/provincecodes.csv")
pref <- pref[pref$CountryCode=="JP",]
pref$jushinritsu <- jushinritsu

G2 <- gvisGeoMap(pref, locationvar='Province', numvar='jushinritsu',
                 options=list(region='JP', height=350, 
                              dataMode='regions',
                              colors='[0xFF8747, 0xFFB581, 0xc06000]'))	
?gvisGeoMap
plot(G2)

#plotGoogleMaps��p���ăv���b�g���遨sp�I�u�W�F�N�g�ւ̕ϊ������܂������Ȃ����Aiframe�Ŗ��ߍ��߂�̂ł�����̕����ǂ�
library(plotGoogleMaps)
data(meuse)
coordinates(meuse)<-~x+y
proj4string(meuse) <- CRS('+init=epsg:28992')
m<-plotGoogleMaps(meuse,filename='myMap.htm')
shell("myMap.html")

coordinates(d3)<-~lat+lon
proj4string(d3) <- CRS('+proj=longlat +datum=WGS84')