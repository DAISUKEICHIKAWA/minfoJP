#APIとフォームと形態素解析とレポーティングを使う
#つくるもの→自分の住所をフォームに入れると自分の住所付近の医療機関が出力されてレポートになる


library(RCurl)
library(bitops)
library(XML)

#ダウンロードしてきた医療機関データを読み込む(今回はdata1.tsvの100件のみ)
setwd("D:/My Dropbox/017_loc/medloc/20110301")
d <- read.delim("data1.tsv", nrows=100,stringsAsFactors=FALSE)

#googleジオコーディングを利用して緯度と経度を取得する

locationdata <- d$住所

#取得関数の定義
getGIS <- function(locationdata){
 data <-  NULL
 for(count in 1:length(locationdata)){
　　  location <- locationdata[count]
　　　　  if(Sys.getlocale("LC_CTYPE")=="Japanese_Japan.932"){
　　　　  Encodelocation <-paste(c("",charToRaw(iconv(location,"CP932","UTF-8"))),collapse="%")
　　　　  }else{
　　　　  Encodelocation <-paste(c("",charToRaw(location)),collapse="%")
　　　　  }
　　  url <- paste("http://maps.google.com/maps/api/geocode/xml?address=",Encodelocation,"&sensor=false", sep="")
　　  xml <- getURL(url)
　　  lat <-as.numeric(xmlValue(xmlRoot(xmlTreeParse(xml))[["result"]][["geometry"]][["location"]][["lat"]]))
　　  lon <-as.numeric(xmlValue(xmlRoot(xmlTreeParse(xml))[["result"]][["geometry"]][["location"]][["lng"]]))
　　  data0 <- data.frame(lat=lat, lon=lon, stringsAsFactors=FALSE)
　　  data <- rbind(data, data0)
   }
 return(data)
 }

latlon <- getGIS(locationdata)

d2 <- cbind(d, latlon)
d2 <- d2[!is.na(d2$lat), ]

#自分の住所を入力する→緯度経度への変換

myaddress <- "松山市大手町1丁目1番1号"
mylatlon <- getGIS(myaddress)

#一定範囲に入る医療機関を抽出する→今回は秒：緯度30m経度25mと仮に置いて500m以内（0.06と0.08）
#近い順に25機関抽出する
#myrangelat <- 0.015
#myrangelon <- 0.02
#d3 <- d2[d2$lat >= (mylatlon$lat-myrangelat) & d2$lon >= (mylatlon$lon-myrangelon) & d2$lat <= (mylatlon$lat+myrangelat) & d2$lon <= (mylatlon$lon+myrangelon),]
distance <- ((d2$lat - mylatlon$lat)^2 + (d2$lon - mylatlon$lon)^2)^(1/2)
d3 <- d2[order(distance), ][1:25,]


###医療機関を表示する（医科、歯科、薬局）→医療機関リスト＋地図（RgoogleMapsで自分の住所を中心に）

#google static map API用にマーカーを作成（自分の住所はマーカーの色が青になるように指定）

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
GetMap(center = mylatlon, zoom = 14, markers = mymarkers, destfile = "Mymap.png") #メッセージがでるが気にせずyを押す

###マップと住所を整形してレポートをHTMLで出力

output <- d3[, c(6, 7, 8)] #施設名、郵便番号、住所を出力用に抽出
rownames(output) <- LETTERS[1:nrow(d3)] #マーカーのアルファベットに合わせる
#施設名をクエリにしてYahooブログ検索を使って極性評価辞書から評判を算出
#標榜科を入れて赤文字等で強調する



library(hwriter)
p <- openPage("report.html", charset="shift-jis") #医療機関データの文字コードがshift-jisであるため
img <- hwriteImage('Mymap.png', br=FALSE, width=500)
doc <- hwrite(output)
hwrite(c(img, doc), p, border=0)

hwrite('<iframe src="GeoMapID3f302206.html" frameborder="0" width="470" height="330"></iframe>',p)

closePage(p)

#シェルスクリプトからブラウザ起動をさせて出力を確認（Windows環境＆PATHが通っていることが必要）
shell("report.html")

#
**hwrite関数について
+基本形はhwrite(オブジェクト, 出力先ファイル)
+複数のオブジェクトを出力する場合はc()で結合して指定すると表形式で出力される
+出力先を指定しないとタグが出力される

+リンクはlink="hoge.html"という形で指定する
+dim=c(2,1)という形でレイアウトは指定
+center=TRUEで中央配置
+heading=数字でhrタグで囲まれる
+styleで文字飾り等CSS styleは指定
+br=TRUEで改行(brタグで囲まれる)
+div=TRUEとすることでdivタグで囲まれる,classも指定可能
+hwrite()の中で適当にhoge=なんとかと指定するとタグとしてそのまま出力されるので関数はここで指定する
**その他の関数について
+hmakeTag("hoge", オブジェクト)とすることでオブジェクトをhogeタグで囲んだhtmlが出力される
+openPage(link.css = "hoge.css")でcssファイルを指定できる



#第一段階→HTMLでデータレポート（最低限整形した形で表示する）
#第二段階→インタラクティブ性を設ける→マウスオーバーで数値が表示される→googlemapでどこまでできるか

#googleVisを利用する→GeoMapで国コードはJP

df <- data.frame(Postcode=c("EC3M 7HA", "EC2P 2EJ"),
                 Tip=c("Lloyd's", "Guildhall"))
 
M2 <- gvisMap(df, "Postcode", "Tip",
              options=list(showTip=TRUE, mapType='normal',
              enableScrollWheel=TRUE))

plot(M2) 

#googleVisを用いて地図を塗り分ける

#データの読み込み
data <- read.csv("Q:/Pln/Analysis/01_hokensya/03_kyousai/03_shigaku/2011/003_other/list110425.csv", as.is = TRUE)
data$対象者数st <- cut(data$対象者数, right=FALSE, breaks=c(1, 10, 50, 100, 500, Inf), labels=c("1-9", "10-49", "50-99", "100-499", "500-"))

sc_type <- data.frame(stringsAsFactors=FALSE, type=toupper(letters[1:11]), name=c("大学", "短大", "高校", "中学校", "小学校", "幼稚園", "養護学校等", "その他学校", "施設", "高専", "専門学校"))
data <- merge(data, sc_type, all.x=TRUE)

#県別プロット
data_summary <- ddply(data, .(pref), summarise, sum_jushin=sum(受診者数), sum_taishou=sum(対象者数))
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

#plotGoogleMapsを用いてプロットする→spオブジェクトへの変換がうまくいかないが、iframeで埋め込めるのでこちらの方が良い
library(plotGoogleMaps)
data(meuse)
coordinates(meuse)<-~x+y
proj4string(meuse) <- CRS('+init=epsg:28992')
m<-plotGoogleMaps(meuse,filename='myMap.htm')
shell("myMap.html")

coordinates(d3)<-~lat+lon
proj4string(d3) <- CRS('+proj=longlat +datum=WGS84')