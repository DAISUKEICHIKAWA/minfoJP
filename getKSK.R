#get Information from http://www.e-stat.go.jp/SG1/estat/NewList.do?tid=000001031016
#kokumin seikatsu kiso chousa H21

library(stringr)
#target files path
trg <- list("http://www.e-stat.go.jp/SG1/estat/Csvdl.do?sinfid=000007741219",
            "http://www.e-stat.go.jp/SG1/estat/Csvdl.do?sinfid=000007741220",
            "http://www.e-stat.go.jp/SG1/estat/Csvdl.do?sinfid=000007741309")
trg <- as.list(paste(sep="", "http://www.e-stat.go.jp/SG1/estat/Csvdl.do?sinfid=000007741", 219:309))
getFiles(trg, getwd())