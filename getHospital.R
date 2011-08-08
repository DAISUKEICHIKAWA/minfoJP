#wgetpathは "D:/wget-1.11.4-1-bin/bin/"のような形でwgetの入っているディレクトリを指定する
#とりあえず2011年1月以降のデータについては動作確認済

getHospital <- function(year, month, wgetpath){
  require(XLConnect)
  require(ggplot2)
  require(stringr)
  tmpdir <- tempdir()
  month <- ifelse(year<2011, formatC(width=2, flag="0", month), month)
  year <- substr(year, start=3, stop=4)
  fname <- sprintf("http://www.mhlw.go.jp/toukei/saikin/hw/byouin/m%s/xls/%s.xls", year, month)
  getFiles(list(fname), dir=tmpdir, wgetpath=wgetpath)
  
  wb <- loadWorkbook(str_c(tmpdir, "/", month, ".xls"))
  sheetlist <- getSheets(wb)
  
  result <- as.list(rep(NA, length(sheetlist)-3))

  res <- readWorksheet(wb, sheet="ﾃﾞｰﾀ", startRow=6, startCol=2, endRow=-1, endCol=7, header=FALSE)
  res <- subset(res[, -5], complete.cases(res[,-5]))
  ynum <- c(grep(".+\\n", res[,1]), Inf) -1 #年の区切りベクトル
  ylab <- gsub(".+\\n", "", res[grep(".+\\n", res[,1]), 1]) #年のラベル
  year <- cut(1:nrow(res), breaks=ynum, labels=ylab) #区切りベクトルに従って年ラベルを生成
  res[,1] <- as.integer(gsub("\\n.+", "", res[,1])) #月ベクトルに変換
  res <- apply(res, 2, as.numeric)
  res <- data.frame(year, res)
  colnames(res) <- c("year", "month", "1日平均在院患者", "1日平均外来患者", "平均在院日数", "病床利用率")

  result[[1]] <- res

  res <- readWorksheet(wb, sheet="参考表１", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("1日平均在院患者数（病院）", c("総数", "精神病床", "結核病床", "療養病床", "一般病床", "介護療養病床")))

  result[[2]] <- res
  
  res <- readWorksheet(wb, sheet="参考表２", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", "1日平均外来患者数（病院）総数", "1日平均外来患者数（病院）精神科病院", "1日平均外来患者数（病院）一般病院", "外来患者延数（病院）総数", "外来患者延数（病院）精神科病院", "外来患者延数（病院）一般病院")

  result[[3]] <- res

  res <- readWorksheet(wb, sheet="参考表３", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("月末病床利用率（病院）", c("総数", "精神病床", "結核病床", "療養病床", "一般病床", "介護療養病床")))

  result[[4]] <- res

  res <- readWorksheet(wb, sheet="参考表４", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("平均在院日数（病院）", c("総数", "精神病床", "結核病床", "療養病床", "一般病床", "介護療養病床")))

  result[[5]] <- res

  res <- readWorksheet(wb, sheet="参考表５", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("在院患者延数（病院）", c("総数", "精神病床", "結核病床", "療養病床", "一般病床", "介護療養病床")))

  result[[6]] <- res

  res <- readWorksheet(wb, sheet="参考表６", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("月末在院患者数（病院）", c("総数", "精神病床", "結核病床", "療養病床", "一般病床", "介護療養病床")))

  result[[7]] <- res

  res <- readWorksheet(wb, sheet="参考表７", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("病床数（病院）", c("総数", "精神病床", "結核病床", "療養病床", "一般病床", "介護療養病床")))

  result[[8]] <- res

  res <- readWorksheet(wb, sheet="参考表８", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("新入院患者数（病院）", c("総数", "精神病床", "結核病床", "療養病床", "一般病床", "介護療養病床")))

  result[[9]] <- res

  res <- readWorksheet(wb, sheet="参考表９", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("退院患者数（病院）", c("総数", "精神病床", "結核病床", "療養病床", "一般病床", "介護療養病床")))

  result[[10]] <- res

  res <- readWorksheet(wb, sheet="参考表１０", startRow=8, startCol=2, endRow=-1, endCol=10, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("病院の療養病床", c("在院患者延数", "月末在院患者数", "新入院患者数", "他の病床から移された患者数", "退院患者数", "他の病床へ移された患者数", "病床数")))

  result[[11]] <- res

  res <- readWorksheet(wb, sheet="参考表１１", startRow=8, startCol=2, endRow=-1, endCol=10, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("病院の介護療養病床", c("在院患者延数", "月末在院患者数", "新入院患者数", "他の病床から移された患者数", "退院患者数", "他の病床へ移された患者数", "病床数")))

  result[[12]] <- res


  res <- readWorksheet(wb, sheet="参考表１２", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", c(str_c("病院の介護療養病床", c("療養病床", "介護療養病床")), str_c("病床利用率（診療所）", c("療養病床", "介護療養病床")), str_c("平均在院日数（診療所）", c("療養病床", "介護療養病床"))))

  result[[13]] <- res

  res <- readWorksheet(wb, sheet="参考表１３", startRow=8, startCol=2, endRow=-1, endCol=10, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("診療所の療養病床", c("在院患者延数", "月末在院患者数", "新入院患者数", "他の病床から移された患者数", "退院患者数", "他の病床へ移された患者数", "病床数")))

  result[[14]] <- res

  res <- readWorksheet(wb, sheet="参考表１４", startRow=8, startCol=2, endRow=-1, endCol=10, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("診療所の介護療養病床", c("在院患者延数", "月末在院患者数", "新入院患者数", "他の病床から移された患者数", "退院患者数", "他の病床へ移された患者数", "病床数")))

  result[[15]] <- res

  names(result) <- c("data",str_c("t", 1:14))
  invisible(result)
  }