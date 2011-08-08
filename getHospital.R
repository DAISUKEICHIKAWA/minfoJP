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

  res <- readWorksheet(wb, sheet="ÃÞ°À", startRow=6, startCol=2, endRow=-1, endCol=7, header=FALSE)
  res <- subset(res[, -5], complete.cases(res[,-5]))
  ynum <- c(grep(".+\\n", res[,1]), Inf) -1 #”N‚Ì‹æØ‚èƒxƒNƒgƒ‹
  ylab <- gsub(".+\\n", "", res[grep(".+\\n", res[,1]), 1]) #”N‚Ìƒ‰ƒxƒ‹
  year <- cut(1:nrow(res), breaks=yearnum, labels=ylab) #‹æØ‚èƒxƒNƒgƒ‹‚É]‚Á‚Ä”Nƒ‰ƒxƒ‹‚ð¶¬
  res[,1] <- as.integer(gsub("\\n.+", "", res[,1])) #ŒŽƒxƒNƒgƒ‹‚É•ÏŠ·
  res <- apply(res, 2, as.numeric)
  res <- data.frame(year, res)
  colnames(res) <- c("year", "month", "1“ú•½‹ÏÝ‰@Š³ŽÒ", "1“ú•½‹ÏŠO—ˆŠ³ŽÒ", "•½‹ÏÝ‰@“ú”", "•a°—˜—p—¦")

  result[[1]] <- res

  res <- readWorksheet(wb, sheet="ŽQl•\‚P", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("1“ú•½‹ÏÝ‰@Š³ŽÒ”i•a‰@j", c("‘”", "¸_•a°", "Œ‹Šj•a°", "—Ã—{•a°", "ˆê”Ê•a°", "‰îŒì—Ã—{•a°")))

  result[[2]] <- res
  
  res <- readWorksheet(wb, sheet="ŽQl•\‚Q", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", "1“ú•½‹ÏŠO—ˆŠ³ŽÒ”i•a‰@j‘”", "1“ú•½‹ÏŠO—ˆŠ³ŽÒ”i•a‰@j¸_‰È•a‰@", "1“ú•½‹ÏŠO—ˆŠ³ŽÒ”i•a‰@jˆê”Ê•a‰@", "ŠO—ˆŠ³ŽÒ‰„”i•a‰@j‘”", "ŠO—ˆŠ³ŽÒ‰„”i•a‰@j¸_‰È•a‰@", "ŠO—ˆŠ³ŽÒ‰„”i•a‰@jˆê”Ê•a‰@")

  result[[3]] <- res

  res <- readWorksheet(wb, sheet="ŽQl•\‚R", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("ŒŽ––•a°—˜—p—¦i•a‰@j", c("‘”", "¸_•a°", "Œ‹Šj•a°", "—Ã—{•a°", "ˆê”Ê•a°", "‰îŒì—Ã—{•a°")))

  result[[4]] <- res

  res <- readWorksheet(wb, sheet="ŽQl•\‚S", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("•½‹ÏÝ‰@“ú”i•a‰@j", c("‘”", "¸_•a°", "Œ‹Šj•a°", "—Ã—{•a°", "ˆê”Ê•a°", "‰îŒì—Ã—{•a°")))

  result[[5]] <- res

  res <- readWorksheet(wb, sheet="ŽQl•\‚T", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("Ý‰@Š³ŽÒ‰„”i•a‰@j", c("‘”", "¸_•a°", "Œ‹Šj•a°", "—Ã—{•a°", "ˆê”Ê•a°", "‰îŒì—Ã—{•a°")))

  result[[6]] <- res

  res <- readWorksheet(wb, sheet="ŽQl•\‚U", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("ŒŽ––Ý‰@Š³ŽÒ”i•a‰@j", c("‘”", "¸_•a°", "Œ‹Šj•a°", "—Ã—{•a°", "ˆê”Ê•a°", "‰îŒì—Ã—{•a°")))

  result[[7]] <- res

  res <- readWorksheet(wb, sheet="ŽQl•\‚V", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("•a°”i•a‰@j", c("‘”", "¸_•a°", "Œ‹Šj•a°", "—Ã—{•a°", "ˆê”Ê•a°", "‰îŒì—Ã—{•a°")))

  result[[8]] <- res

  res <- readWorksheet(wb, sheet="ŽQl•\‚W", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("V“ü‰@Š³ŽÒ”i•a‰@j", c("‘”", "¸_•a°", "Œ‹Šj•a°", "—Ã—{•a°", "ˆê”Ê•a°", "‰îŒì—Ã—{•a°")))

  result[[9]] <- res

  res <- readWorksheet(wb, sheet="ŽQl•\‚X", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("‘Þ‰@Š³ŽÒ”i•a‰@j", c("‘”", "¸_•a°", "Œ‹Šj•a°", "—Ã—{•a°", "ˆê”Ê•a°", "‰îŒì—Ã—{•a°")))

  result[[10]] <- res

  res <- readWorksheet(wb, sheet="ŽQl•\‚P‚O", startRow=8, startCol=2, endRow=-1, endCol=10, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("•a‰@‚Ì—Ã—{•a°", c("Ý‰@Š³ŽÒ‰„”", "ŒŽ––Ý‰@Š³ŽÒ”", "V“ü‰@Š³ŽÒ”", "‘¼‚Ì•a°‚©‚çˆÚ‚³‚ê‚½Š³ŽÒ”", "‘Þ‰@Š³ŽÒ”", "‘¼‚Ì•a°‚ÖˆÚ‚³‚ê‚½Š³ŽÒ”", "•a°”")))

  result[[11]] <- res

  res <- readWorksheet(wb, sheet="ŽQl•\‚P‚P", startRow=8, startCol=2, endRow=-1, endCol=10, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("•a‰@‚Ì‰îŒì—Ã—{•a°", c("Ý‰@Š³ŽÒ‰„”", "ŒŽ––Ý‰@Š³ŽÒ”", "V“ü‰@Š³ŽÒ”", "‘¼‚Ì•a°‚©‚çˆÚ‚³‚ê‚½Š³ŽÒ”", "‘Þ‰@Š³ŽÒ”", "‘¼‚Ì•a°‚ÖˆÚ‚³‚ê‚½Š³ŽÒ”", "•a°”")))

  result[[12]] <- res


  res <- readWorksheet(wb, sheet="ŽQl•\‚P‚Q", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", c(str_c("•a‰@‚Ì‰îŒì—Ã—{•a°", c("—Ã—{•a°", "‰îŒì—Ã—{•a°")), str_c("•a°—˜—p—¦if—ÃŠj", c("—Ã—{•a°", "‰îŒì—Ã—{•a°")), str_c("•½‹ÏÝ‰@“ú”if—ÃŠj", c("—Ã—{•a°", "‰îŒì—Ã—{•a°"))))

  result[[13]] <- res

  res <- readWorksheet(wb, sheet="ŽQl•\‚P‚R", startRow=8, startCol=2, endRow=-1, endCol=10, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("f—ÃŠ‚Ì—Ã—{•a°", c("Ý‰@Š³ŽÒ‰„”", "ŒŽ––Ý‰@Š³ŽÒ”", "V“ü‰@Š³ŽÒ”", "‘¼‚Ì•a°‚©‚çˆÚ‚³‚ê‚½Š³ŽÒ”", "‘Þ‰@Š³ŽÒ”", "‘¼‚Ì•a°‚ÖˆÚ‚³‚ê‚½Š³ŽÒ”", "•a°”")))

  result[[14]] <- res

  res <- readWorksheet(wb, sheet="ŽQl•\‚P‚S", startRow=8, startCol=2, endRow=-1, endCol=10, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("f—ÃŠ‚Ì‰îŒì—Ã—{•a°", c("Ý‰@Š³ŽÒ‰„”", "ŒŽ––Ý‰@Š³ŽÒ”", "V“ü‰@Š³ŽÒ”", "‘¼‚Ì•a°‚©‚çˆÚ‚³‚ê‚½Š³ŽÒ”", "‘Þ‰@Š³ŽÒ”", "‘¼‚Ì•a°‚ÖˆÚ‚³‚ê‚½Š³ŽÒ”", "•a°”")))

  result[[15]] <- res

  names(result) <- c("data",str_c("t", 1:14))
  invisible(result)
  }