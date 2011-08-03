#get Information from http://www.mhlw.go.jp/toukei/saikin/hw/life/life10/
#"sankousiryou2" ----> "Heikinyomei no keinen suii"

getHeikinyomei <- function(wgetpath){
  require(XLConnect)
  require(ggplot2)
  require(stringr)
  tmpdir <- tempdir()
  getFiles(list("http://www.mhlw.go.jp/toukei/saikin/hw/life/life10/xls/sankou.xls"), dir=tmpdir, wgetpath=wgetpath)

  wb <- loadWorkbook(str_c(tmpdir, "/sankou.xls"))
  res <- readWorksheet(wb, sheet=1, startRow=5, startCol=1, endRow=-1, endCol=15, header=FALSE)
  res <- subset(res[, -c(2,3)], complete.cases(res[, -c(2,3)]) &res[,1]!="*1950-1952")
  colnames(res) <- gsub("Î|\\.0", "", res[1,])
  colnames(res) <- c("seireki", str_c(sep="X", "male", unique(colnames(res)[-1])), str_c(sep="X", "female", unique(colnames(res)[-1])))

  res <- res[-1, ]
  seireki <- res[,1]
  seireki <- c(1947:ifelse(nchar(seireki[length(seireki)])==4, seireki[length(seireki)], 2000+as.numeric(seireki[length(seireki)])))
  res[,1] <- seireki
  res <- data.frame(apply(res, 2, function(x)as.numeric(gsub("c", NA, x))))
  res.m <- melt(res, id.var="seireki")
  sexage <- do.call("rbind", strsplit(as.character(res.m[,2]), split="X"))
  res.m <- cbind(res.m, sexage)
  res.m[,2] <- NULL
  colnames(res.m) <- c("seireki", "heikinyomei", "sex", "age")
  invisible(res.m)
  }