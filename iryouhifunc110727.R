#医療機関別

conv_iryoukikan <- function(file, sheet){
  wb <- loadWorkbook(file)
  ws <- readWorksheet(wb, sheet, header=FALSE, startRow=-1, startCol=-1, endRow=419, endCol=140) #140は決め打ち
  month <- as.numeric(ws[3,]) #年月取得
  month <- month[!is.na(month)]
  id <- apply(ws[6:nrow(ws),1:2], 2, as.numeric) #データID取得
  data <- ws[6:nrow(ws), 16:(15+length(month))]#データ取得
  data <- apply(data, 2, as.numeric)
  res <- data.frame(id, data)
  colnames(res) <- c("code1", "code2", month)
  invisible(res)
  }


#病床規模別
conv_byoushou <- function(file, sheet, new=TRUE){
  wb <- loadWorkbook(file)
  ws <- readWorksheet(wb, sheet, header=FALSE, startRow=-1, startCol=-1, endRow=527, endCol=-1)
  month <- as.numeric(ws[3,16:ncol(ws)]) #年月取得
  id <- apply(ws[6:nrow(ws),1:2], 2, as.numeric) #データID取得
  data <- ws[6:nrow(ws), 16:ncol(ws)]#データ取得
  data <- apply(data, 2, as.numeric)
  res <- data.frame(id, data)
  colnames(res) <- c("code1", "code2", month)
  invisible(res)
  }

#病院区分別一件当たり医療費を求める関数
per_iryouhi <- function(iryouhi, kensuu, seido=0){
  iryouhi[,-c(1:2)] <- iryouhi[,-c(1:2)] / kensuu[,-c(1:2)] #一件当たり医療費

  iryouhi <- merge(iryouhi, dseido, all.x=TRUE)
  iryouhi <- merge(iryouhi, dkikan, all.x=TRUE)
  
  iryouhi <- iryouhi[iryouhi$code1==seido, ]
  iryouhi <- iryouhi[iryouhi$kikan %in% c("医科病院大学病院", "医科病院公的病院", "医科病院法人病院", "医科病院個人病院"), ]

  iryouhi.m <- melt(iryouhi, id.var=c("kikan", "seido", "code1", "code2"))
  iryouhi.m$variable <- as.POSIXct(str_c(iryouhi.m$variable, "01"), format="%Y%m%d")
  iryouhi.m$kikan <- as.character(iryouhi.m$kikan)
  res <- iryouhi.m
  }

#病院区分別医療費/件数を求める関数
calc_data <- function(data, seido=0, kikan=c("医科病院大学病院", "医科病院公的病院", "医科病院法人病院", "医科病院個人病院")){

  data <- merge(data, dseido, all.x=TRUE)
  data <- merge(data, dkikan, all.x=TRUE)
  
  data <- data[data$code1==seido, ]
  data <- data[data$kikan %in% kikan, ]

  data.m <- melt(data, id.var=c("kikan", "seido", "code1", "code2"))
  data.m$variable <- as.POSIXct(str_c(data.m$variable, "01"), format="%Y%m%d")
  data.m$kikan <- as.character(data.m$kikan)
  res <- data.m
  }