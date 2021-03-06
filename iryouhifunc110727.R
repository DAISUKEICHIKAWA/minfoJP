#ãÃ@ÖÊ

conv_iryoukikan <- function(file, sheet){
  wb <- loadWorkbook(file)
  ws <- readWorksheet(wb, sheet, header=FALSE, startRow=-1, startCol=-1, endRow=419, endCol=140) #140ÍßÅ¿
  month <- as.numeric(ws[3,]) #Næ¾
  month <- month[!is.na(month)]
  id <- apply(ws[6:nrow(ws),1:2], 2, as.numeric) #f[^IDæ¾
  data <- ws[6:nrow(ws), 16:(15+length(month))]#f[^æ¾
  data <- apply(data, 2, as.numeric)
  res <- data.frame(id, data)
  colnames(res) <- c("code1", "code2", month)
  invisible(res)
  }


#a°KÍÊ
conv_byoushou <- function(file, sheet, new=TRUE){
  wb <- loadWorkbook(file)
  ws <- readWorksheet(wb, sheet, header=FALSE, startRow=-1, startCol=-1, endRow=527, endCol=-1)
  month <- as.numeric(ws[3,16:ncol(ws)]) #Næ¾
  id <- apply(ws[6:nrow(ws),1:2], 2, as.numeric) #f[^IDæ¾
  data <- ws[6:nrow(ws), 16:ncol(ws)]#f[^æ¾
  data <- apply(data, 2, as.numeric)
  res <- data.frame(id, data)
  colnames(res) <- c("code1", "code2", month)
  invisible(res)
  }

#a@æªÊê½èãÃïðßéÖ
per_iryouhi <- function(iryouhi, kensuu, seido=0){
  iryouhi[,-c(1:2)] <- iryouhi[,-c(1:2)] / kensuu[,-c(1:2)] #ê½èãÃï

  iryouhi <- merge(iryouhi, dseido, all.x=TRUE)
  iryouhi <- merge(iryouhi, dkikan, all.x=TRUE)
  
  iryouhi <- iryouhi[iryouhi$code1==seido, ]
  iryouhi <- iryouhi[iryouhi$kikan %in% c("ãÈa@åwa@", "ãÈa@öIa@", "ãÈa@@la@", "ãÈa@Âla@"), ]

  iryouhi.m <- melt(iryouhi, id.var=c("kikan", "seido", "code1", "code2"))
  iryouhi.m$variable <- as.POSIXct(str_c(iryouhi.m$variable, "01"), format="%Y%m%d")
  iryouhi.m$kikan <- as.character(iryouhi.m$kikan)
  res <- iryouhi.m
  }

#a@æªÊãÃï/ðßéÖ
calc_data <- function(data, seido=0, kikan=c("ãÈa@åwa@", "ãÈa@öIa@", "ãÈa@@la@", "ãÈa@Âla@")){

  data <- merge(data, dseido, all.x=TRUE)
  data <- merge(data, dkikan, all.x=TRUE)
  
  data <- data[data$code1==seido, ]
  data <- data[data$kikan %in% kikan, ]

  data.m <- melt(data, id.var=c("kikan", "seido", "code1", "code2"))
  data.m$variable <- as.POSIXct(str_c(data.m$variable, "01"), format="%Y%m%d")
  data.m$kikan <- as.character(data.m$kikan)
  res <- data.m
  }