#��Ë@�֕�

conv_iryoukikan <- function(file, sheet){
  wb <- loadWorkbook(file)
  ws <- readWorksheet(wb, sheet, header=FALSE, startRow=-1, startCol=-1, endRow=419, endCol=140) #140�͌��ߑł�
  month <- as.numeric(ws[3,]) #�N���擾
  month <- month[!is.na(month)]
  id <- apply(ws[6:nrow(ws),1:2], 2, as.numeric) #�f�[�^ID�擾
  data <- ws[6:nrow(ws), 16:(15+length(month))]#�f�[�^�擾
  data <- apply(data, 2, as.numeric)
  res <- data.frame(id, data)
  colnames(res) <- c("code1", "code2", month)
  invisible(res)
  }


#�a���K�͕�
conv_byoushou <- function(file, sheet, new=TRUE){
  wb <- loadWorkbook(file)
  ws <- readWorksheet(wb, sheet, header=FALSE, startRow=-1, startCol=-1, endRow=527, endCol=-1)
  month <- as.numeric(ws[3,16:ncol(ws)]) #�N���擾
  id <- apply(ws[6:nrow(ws),1:2], 2, as.numeric) #�f�[�^ID�擾
  data <- ws[6:nrow(ws), 16:ncol(ws)]#�f�[�^�擾
  data <- apply(data, 2, as.numeric)
  res <- data.frame(id, data)
  colnames(res) <- c("code1", "code2", month)
  invisible(res)
  }

#�a�@�敪�ʈꌏ�������Ô�����߂�֐�
per_iryouhi <- function(iryouhi, kensuu, seido=0){
  iryouhi[,-c(1:2)] <- iryouhi[,-c(1:2)] / kensuu[,-c(1:2)] #�ꌏ�������Ô�

  iryouhi <- merge(iryouhi, dseido, all.x=TRUE)
  iryouhi <- merge(iryouhi, dkikan, all.x=TRUE)
  
  iryouhi <- iryouhi[iryouhi$code1==seido, ]
  iryouhi <- iryouhi[iryouhi$kikan %in% c("��ȕa�@��w�a�@", "��ȕa�@���I�a�@", "��ȕa�@�@�l�a�@", "��ȕa�@�l�a�@"), ]

  iryouhi.m <- melt(iryouhi, id.var=c("kikan", "seido", "code1", "code2"))
  iryouhi.m$variable <- as.POSIXct(str_c(iryouhi.m$variable, "01"), format="%Y%m%d")
  iryouhi.m$kikan <- as.character(iryouhi.m$kikan)
  res <- iryouhi.m
  }

#�a�@�敪�ʈ�Ô�/���������߂�֐�
calc_data <- function(data, seido=0, kikan=c("��ȕa�@��w�a�@", "��ȕa�@���I�a�@", "��ȕa�@�@�l�a�@", "��ȕa�@�l�a�@")){

  data <- merge(data, dseido, all.x=TRUE)
  data <- merge(data, dkikan, all.x=TRUE)
  
  data <- data[data$code1==seido, ]
  data <- data[data$kikan %in% kikan, ]

  data.m <- melt(data, id.var=c("kikan", "seido", "code1", "code2"))
  data.m$variable <- as.POSIXct(str_c(data.m$variable, "01"), format="%Y%m%d")
  data.m$kikan <- as.character(data.m$kikan)
  res <- data.m
  }