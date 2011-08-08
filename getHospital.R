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

  res <- readWorksheet(wb, sheet="�ް�", startRow=6, startCol=2, endRow=-1, endCol=7, header=FALSE)
  res <- subset(res[, -5], complete.cases(res[,-5]))
  ynum <- c(grep(".+\\n", res[,1]), Inf) -1 #�N�̋�؂�x�N�g��
  ylab <- gsub(".+\\n", "", res[grep(".+\\n", res[,1]), 1]) #�N�̃��x��
  year <- cut(1:nrow(res), breaks=yearnum, labels=ylab) #��؂�x�N�g���ɏ]���ĔN���x���𐶐�
  res[,1] <- as.integer(gsub("\\n.+", "", res[,1])) #���x�N�g���ɕϊ�
  res <- apply(res, 2, as.numeric)
  res <- data.frame(year, res)
  colnames(res) <- c("year", "month", "1�����ύ݉@����", "1�����ϊO������", "���ύ݉@����", "�a�����p��")

  result[[1]] <- res

  res <- readWorksheet(wb, sheet="�Q�l�\�P", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("1�����ύ݉@���Ґ��i�a�@�j", c("����", "���_�a��", "���j�a��", "�×{�a��", "��ʕa��", "���×{�a��")))

  result[[2]] <- res
  
  res <- readWorksheet(wb, sheet="�Q�l�\�Q", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", "1�����ϊO�����Ґ��i�a�@�j����", "1�����ϊO�����Ґ��i�a�@�j���_�ȕa�@", "1�����ϊO�����Ґ��i�a�@�j��ʕa�@", "�O�����҉����i�a�@�j����", "�O�����҉����i�a�@�j���_�ȕa�@", "�O�����҉����i�a�@�j��ʕa�@")

  result[[3]] <- res

  res <- readWorksheet(wb, sheet="�Q�l�\�R", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("�����a�����p���i�a�@�j", c("����", "���_�a��", "���j�a��", "�×{�a��", "��ʕa��", "���×{�a��")))

  result[[4]] <- res

  res <- readWorksheet(wb, sheet="�Q�l�\�S", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("���ύ݉@�����i�a�@�j", c("����", "���_�a��", "���j�a��", "�×{�a��", "��ʕa��", "���×{�a��")))

  result[[5]] <- res

  res <- readWorksheet(wb, sheet="�Q�l�\�T", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("�݉@���҉����i�a�@�j", c("����", "���_�a��", "���j�a��", "�×{�a��", "��ʕa��", "���×{�a��")))

  result[[6]] <- res

  res <- readWorksheet(wb, sheet="�Q�l�\�U", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("�����݉@���Ґ��i�a�@�j", c("����", "���_�a��", "���j�a��", "�×{�a��", "��ʕa��", "���×{�a��")))

  result[[7]] <- res

  res <- readWorksheet(wb, sheet="�Q�l�\�V", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("�a�����i�a�@�j", c("����", "���_�a��", "���j�a��", "�×{�a��", "��ʕa��", "���×{�a��")))

  result[[8]] <- res

  res <- readWorksheet(wb, sheet="�Q�l�\�W", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("�V���@���Ґ��i�a�@�j", c("����", "���_�a��", "���j�a��", "�×{�a��", "��ʕa��", "���×{�a��")))

  result[[9]] <- res

  res <- readWorksheet(wb, sheet="�Q�l�\�X", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("�މ@���Ґ��i�a�@�j", c("����", "���_�a��", "���j�a��", "�×{�a��", "��ʕa��", "���×{�a��")))

  result[[10]] <- res

  res <- readWorksheet(wb, sheet="�Q�l�\�P�O", startRow=8, startCol=2, endRow=-1, endCol=10, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("�a�@�̗×{�a��", c("�݉@���҉���", "�����݉@���Ґ�", "�V���@���Ґ�", "���̕a������ڂ��ꂽ���Ґ�", "�މ@���Ґ�", "���̕a���ֈڂ��ꂽ���Ґ�", "�a����")))

  result[[11]] <- res

  res <- readWorksheet(wb, sheet="�Q�l�\�P�P", startRow=8, startCol=2, endRow=-1, endCol=10, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("�a�@�̉��×{�a��", c("�݉@���҉���", "�����݉@���Ґ�", "�V���@���Ґ�", "���̕a������ڂ��ꂽ���Ґ�", "�މ@���Ґ�", "���̕a���ֈڂ��ꂽ���Ґ�", "�a����")))

  result[[12]] <- res


  res <- readWorksheet(wb, sheet="�Q�l�\�P�Q", startRow=8, startCol=2, endRow=-1, endCol=9, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", c(str_c("�a�@�̉��×{�a��", c("�×{�a��", "���×{�a��")), str_c("�a�����p���i�f�Ï��j", c("�×{�a��", "���×{�a��")), str_c("���ύ݉@�����i�f�Ï��j", c("�×{�a��", "���×{�a��"))))

  result[[13]] <- res

  res <- readWorksheet(wb, sheet="�Q�l�\�P�R", startRow=8, startCol=2, endRow=-1, endCol=10, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("�f�Ï��̗×{�a��", c("�݉@���҉���", "�����݉@���Ґ�", "�V���@���Ґ�", "���̕a������ڂ��ꂽ���Ґ�", "�މ@���Ґ�", "���̕a���ֈڂ��ꂽ���Ґ�", "�a����")))

  result[[14]] <- res

  res <- readWorksheet(wb, sheet="�Q�l�\�P�S", startRow=8, startCol=2, endRow=-1, endCol=10, header=FALSE)
  res <- subset(res[, -2], complete.cases(res[,-2]))
  res[,-1] <- apply(res[,-1], 2, as.numeric)
  colnames(res) <- c("pref", str_c("�f�Ï��̉��×{�a��", c("�݉@���҉���", "�����݉@���Ґ�", "�V���@���Ґ�", "���̕a������ڂ��ꂽ���Ґ�", "�މ@���Ґ�", "���̕a���ֈڂ��ꂽ���Ґ�", "�a����")))

  result[[15]] <- res

  names(result) <- c("data",str_c("t", 1:14))
  invisible(result)
  }