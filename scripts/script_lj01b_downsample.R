setwd("C:/Users/liina/OneDrive/PhD/EMO2021/EMO2021/ERP-package/input")

rw <- list.files(pattern = '\\.dat')

rw <- rw[grepl(pattern = '020f23ee', x = rw)]

for (i in 1:length(rw)) {
  
  b <- read.table(file = rw[i], header = T, sep = ';')
  b <- b[seq(1, nrow(b), by = 4), ]
  b <- b[1:22116, ]
  rownames(b) <- NULL
  write.table(x = b, file = paste('p20/', rw[i], sep = ''), sep = ';')
  print(paste(rw[i], 'Done!', sep = ' '))
}
