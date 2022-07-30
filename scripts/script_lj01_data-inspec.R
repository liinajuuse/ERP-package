fi <- list.files(pattern = '\\.dat')

t0 <- Sys.time()

for (j in 1:length(fi)) {
  t = read.table(file = fi[j], header = T, sep = ";")
  t2 = as.data.frame(nrow(t))
  t2$File = fi[j]
  t2 = t2[,c(2,1)]
  #t2$Subj = substr(fi[j], 6, 8)
  #t2$Marker <- substr(fi[j], nchar(fi[j])-6, nchar(fi[j])-4)
  names(t2) = c("File", "Rows")
  
  timestamp()
  print(paste(fi[j], ": Done!"))
  
  row.values = rbind(row.values, t2)
  }

(Sys.time()-t0)


# -------------------------------------------------------------------------


data_inspec_false = subset(data_inspec, Trials != 6)
write.csv(data_inspec, file = "data_inspec_threshold.csv")

save(data_threshold, file="data-threshold.RData")
write.csv(data.ERP, file = paste(output,"data-ERP.csv", sep = ''))