setwd("F:/Khaki/lreis/CVD&RespiratoryDisease/track/baidu/try")
list.files <- list.files()[-(40:41)]
all.csv <- read.csv(list.files[1])
# i <- list.files[-1][1]
for (i in list.files[-1]) {
  i.csv <- read.csv(i)
  all.csv <- rbind(all.csv,i.csv)
}
# all.csv
# a <- sort(table(all.csv$key),decreasing=TRUE)
# which(a>1)
# all.csv[all.csv$key==44988,]
all.csv <- all.csv[order(all.csv$key),]#make the dataset by the key's increasing order
keys <- as.matrix(all.csv$key,1,length(all.csv$key))
duplicated <- duplicated(all.csv$key)
merge <- cbind(keys,duplicated)
for (i in all.csv$key) {
  
}
