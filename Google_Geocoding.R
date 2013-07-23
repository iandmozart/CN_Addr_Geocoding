setwd("F:/Khaki/CVD&RespiratoryDisease/track")
#Before installing the RJSONIO, a JAVA installation might be required.
install.packages("RJSONIO")
install.packages("RCurl")
library(RJSONIO)
library(RCurl)
# library(qdap) #It could not be installed. Dont know why
#This following function is a multiple gsub() originally in library(qdap). I copied it from
#http://stackoverflow.com/questions/15253954/replace-multiple-arguments-with-gsub
#Google Maps Geocoding API Service could offer better address analysis than Baidu. If possible, I suggest to use Google Geocoding only(No Baidu).
#Google Maps Geocoding API Service has a limitation of 2,500 query per day. You could buy a stable and high-speed VPN to run it.
#The VPN I used is https://www.grjsq.biz/ You could choose the server.

#mgsub() is a gsub() function of multiple replacement. You could read the the gsub() documentation for more details.
mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}
#construct.geocode.url() is a function aiming to get a json file location.
#For the paramaters in this url, you could refer the Google Maps Geocoding API Service. 
#Link: https://developers.google.com/maps/documentation/geocoding/
construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&language=zh-CN","&sensor=", sensor, sep = "")
  return(URLencode(u))
}
#Geocoding
gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"/n")
  connectStr <- construct.geocode.url(address)
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  data.json <- unlist(data.json)
  if(data.json["status"]=="OK"){
    lng <- iconv(data.json["results.geometry.location.lng"],"UTF-8","gb2312")
    lat <- iconv(data.json["results.geometry.location.lat"],"UTF-8","gb2312")
    return(c(lat, lng))
  } else {
    return(c(NA,NA))
  }
}
#Reverse Geocoding 
reverseGeoCode <- function(latlng) {
  latlngStr <-  gsub(' ','%20', paste(latlng, collapse=","))#Collapse and Encode URL Parameters
  library("RJSONIO") #Load Library
  #Open Connection
  connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&language=zh-CN&latlng=',latlngStr, sep="")
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  #Flatten the received JSON
  data.json <- unlist(data.json)
  if(data.json["status"]=="OK") {    
    address <- iconv(data.json["results.formatted_address"],"UTF-8","gb2312")
    return (address)
  } else {
    return(NA)
  } 
}

#This is a csv file with key and location for the observations.
#Format:
# key   addr
# 1    北京市朝阳区大屯路甲11号中国科学院地理科学与资源研究所
# 2    北京市西城区新街口外大街12号
#...
book <- read.csv("db2_Addr_Final.csv")
#This is a csv file for the useless phrases in the addr.
#Format
# Jiedao
# 东华门街道办事处
# 景山街道办事处
# 交道口街道办事处
# 安定门街道办事处
#...
jiedao <- read.csv("Jiedao_ToDel.csv")
#Because Google has a overall_limitation of 2,500 query per day. You could choose the lines you will run via this IP.
#(After this trial, you should disconnect the VPN, and begin to run wia another IP(VPN server)

#Please make sure the format of the files are right.
head(book)
head(jiedao)

#Let's ROAR!!!!!!
Begin <- 1
End <- 2500 #No more than 2,500
coor <- rep(NA,3)

for (i in Begin:End) {
  library(RCurl)
  ad <- book$Baddress[i]
  blank <- rep("",dim(jiedao)[1])
  ad <- mgsub(jiedao[,1],blank,ad)
  if(ad==ad1) {
    result<- gGeoCode(iconv(ad1,"gb2312","UTF-8"))
  }else{
    result<- gGeoCode(ad1)
  }

  result <- c(book[i,"key"],result)
  coor <- rbind(coor,result)
}
coor <- as.data.frame(coor[-1,])
colnames(coor) <- c("key","results.geometry.location.lat","results.geometry.location.lng")
rownames(coor) <- 1:dim(coor)[1]
fileName <- paste("gCoor",Begin,Begin+dim(coor)[1]-1".csv",sep="_")
write.csv(coor,filename)

#Reverse Geocoding
addr <- rep(NA,2)
for (i in Begin:End) {
  library(RCurl)
  latlng <- paste(coor[i,"results.geometry.location.lat"],coor[i,"results.geometry.location.lng"],sep=",")
  result <- reverseGeoCode(latlng)
  result <- c(coor[i,1],result)
  addr <- rbind(addr,result)
}
addr <- as.data.frame(addr[-1,])
colnames(addr) <- c("key","results.formatted_address")
rownames(addr) <- 1:dim(addr)[1]
write.csv(addr,"addr.csv")
fileName <- paste("gAddr",Begin,Begin+dim(addr)[1]-1".csv",sep="_")
write.csv(addr,filename)


continue <- merge(book,addr,by="key",incomparables = NA)
final <- merge(continue,coor,by="key",incomparables=NA)
write.csv(final,"final.csv")
