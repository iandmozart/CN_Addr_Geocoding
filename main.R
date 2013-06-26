setwd("F:/Khaki/CVD&RespiratoryDisease/track")
install.packages("RJSONIO")
install.packages("RCurl")
library(RJSONIO)
library(RCurl)
# library(qdap) #It could not be installed. Dont know why
#This following function is a multiple gsub() originally in library(qdap). I copied it from
#http://stackoverflow.com/questions/15253954/replace-multiple-arguments-with-gsub
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
construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&language=zh-CN","&sensor=", sensor, sep = "")
  return(URLencode(u))
}

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


book <- read.csv("db2_Addr_Final.csv")
# book <- book[11000,]
jiedao <- read.csv("Jiedao_ToDel.csv")
Begin <- 1
End <- 50000
#1:5000;50001:100000;100001:dim(book)[1]
head(book)
head(jiedao)
coor <- rep(NA,3)

for (i in Begin:End) {
  library(RCurl)
  ad <- book$Baddress[i]
  blank <- rep("",dim(jiedao)[1])
  ad <- mgsub(jiedao[,1],blank,ad)
#   ad <- as.character(gsub("街道办事处","",ad))
  #   result<- gGeoCode(iconv(ad,"gb2312","UTF-8"))
  result<- gGeoCode(ad)
  
  result <- c(book[i,"key"],result)
  coor <- rbind(coor,result)
}
coor <- as.data.frame(coor[-1,])
colnames(coor) <- c("key","results.geometry.location.lat","results.geometry.location.lng")
rownames(coor) <- 1:dim(coor)[1]
write.csv(coor,"coor1.csv")
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


continue <- merge(book,addr,by="key",incomparables = NA)
final <- merge(continue,coor,by="key",incomparables=NA)
write.csv(final,"final.csv")
