#Baidu Maps Geocoding Service documentation: http://developer.baidu.com/map/webservice-geocoding.htm
#Please read through the Google_Geocoding.R firstly,
#Baidu GeoCode sample: http://api.map.baidu.com/geocoder?address=地址&output=输出格式类型&key=用户密钥&city=城市名
#Baidu ReverseGeoCode sample: http://api.map.baidu.com/geocoder?location=纬度,经度&output=输出格式类型&key=用户密钥
#Baidu user name: 蝶舞凄扬
#Baidu Password: xxxx=xxxx or xxxxxxxx

#Baidu Key: 9ce566a6dd950173e625cd335c8b23a4
#Another Baidu Key: 37492c0ee6f924cb5e934fa08c6b1676

setwd("F:/Khaki/CVD&RespiratoryDisease/track/baidu")
# install.packages("RJSONIO")
# install.packages("RCurl")
library(RJSONIO)
library(RCurl)
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
#http://api.map.baidu.com/geocoder?address=地址&output=输出格式类型&key=用户密钥&city=城市名
# key <- 9ce566a6dd950173e625cd335c8b23a4
construct.geocode.url <- function(address, return.call = "json", key= "9ce566a6dd950173e625cd335c8b23a4", city="北京市") {
  root <- "http://api.map.baidu.com/geocoder"
  u <- paste(root, "?address=", address, "&output=", return.call, "&key=",key,"&city=", city, sep = "")
  return(URLencode(u))
}


bGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"/n")
  connectStr <- construct.geocode.url(address)
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  data.json <- unlist(data.json)
  if(data.json["status"]=="OK"){
    lng <- iconv(data.json["result.location.lng"],"UTF-8","gb2312")
    lat <- iconv(data.json["result.location.lat"],"UTF-8","gb2312")
    return(c(lat, lng))
  } else {
    return(c(NA,NA))
  }
}

# {status: '字符串状态常量', 取值如下：
#  //OK 成功
#  INVILID_KEY 非法密钥 
#  INVALID_PARAMETERS 非法参数，参数错误时候给出。
#  result: {    
#    location: {
#      lat: 纬度：数值，
#      lng: 经度：数值
#    },
#    precise:’位置的附加信息，是否精确查找’（1为精确查找，0为不精确查找）,
#    confidence: 可信度,
#    level:'级别'
#  },
# }




#Reverse Geocoding 
# http://api.map.baidu.com/geocoder?location=纬度,经度&output=输出格式类型&key=用户密钥
bReverseGeoCode <- function(latlng) {
  latlngStr <-  gsub(' ','%20', paste(latlng, collapse=","))#Collapse and Encode URL Parameters
  library("RJSONIO") #Load Library
    #Open Connection
  connectStr <- paste("http://api.map.baidu.com/geocoder?location=",latlngStr,"&output=json&key=9ce566a6dd950173e625cd335c8b23a4", sep="")
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  #Flatten the received JSON
  data.json <- unlist(data.json)
  if(data.json["status"]=="OK") {    
    address <- iconv(data.json["result.formatted_address"],"UTF-8","gb2312")
    return (address)
  } else {
    return(NA)
  } 
}


book <- read.csv("db2_Addr_Final.csv")
# book <- book[11000,]
jiedao <- read.csv("Jiedao_ToDel.csv")
Begin <- 50001
End <- 100000
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
  result<- bGeoCode(ad)
  
  result <- c(book[i,"key"],result)
  coor <- rbind(coor,result)
}
coor <- as.data.frame(coor[-1,])
colnames(coor) <- c("key","result.location.lat","result.location.lng")
rownames(coor) <- 1:dim(coor)[1]
write.csv(coor,"coor2.csv")
#####################################################################
#####################################################################
addr <- rep(NA,2)
for (i in Begin:End) {
  library(RCurl)
  I <- i - Begin + 1
  latlng <- paste(coor[I,"result.location.lat"],coor[I,"result.location.lng"],sep=",")
  result <- bReverseGeoCode(latlng)
  result <- c(coor[I,1],result)
  addr <- rbind(addr,result)
}
addr <- as.data.frame(addr[-1,])
colnames(addr) <- c("key","result.formatted_address")
rownames(addr) <- 1:dim(addr)[1]
write.csv(addr,"addr2.csv")

#####################################################################
#####################################################################
continue <- merge(book,addr,by="key",incomparables = NA)
final <- merge(continue,coor,by="key",incomparables=NA)
write.csv(final,"final.csv")
