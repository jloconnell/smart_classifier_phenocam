###R script for processing PhenoCam metadata
###extract date_time and image type from a list of image names
###images must be a character vector that lists all of the images
library(stringr)

get.info<-function(images){
  irimages<-images[grep('_IR_', images)]
  images<-images[grep('gcesapelo_20', images)]
 # if (length(images==0)){print(
  #  "site must be sapelo, and year begins with 20; modify function")}
  image_data<-str_split(images, "_")
  image_num<-length(images)
  y<-seq(1:image_num)
  for (i in 1:5){
    w<-sapply(image_data, "[[", i)
    y<-cbind(y,w)}
  image_data<-data.frame(y)
  colnames(image_data)<-c("index", "site", "year", "month", "day", "hr_min")
  #image_data$index<-seq(1:image_num)
  image_data$IR<-0
  image_data$date<-paste(image_data$year, image_data$month, image_data$day, sep="-")
  image_data$hour<-substr(image_data[,6], 1, 2)
  image_data$min<-substr(image_data[,6], 3, 4)
  image_data$sec<-substr(image_data[,6], 5, 6)
  image_data$hr_min<-paste(image_data$hour, image_data$min, sep=":")
  image_data$date<-paste(image_data$year, image_data$month, image_data$day, sep="-")
  image_data$date_time<-as.POSIXct(strptime(paste(image_data$date, 
          image_data$hr_min, sep=" "), tz="", format="%Y-%m-%d %H:%M"))
  image_data<-cbind(image_data, images)
  
  if(length(irimages>0)){
    irimage_num<-length(irimages)
    irimage_data<-str_split(irimages, "_")
    y<-seq(1:irimage_num)
    for (i in 1:6){
      w<-sapply(irimage_data, "[[", i)
      y<-cbind(y,w)}
    irimage_data<-data.frame(y)
    colnames(irimage_data)<-c("index", "site", "IR", "year", "month", "day", "hr_min")
    #irimage_data$index<-seq(1:irimage_num)
    irimage_data$date<-paste(irimage_data$year, irimage_data$month, irimage_data$day, sep="-")
    irimage_data$hour<-substr(irimage_data[,7], 1, 2)
    irimage_data$min<-substr(irimage_data[,7], 3, 3)
    irimage_data$min<-paste(irimage_data$min, 0, sep="")
    irimage_data$sec<-substr(irimage_data[,7], 5, 6)
    irimage_data$hr_min<-paste(irimage_data$hour, irimage_data$min, sep=":")
    irimage_data$date<-paste(irimage_data$year, irimage_data$month, irimage_data$day, sep="-")
    irimage_data$date_time<-as.POSIXct(strptime(paste(irimage_data$date, 
                                                      irimage_data$hr_min, sep=" "), tz="", format="%Y-%m-%d %H:%M"))
    irimage_data<-cbind(irimage_data, irimages)
    irimage_data<-irimage_data[,12:13]
    image_data<-merge(image_data, irimage_data, by.x="date_time", by.y="date_time", all.x=TRUE, all.y=FALSE) 
    image_data$IR<-ifelse(is.na(image_data$irimages), 0,1)
  }
image_data$index<-seq(1:nrow(image_data))
  return(image_data)
}
