###THE SMART CLASSIFIER ALGORITHM
###R Scrit
###From: O’Connell, J. L., and M. Alber. 2016. A smart classifier for extracting 
###          environmental data from digital image time-series: Applications for PhenoCam 
###          data in a tidal salt marsh. Environmental Modelling & Software 84:134–139.
###
###writes 0 into non-veg pixels and writes out a classified jpeg 
###if percent of veg pixels in ROI is high enough
###writes out a text file of the classification results
###ability to append to earlier results and specify dates to analyze is included
###in windows, need to change out dir path late in script

##load libraries
library(biOps);library(stringr);library("abind")
#######################################################################################
#####PROVIDE USER INPUT BELOW##########################################################
#######################################################################################

##paths to files
root_path<-"path/to/project/data"
##write out a text file of results? Append old results writen previously to new?
append.out<-FALSE
write.out<-TRUE

###paths to files
##load function for creating a dataframe of time, date and IR status from the file names
source(paste(root_path, "get_phenocam_image_info.r",sep=""))
##where are the raw jpegs to process
basedir<-paste(root_path,"example_images", sep="")
##where to save processed files?
outdir<-paste(root_path,"classified_photos", sep="")

##target dates to process; if you want to taget some dates, set all.days to FALSE, else TRUE
alldays<-TRUE
start.day<-as.Date("2017-02-15")
end.day<-as.Date("2017-02-16")

######END OF USER INPUT############
setwd(basedir)

##make a list of all the files in the folder
images<-list.files(recursive=TRUE)
##use only the jpgs
images<-images[grep('.jpg', images)]
##how many images are there?
#image_num<-length(images)
##parse the file names into date and time info for rgb and ir images; requires custom function
image_data<-get.info(images)

##subset to target days
if(alldays==FALSE){
  image_data$date<-as.Date(image_data$date)
  image_data<-image_data[image_data$date>=start.day&image_data$date<=end.day,]
}

##load the ROI as a mask object and melt from an array to a dataframe
#mask<-readTiff("harvard_deciduous_0001_01.tif")#Test data
#mask<-readTiff("ROI/gceROI_test2.tiff")
mask<-readJpeg("../ROI/gceROI_marsh.jpg")
skymask<-readJpeg("../ROI/gceROI_sky.jpg")

##cycle through the images, subset to ROI, reclassify remaining pixels
##and write out reclassified jpeg if high number of pixels in ROI are veg; 
##writing out the classified photo checks that classifier is working; 
##for downstream analysis, I use the raw photos identified as "good"
results<-data.frame(date_time=as.POSIXct(image_data[,"date_time"]),
                    marshpixels=rep(999,nrow(image_data)) ,
                    avgmarshvalue=rep(999,nrow(image_data)),
                    percent_shadow=rep(999,nrow(image_data)), 
                    percent_veg=rep(999,nrow(image_data)), 
                    percent_lowwhite=rep(999,nrow(image_data)),
                    percent_water=rep(999,nrow(image_data)), 
                    #percent_cloudshad=rep(999,nrow(image_data)), 
                    skypixels=rep(999,nrow(image_data)),
                    avgskyvalue=rep(999,nrow(image_data)), 
                    percent_clouds=rep(999,nrow(image_data)),
                    percent_bluesky=rep(999,nrow(image_data)),
                    avggcc=rep(999,nrow(image_data)),
                    avgexg=rep(999,nrow(image_data)),
                    wfi=rep(999,nrow(image_data)),
                    bmean=rep(999,nrow(image_data)),
                    gmean=rep(999,nrow(image_data)),
                    rmean=rep(999,nrow(image_data))
)

for (i in 1:nrow(image_data)){
  #for (i in 3:3){
  #read in the jpeg
  x<-readJpeg(as.character(image_data[i,"images"]))##i
  #plot(x)
  ##convert image arrays to dataframe
  
  r <- as.array(x[,,1])
  g <-as.array(x[,,2])
  b <- as.array(x[,,3])
  r.sky <- as.array(x[,,1])
  g.sky <-as.array(x[,,2])
  b.sky <- as.array(x[,,3])
  
  ##subset to ROIs, marsh and sky
  r[mask[,,1]==0]<-0
  g[mask[,,2]==0]<-0
  b[mask[,,3]==0]<-0
  
  r.sky[skymask[,,1]==0]<-0
  g.sky[skymask[,,2]==0]<-0
  b.sky[skymask[,,3]==0]<-0
  
  #reclass <- do.call(abind, c(list(r,g,b), along = 3))
  #plot(imagedata(reclass))
  
  #######################################################################################################
  #######ReClass###########
  #######################################################################################################
  ##How many pixels in marsh ROI were veg, shadow, fog (whites) and water (dark grey)?
  ##note, this returns the number of cells >0, not the sum of their values, the ">" converts cells to TRUE/FALSE, where T=1
  pixel_num<-sum(mask[,,1]>0)
  pixel_num.sky<-sum(skymask[,,1]>0)
  
  ##reclassify to veg and non veg pixels in marsh ROI
  ##set up: get color ratios for pixels
  b_r<-b/r
  b_r[is.na(b_r)]<-0
  
  b_g<-b/g 
  b_g[is.na(b_g)]<-0
  
  bright<-g+b+r #helps distinguish shadows
  bright.sky<-g.sky+b.sky+r.sky #to calculate wfi
  g_r<-g/r
  g_r[is.na(g_r)]<-0
  
  gcc<-mean(g[g>0]/bright[g>0])
  exg<-mean(2*g[g>0]-(r[g>0]+b[g>0]))
  wfi<-(mean(b.sky[b.sky>0]/bright.sky[b.sky>0])-mean(r.sky[b.sky>0]/bright.sky[b.sky>0]))/
    (mean(b.sky[b.sky>0]/bright.sky[b.sky>0])+mean(r.sky[b.sky>0]/bright.sky[b.sky>0]))
  bmean<-mean(b[b>0])
  gmean<-mean(g[g>0])
  rmean<-mean(r[r>0])
  
  ##classify vegetation: edit: veg can just be the opposite of all non veg classes
  veg<-array(dim=c(960,1296), data=1)
  veg[mask[,,1]==0]<-0
  
  ##classify fog and white objects (birds)
  white<-array(dim=c(960,1296), data=0)
  white[b>150&g>150&r>150]<-1
  white[b>100&g>100&r>200&b_r>0.7]<-1
  
  ##smart classifier: classify more white pixels as fog, even those colors that sometimes can be veg
  ## when white crosses a threshold (ie. a dense fog washes out the whole scene)
  percent_white<-sum(white)/pixel_num*100
  white[percent_white>0.15&b>100&g>100&r>190&b_r>0.58]<-1
  percent_white<-sum(white)/pixel_num*100
  
  shadow<-array(dim=c(960,1296), data=0)
  shadow[bright<=32&r<16&mask[,,1]>0]<-1
  percent_shadow<-sum(shadow)/pixel_num*100
  
  ####classify sky#####
  brightsky<-b.sky+r.sky+g.sky
  b_r.sky<-b.sky/r.sky
  b_r.sky[is.na(b_r.sky)]<-0
  b_g.sky<-b.sky/g.sky
  b_g.sky[is.na(b_g.sky)]<-0
  
  bluesky<-array(dim=c(960,1296), data=0)
  
  ##some of the dark grey clouds have more green than blue
  bluesky[b_r.sky>1&b_g.sky>1&brightsky<745]<-1
  bluesky[b.sky<178]<-0#very dark blues are the undersides of clouds##180
  bluesky[b.sky<195&(b_r.sky|b_g.sky)<1.04]<-0#very dark blues are the undersides of clouds##200
  bluesky[b.sky<230&(b_r.sky)<1.02]<-0#very dark blues are the undersides of clouds
  percent_bluesky<-sum(bluesky)/pixel_num.sky*100
  
  ##classify cloulds as all non-bluesky pixels instead of an explict classification
  clouds<-array(dim=c(960,1296), data=0)
  clouds[skymask[,,1]>0&bluesky==0]<-1
  percent_clouds<-sum(clouds)/pixel_num.sky*100
  
  if(percent_clouds>30){
    ##using gradual increase of blue proportion to get more accuate dark cloud measure
    bluesky[b.sky<240&(b_r.sky)<1.1]<-0
    bluesky[b.sky<220&(b_r.sky)<1.15]<-0
    bluesky[b.sky<200&(b_r.sky)<1.2]<-0
    bluesky[b.sky<190&(b_r.sky)<1.25]<-0
    percent_bluesky<-sum(bluesky)/pixel_num.sky*100  
    clouds[skymask[,,1]>0&bluesky==0]<-1
    percent_clouds<-sum(clouds)/pixel_num.sky*100
  }
  
  ##use wfi to increase specificity (i.e remove false positives); 
  ##The false positives removed are from a dark grey haze low on the horizon, 
  ##rather than a full cloud cover
  ##haze is maybe smoke or smog 
  if(percent_clouds>0&wfi>0.14){
    percent_clouds<-0
  }
  if(wfi>0.12&percent_clouds>65){
    percent_clouds<-percent_clouds-25
  }
  if(percent_clouds<0){
    percent_clouds<-0
  }
  percent_bluesky<-100-percent_clouds
  
  ######CLASSIFY WATER####
  ##approach: set conservative first threshold that can select only water values;
  ##include values that overlap with vegetation if water thresholds are surpassed; 
  ##this approach allows more precise water quantity to be calculated
  ##gcc is used adjust the water calculation seasonally, so water can be separated from veg in all seasons
  water<-array(dim=c(960,1296), data=0)
  
  ##water values differ by season: summer, high sediment water is redder than green veg
  if(gcc>=0.366){    
    ##pull out very dark greys, nearly always water
    water[r<90&r>0&g<100&b_g>0.65&b_r>0.8]<-1
    ##pull out strong blues, these also are nearly always water
    water[b_r>0.9]<-1
    water[b_g>0.95]<-1
    water[g_r>0.93&g_r<1.08&b_r>0.75]<-1
  }
  ##spring/fall
  if(gcc<0.366&gcc>0.355){    
    water[r<80&r>0&g<80&b_g>0.65&b_r>0.85]<-1#water[r<100&r>0&g<110&b_g>0.62&b_r>0.68]<-1
    ##pull out strong blues, these also are nearly always water
    water[b_r>0.95]<-1
    water[b_g>0.98]<-1
    water[g_r>0.95&g_r<1.04&b_r>0.86]<-1
  }
  percent_water<-sum(water)/pixel_num*100
  percent_water
  
  ##winter, water more nearly matches veg (both brown) so more care is needed
  if(gcc<=0.355){    
    ##pull out strong blues, these also are nearly always water
    water[b_r>0.9]<-1
    water[b_g>0.86]<-1
    water[g_r>0.9&g_r<1.05&b_g>0.75]<-1 ##water[g_r>0.96&g_r<1.04&b_g>0.8]<-1 
  }
  water[shadow==1]<-0
  
  ###need this percent watere calc to inlude a smart water addition
  percent_water<-sum(water)/pixel_num*100
  
  ###when water is detected, the classification for water become more aggressive
  if(gcc>=0.366&percent_water>0.25){    
    water[r<110&r>0&g<110&g_r<1.04&g_r>0.86&b_g>0.58&b_r>0.75&r>0]<-1
    ##pull out strong blues, these also are nearly always water
    water[b_r>0.9]<-1
    water[b_g>0.9]<-1
    water[g_r>0.96&g_r<1.04&b_g>0.8]<-1 
    
  }
  if(gcc<0.366&gcc>0.355){    
    water[percent_water>0.3&r<100&r>0&g<100&g_r<1.04&g_r>0.88&b_g>0.58&b_r>0.77&r>0]<-1
    ##pull out strong blues, these also are nearly always water
    water[b_r>0.9]<-1
    water[b_g>0.9]<-1
    water[g_r>0.96&g_r<1.04&b_g>0.8]<-1 
  }
  if(gcc<=0.355){    
    water[percent_water>0.17&r<110&g<110&g_r<1.04&g_r>0.86&b_g>0.58&b_r>0.72&r>0]<-1
  }
  percent_water<-sum(water)/pixel_num*100 
  
  ###when more water is detected, the classification for water become even more greedy for assigning water
  if(gcc<0.365){    
    water[percent_water>0.6&r<90&g<110&b_g>0.58&b_r>0.62&r>0]<-1
  }
  ##pull out more dark greys if some water detected
  if(gcc>=0.365){    
    percent_water<-sum(water)/pixel_num*100 
    water[percent_water>0.6&r<125&g<135&b_g>0.65&b_r>0.62&r>0]<-1
    percent_water<-sum(water)/pixel_num*100 
  }
  
  ##pull out water that is blue-green light greys; water reflects sky on cloudy days
  if(gcc>=0.368|gcc<0.36){
    water[percent_water>0.3&percent_bluesky<30&b>62&b_g>0.62&b_r>0.72&r>0]<-1
  }
  if(gcc<0.368&gcc>0.355){
    water[percent_water>0.5&percent_bluesky<30&b>62&b_g>0.62&b_r>0.72&r>0]<-1
  }
  percent_water<-sum(water)/pixel_num*100 
  
  ##grab even more grey values, light as well as dark; veg is <0.86 b/r, so some veg values here
  ##get more grey greens than reds to target winter water vs winter dead veg; note however that summer water is red, not green
  water[percent_water>0.5&b_g>0.78&r<160&r>0]<-1
  percent_water<-sum(water)/pixel_num*100 
  
  ##if lots of water is present, use the most aggressive grey targeted water filter
  ##pull out all greys, including light and dark grey reds and greens;
  ##many of these overlap with possible veg pixels, especially in winter scenes, but in water scenes, almost all are water
  water[percent_water>5&g_r>0.87&g_r<1.03&
          b_g>0.68&b_r>0.68&r>0]<-1
  
  ###check for cloud shadows, which can be misclassed as water, especially in summer
  ###cloudshadow overlapps too much with water, so not using this; 
  ###inability to seprarte cloud shadows from water is a known fault, however images with 
  ###both cover types are eventually removed from the downstream phenophase analysis
  #cloudshadow[percent_clouds>70&water==1&bright<220&b_r<0.75&g_r<1
  #                  &r>0,1,0)
  #percent_cloudshadow<-sum(as.numeric(cloudshadow>0))/pixel_num*100
  
  ###remove cloudshadow from water class####
  #cloudshad<-array(dim=c(960,1296), data=0)  
  #avgmarshvalue<-(mean(bright[mask[,,1]>0]))/3
  #if(avgmarshvalue<79&percent_water>3&percent_clouds>70&(gmean/rmean)>0.95&(gmean/rmean)<1.05){
  #    cloudshad[water==1]<-1
  #}
  #if(gcc>=0.366&percent_water>3&percent_clouds>70&(as.POSIXlt(image_data$date_time[i])$mon+1)<5|(as.POSIXlt(image_data$date_time[i])$mon+1)>10){
  #        cloudshad[water==1]<-1
  #}
  #if(mean(g_r[g_r<10000])>0.75&percent_water>3&percent_clouds>70&(as.POSIXlt(image_data$date_time[i])$mon+1)>4&(as.POSIXlt(image_data$date_time[i])$mon+1)<11){
  #    cloudshad[water==1]<-1
  #}
  
  ##mask to pull out all images where r,g,b are all equal and image is greyscale
  #if(mean(g_r[g_r<1000])==mean(b_r[b_r<1000])&mean(b_r[b_r<1000])==mean(b_g[b_g<1000])){
  #    cloudshad[mask[,,1]>0]<-1
  #}
  #percent_cloudshad<-sum(cloudshad)/pixel_num*100 
  #water[cloudshad==1]<-0
  
  ##finalize water estimate
  percent_water<-sum(water)/pixel_num*100 
  
  ##finalize veg class, remove non veg pixels
  veg[water==1]<-0
  #veg[cloudshad==1]<-0
  veg[white==1]<-0
  veg[shadow==1]<-0
  percent_veg<-sum(veg)/pixel_num*100
  
  ##plot the veg image for error checking; not needed if satisfied all is working; 
  ##uncomment this line to try this
  #r[veg==0] <-0;g[veg==0] <-0;b[veg==0] <-0;reclass <- do.call(abind, c(list(r,g,b), along = 3)); par(mar=c(0,0,0,0));plot(imagedata(reclass))
  
  ##compile results
  results$marshpixels[i]<-pixel_num
  results$avgmarshvalue[i]<-(mean(bright[mask[,,1]>0]))/3
  results$percent_shadow[i]<-percent_shadow
  results$percent_veg[i]<-percent_veg
  results$percent_lowwhite[i]<-percent_white ##this is fog or other white objects in the veg ROI
  results$percent_water[i]<-percent_water
  #results$percent_cloudshad[i]<-percent_cloudshad
  results$skypixels[i]<-pixel_num.sky
  results$avgskyvalue[i]<-(mean(brightsky[skymask[,,1]>0]))/3
  results$percent_clouds[i]<-percent_clouds
  results$percent_bluesky[i]<-percent_bluesky
  results$avggcc[i]<-mean(g[g>0]/bright[g>0])
  results$avgexg[i]<-exg
  results$wfi[i]<-wfi
  results$bmean[i]<-bmean
  results$gmean[i]<-gmean
  results$rmean[i]<-rmean
  results$avg.g_r[i]<-mean(g_r[g_r<10000])
  
  ##write out only jpegs with high veg in ROI
  ##if you want to write out all photos, remove if statement
  if(percent_veg>98.1&percent_water<0.7&percent_white<0.7){
    ##make folders to put new files in
    setwd(outdir)
    year<-as.character(image_data[i,"year"]) ##i
    d<-paste(outdir, year, sep="/")
    if (file.exists(as.character(year))==FALSE){
      dir.create(as.character(year))}
    setwd(d)
    folder<-as.character(image_data[i,"month"]) ##i
    dd<-paste(d, folder, sep="/")
    if (file.exists(as.character(folder))==FALSE){
      dir.create(as.character(folder))
    }
    
    setwd(outdir)
    ##reclass rgb to "veg only", ie. color values only for veg pixels, 0 for non veg
    r[veg==0] <-0
    g[veg==0] <-0
    b[veg==0] <-0
    ##bind the matrices into a 3 level array; along =3 forces an array rather than a matrix
    reclass <- do.call(abind, c(list(r,g,b), along = 3))
    #plot(imagedata(reclass))
    ##write out the reclassed jpeg
    ##this writes into the year and month folders if the get image data function
    ##found your photos in year and month folders
    writeJpeg(as.character(image_data[i,"images"]), imagedata(reclass)) ##i
    
    
  }
  setwd(basedir)    
}

if(append.out==TRUE){
  out<-read.table("pixelclassifyresult.csv", as.is=TRUE, header=TRUE,sep=",")
  out$date_time<-as.POSIXct(strptime(out$date_time,tz="", "%Y-%m-%d %H:%M"))
  results<-rbind(out,results)
}

if(write.out==TRUE){
  write.table(results, "../pixelclassifyresult.csv", row.names=FALSE, sep=",")
}
