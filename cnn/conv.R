rm(list = ls())
library(calibrate)

x=c(0.36,3.6,2.7,-1.3,3.034,0.73,-0.063)
y=c(2.76,-1.35,3.03,1.5,1.4,1.41,0.67)
z=c(1:7)

setwd("C:/Users/Murthy/Desktop/RegBatch/Philips/Day13/DiscConv/")
mypath<-file.path(getwd(),
                  paste("convolution lag",0,".jpg",sep="_"))
jpeg(file=mypath)
plot(x,type="l",xlim=c(1,14),
     ylim=c(-2,5),lwd =1,col=2 )
textxy(X=c(1:7),Y=x,labs=x,
       cex=0.9,col=2)
lines(x=z,y=y,col=4,lwd=3)
textxy(X=c(1:7),Y=y,labs=y,cex=0.95,col=4)
dev.off()

for(i in 0:6){
  mypath<-file.path(getwd(),paste("convolution lag",i,".jpg",sep="_"))
  jpeg(file=mypath)
  plot(x,type="l",xlim=c(1,14),ylim=c(-2,5),lwd=1,col=2)
  textxy(X=c(1:7),Y=x,labs=x,cex=0.9,col=2)
  lines(x=z+i,y=y,col=4,lwd=3)
  textxy(X=c(1:7),Y=y,labs=y,cex=0.95,col=4,offset=i)
  dev.off()
}




source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")
# install.packages('jpeg')
library(jpeg)
library(EBImage)


image <- readJPEG('lena.jpg')
##GRAY SCALING##
image <- (image[,,1]+image[,,2]+
            image[,,3])/3


#FILTERS
horizontal_filter <- matrix(c(1, 1, 1, 0, 0, 0, -1, -1, -1), ncol = 3, byrow = T)
horizontal_filter
vertical_filter <- matrix(c(1, 1, 1, 0, 0, 0, -1, -1, -1), ncol = 3, byrow = F)
vertical_filter

#CROSS-CORRELATION
new_im_horizontal <- flop(rotate(filter2(image, horizontal_filter), 90))
new_im_vertical <- flop(rotate(filter2(image, vertical_filter), 90))

##DISPLAY
display(new_im_vertical)
display(new_im_horizontal)

##SAVE
writeImage(new_im_vertical, 'output vertical edge detection.jpg')
writeImage(new_im_horizontal, 'output horizontal edge detection.jpg')
