setwd("C:/Users/user/Google Drive/UCL/class/GEOGG125 Principles of Spatial Analysis/housePrice")
library(ggplot2)
library("rgdal")
library("rgeos")
library(sp)
library("tmap")
library(automap)
library(grid)

############# functions ########################################
tmhpplot = function(spclass,colname,
                    comp=FALSE,scale=FALSE,legend=FALSE,
                    title = ""){
  plot = tm_shape(price)+
    tm_fill(col = colname, palette = "Reds", 
            style = "fixed",
            breaks = c(0,0.71,0.87,1.00,1.22,2.00),
            legend.show = legend,
            title = title)+
    tm_borders(col = "black",alpha = 0.5)+
    tm_layout(legend.position = c("right", "bottom"), 
              frame = FALSE)+
    tm_legend(bg.color = "white", bg.alpha=.6)
  if(comp){
    plot = plot+tm_compass(position = c("left","bottom"))
  }
  if(scale){
    plot = plot+tm_scale_bar(position = c("left","bottom"))
  }
  return(plot)
}
fourplots = function(plot1,plot2,plot3,plot4){
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(2,2)))
  print(plot1, vp=viewport(layout.pos.col = 1,layout.pos.row = 1))
  print(plot2, vp=viewport(layout.pos.col = 2,layout.pos.row = 1))
  print(plot3, vp=viewport(layout.pos.col = 1,layout.pos.row = 2))
  print(plot4, vp=viewport(layout.pos.col = 2,layout.pos.row = 2))
}

################################################################
################################################################

priceData = read.csv("Land-Registry-Data-London-2015.csv",
                     header = TRUE, stringsAsFactors=FALSE )
priceData = priceData[,c(1,2,3,8,9)]
House.points = 
  SpatialPointsDataFrame(priceData[,c(4,5)],
      priceData,proj4string = CRS("+init=EPSG:27700"))
Output.Areas<- readOGR("Ward", "London_Ward_CityMerged")
proj4string(Output.Areas) = CRS("+init=EPSG:27700")
#Boroughs<- readOGR("Borough", "LondonBoroughs")
#proj4string(Boroughs) = CRS("+init=EPSG:27700")
### boxplot ###
ybreaks <- c(10000,100000,1000000,10000000)
xlabel <-c("Detached", "Flats/Maisonettes","Semi-Detached","Terraced")
ggplot(
  priceData,                # set dataframe
  aes (x = reorder(Type, Price, FUN=median),y = Price)
)+ geom_boxplot()+ xlab("Type")+ ylab("Price(?)") + 
  scale_y_log10(breaks=ybreaks,labels=ybreaks)+# y axes as log scale
  scale_x_discrete(breaks=c("D","F","S","T"),labels=xlabel)
### histogram ###
par(mfrow = c(1,4))
hist(log10(priceData[priceData[,3]=="F",2]),xlab ="Flat/Maisonettes",xlim=c(4,7),ylim=c(0,20000),main="",ylab = "Frequency",breaks = 16)
hist(log10(priceData[priceData[,3]=="T",2]),xlab ="Terraced",xlim=c(4,7),ylim=c(0,20000),main="",ylab = "",breaks = 25)
hist(log10(priceData[priceData[,3]=="S",2]),xlab ="Semi-Detached",xlim=c(4,7),ylim=c(0,20000),main="",ylab = "",breaks = 16)
hist(log10(priceData[priceData[,3]=="D",2]),xlab ="Detached",xlim=c(4,7),ylim=c(0,20000),main="",ylab = "",breaks = 16)


### point plot ###

plot1 = tm_shape(House.points[House.points@data[,3]=="F",]) + 
  tm_dots(col = "Price", palette = "Reds", style = "fixed",
          breaks = c(400,230000,309000,400000,575000,37000000),
          title = "Flat/Maisonette:Price(?)",
          legend.show = FALSE)+
  tm_shape(Output.Areas)+
  tm_borders(col = "black",alpha = 0.2)+
  tm_layout(legend.position = c("left", "bottom"), frame = FALSE)
plot2 = tm_shape(House.points[House.points@data[,3]=="T",]) + 
  tm_dots(col = "Price", palette = "Reds", style = "fixed",
          breaks = c(400,230000,309000,400000,575000,37000000),
          title = "Terraced:Price(?)",
          legend.show = FALSE)+
  tm_shape(Output.Areas)+
  tm_borders(col = "black",alpha = 0.2)+
  tm_layout(legend.position = c("left", "bottom"), frame = FALSE)
plot3 = tm_shape(House.points[House.points@data[,3]=="S",]) + 
  tm_dots(col = "Price", palette = "Reds", style = "fixed",
          breaks = c(400,230000,309000,400000,575000,37000000),
          title = "Semi-Detached:Price(?)",
          legend.show = FALSE)+
  tm_shape(Output.Areas)+
  tm_borders(col = "black",alpha = 0.2)+
  tm_layout(legend.position = c("left", "bottom"), frame = FALSE)
plot4 = tm_shape(House.points[House.points@data[,3]=="D",])+
  tm_dots(col = "Price", palette = "Reds", style = "fixed",
          breaks = c(400,230000,309000,400000,575000,37000000),
          title = "Price(?)")+
  tm_legend(bg.color = "white", bg.alpha=.6)+
  tm_shape(Output.Areas)+
  tm_borders(col = "black",alpha = 0.2)+
  tm_layout(legend.position = c("left", "bottom"), frame = FALSE)+
  tm_compass()+tm_scale_bar()

grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))
print(plot1, vp=viewport(layout.pos.col = 1,layout.pos.row = 1))
print(plot2, vp=viewport(layout.pos.col = 2,layout.pos.row = 1))
print(plot3, vp=viewport(layout.pos.col = 1,layout.pos.row = 2))
print(plot4, vp=viewport(layout.pos.col = 2,layout.pos.row = 2))


### kriging ###
price = Output.Areas
# create grid
grid <-spsample(House.points, type = 'regular', n = 10000)
# repeat over 4 types
for(type in c("F","T","S","D")){
# use 2000 samples from pricedata
  pd = priceData[priceData[,3] == type,]
  srow = sample(1:nrow(pd), size=2000, replace = FALSE)
  SampledPrice = pd[srow,]
  House.sampled = SpatialPointsDataFrame(SampledPrice[,c(4,5)],SampledPrice,
                                         proj4string = CRS("+init=EPSG:27700"))
# interpolate the price
  kriging_result = autoKrige(Price~1, House.sampled, grid)
  kg = kriging_result$krige_output

### standardrised price ratio
  hp = SpatialPointsDataFrame(pd[,c(4,5)],pd,
                              proj4string = CRS("+init=EPSG:27700"))
  pip <- over(hp, Output.Areas)
  hp@data = cbind(hp@data,pip)
  priceObs = aggregate(hp@data$Price,
                       by = list(hp@data$GSS_CODE),mean)
  names(priceObs) = c("GSS_CODE",paste("priceObs",type,sep = "."))

  pip = over(kg, Output.Areas)
  kg@data = cbind(kg@data,pip)
  smooth = aggregate(kg@data$var1.pred, 
                     by = list(kg@data$GSS_CODE),mean)
  names(smooth) = c("GSS_CODE",paste("priceStd",type,sep = "."))
  price = merge(price,priceObs,by.x="GSS_CODE",by.y="GSS_CODE",all.x=TRUE)
  price = merge(price,smooth,by.x="GSS_CODE",by.y="GSS_CODE",all.x=TRUE)
}

price@data = transform(price@data,
                       ratioF = price$priceObs.F/price$priceStd.F)
price@data = transform(price@data,
                       ratioT = price$priceObs.T/price$priceStd.T)
price@data = transform(price@data,
                       ratioS = price$priceObs.S/price$priceStd.S)
price@data = transform(price@data,
                       ratioD = price$priceObs.D/price$priceStd.D)

plots1 = tm_shape(price) + 
  tm_fill(col = "priceStd.F", palette = "Reds", 
          style = "fixed",
          breaks = c(0,300000,600000,950000,1800000,3600000),
          legend.show = FALSE)+
  tm_borders(col = "black",alpha = 0.5)+
  tm_layout(frame = FALSE)
plots2 = tm_shape(price) + 
  tm_fill(col = "priceStd.T", palette = "Reds", 
          style = "fixed",
          breaks = c(0,300000,600000,950000,1800000,3600000),
          legend.show = FALSE)+
  tm_borders(col = "black",alpha = 0.5)+
  tm_layout(frame = FALSE)
plots3 = tm_shape(price) + 
  tm_fill(col = "priceStd.S", palette = "Reds", 
          style = "fixed",
          breaks = c(0,300000,600000,950000,1800000,3600000),
          legend.show = FALSE)+
  tm_borders(col = "black",alpha = 0.5)+
  tm_layout(frame = FALSE)+
  tm_scale_bar(position = c("left","bottom"))+
  tm_compass(position = c("left","bottom")) 
plots4 = tm_shape(price) + 
  tm_fill(col = "priceStd.D", palette = "Reds", 
          style = "fixed",
          breaks = c(0,300000,600000,950000,1800000,3600000),
          title = "Expected House Price(?)")+
  tm_borders(col = "black",alpha = 0.5)+
  tm_layout(legend.position = c("right", "bottom"), 
            frame = FALSE)+
  tm_legend(bg.color = "white", bg.alpha=.6)
fourplots(plot1,plot2,plot3,plot4)



# writeOGR(price, dsn = "Interpolation", 
#          layer =  "price", driver="ESRI Shapefile")
# price = readOGR(dsn = "Interpolation",  layer =  "price")
# tm_shape(kg) +
#   tm_dots(col = "var1.pred", palette = "Reds",
#           style = "quantile",size = 0.2)+
#  tm_shape(Output.Areas[which(Output.Areas@data$GSS_CODE == "E05000144"),])+
#    tm_borders(col = "black",alpha = 0.7)
# 
# tm_shape(price) +
#   tm_fill(col = "ratioF", palette = "Reds",
#           style = "jenks",title = "ratio")+
#   tm_borders(col = "black",alpha = 0.5)+
#   tm_layout(frame = FALSE)

fourplots(tmhpplot(price,"ratioF"),tmhpplot(price,"ratioT"),
          tmhpplot(price,"ratioS",comp =TRUE,scale=TRUE),
          tmhpplot(price,"ratioD",legend=TRUE,
                   title = "Standardised Price Ratio"))

library(rstan)
dat<-list(N=nrow(price@data),xi=price$priceStd.F,obs=price$priceObs.F)
d.fit<-stan(file='hpstan.stan',data=dat,iter=2000,chains=4)
theta = rstan::extract(d.fit,pars="theta")[[1]]
isHot = rep(0,ncol(theta))
for (i in 1:ncol(theta)){
  if(quantile(theta[,i],0.1) >1.0){
    isHot[i] = 1
  }
}
polyModel1 = price
polyModel1@data = data.frame(spot = isHot)
tm_shape(polyModel1) + 
  tm_fill(col = "spot", palette = "Reds", style = "jenks",title = "hotspot")+
  tm_borders(col = "black",alpha = 0.5)
### map estimator
mapEstimate <- function(z){
  density(z)$x[which.max(density(z)$y)]
}
### spatial interaction
#adjascency matrix
library(spdep)
pnb <- poly2nb(price)
plot(price, border="grey")
plot(pnb, centroid@coords, add=TRUE)

from = 0
to = 0
for(i in 1:length(pnb)){
  for(j in pnb[[i]]){
    if(i<j){
      from = c(from,i)
      to = c(to,j)
    }
  }
}
from = from[-1]
to = to[-1]
#### create dataset for STAN #############
ratio = price$ratioD
for(i in 1:length(ratio)){
  if(ratio[i]<=0) ratio[i] = NA
}
numNotNA = sum(!is.na(ratio))
NotNA = which(!is.na(ratio))
ratio[which(is.na(ratio))] = 0
dat<-list(N=nrow(price@data),
          Ra=ratio,I=length(from),From=from,To=to,O=numNotNA,NotNA=NotNA)
d.fit<-stan(file='hsptstan.stan',data=dat,iter=2000,chains=4)
theta = rstan::extract(d.fit,pars="theta")[[1]]
isHot = rep(0,ncol(theta))
for (i in 1:ncol(theta)){
  isHot[i] = mapEstimate(theta[,i])
}
polyModel1 = price
polyModel1@data = data.frame(spot = isHot)
plot4 = tm_shape(polyModel1) + 
  tm_fill(col = "spot", palette = "Reds",
          style = "fixed",title = "SPR",
          breaks = c(0.5,0.8,0.9,1.0,1.1,1.2,1.3)
          )+
  tm_borders(col = "black",alpha = 0.5)+
  tm_layout(legend.position = c("right", "bottom"), 
            frame = FALSE)+
  tm_legend(bg.color = "white", bg.alpha=.6)

fourplots(plot1,plot2,plot3,plot4)
# calculate the centroid of boroughs
# centroid = gCentroid(Output.Areas, byid = TRUE)
# proj4string(centroid) = CRS("+init=EPSG:27700")
# centroid = SpatialPointsDataFrame(centroid@coords,Output.Areas@data)
# centroid = merge(centroid,price@data,by.x="GSS_CODE",by.y="GSS_CODE")
# sm_ratio = lowess(centroid$ratioF)
# centroid@data = transform(centroid,sm = sm_ratio$y)
# stanmodel <- stan_model(file='hsptstan.stan')
# d.fit = sampling(stanmodel,data = dat, iter=5200, warmup=200, 
#                  thin=5, seed=1234,control = list(adapt_delta = 0.8),
#                  init = function(){
#                    list(theta = price$ratioF,s_Y=3)
#                  })

## spatial hotspot
dat<-list(N=nrow(price@data),xi=price$priceStd,obs=price$priceObs,
          I=length(from),from=from,to=to,U=0.1)
d2.fit<-stan(file='hsp2stan.stan',data=dat,iter=2000,chains=4)

thet = rstan::extract(d2.fit,pars="theta")[[1]]
isHot = rep(0,ncol(thet))
for (i in 1:ncol(thet)){
  if(quantile(thet[,i],0.5) >1.0){
    isHot[i] = 1
  }
  #isHot[i] = mean(theta[,i])
}
polyModel1 = price
polyModel1@data = data.frame(spot = isHot)
tm_shape(polyModel1) + 
  tm_fill(col = "spot", palette = "Reds", style = "quantile",title = "hotspot")+
  tm_borders(col = "black",alpha = 0.5)
