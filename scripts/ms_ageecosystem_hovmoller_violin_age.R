#============================================
#
# hovmoller plots ecosystem age
# ..regression of age by latitudinal bands
# ..violin plots of age by continent
# plots of Global Forest Age Database
# ..also, hovmöller plots for GFAD
# ..also, violin plots of age by contient
#
#============================================
Sys.setenv(TZ = "UTC")
#internal functions
wherenearest   <- function(val,matrix){
  dist = abs(matrix-val)
  index = which.min(dist)
  return( index )
}
fn_getvar      <- function(var,fpath){
	   t <- nc_open(fpath)
       v <- ncvar_get(t,var)
       nc_close(t)
       return(v)
}

library(ncdf4)
fdir.main = "~/VTFT_demography/" #or directory of input,output data

#get data
#..zonmean_age
dat.yfyl <- fn_getvar(var = 'zonmean_age',fpath = paste0(fdir.main,"/data/data_simulation/github_lpj_agems_prisec_luhv2/zonmean_meanAge_gridcell_prisec_18602017_plantonly_fpc_gtc_0d01.nc"))
dat.yfnl <- fn_getvar(var = 'zonmean_age',fpath = paste0(fdir.main,"/data/data_simulation/github_lpj_agems_prisec_noluc/zonmean_meanAge_gridcell_prisec_18602017_plantonly_fpc_gtc_0d01.nc"))
dat.nfyl <- fn_getvar(var = 'zonmean_age',fpath = paste0(fdir.main,"/data/data_simulation/github_lpj_agems_prisec_nofire_yesluc/zonmean_meanAge_gridcell_prisec_18602017_plantonly_fpc_gtc_0d01.nc"))
dat.lat  <- fn_getvar(var = 'lat',        fpath = paste0(fdir.main,"/data/data_simulation/github_lpj_agems_prisec_luhv2/zonmean_meanAge_gridcell_prisec_18602017_plantonly_fpc_gtc_0d01.nc"))

#put into dataframe
df  <- data.frame(age       =as.vector(dat.yfyl),
                  age.nluc  =as.vector(dat.yfnl),
                  age.nfire =as.vector(dat.nfyl),
                  lat       =rep(dat.lat,times=dim(dat.yfyl)[2]),
                  date      =rep(seq.Date(from = as.Date("1860-01-01"),
                                            to = as.Date("2017-01-01"),by = "1 year"),
                                 each=dim(dat.yfyl)[1]))
df <- df[which(df$date < as.Date("2016-01-01")),]

#add zonal band label
df$zonalband <- ifelse(df$lat >= -23.0 & df$lat <= 23.0, 'tropics',
                ifelse(df$lat >   23.0 & df$lat <  50.0, 'temperate',
                ifelse(df$lat >   50.0 & df$lat <= 90.0, 'boreal',NA)))

#color fn
#colfunc <-colorRampPalette(c("red","yellow","springgreen","royalblue","magenta"))
colfunc <-colorRampPalette(c("#ff0004","#ff3302","#ff6601","#ff9900","#ffbd00","#ffd300",
                             "#ffe900","#ffff00","#b6ff48","#6dff91","#24ffda","#00e6ff",
                             "#00b4ff","#0083ff","#0052ff"))
cut.seq = seq(5,150,10)
for(foldcode_hovmoller_colors in 1:1){
		#..........
		# .. yfyl
		#binned ages
		cutval  <- cut(x = df$age, breaks=cut.seq)
		#add colors to dataframe
		df$cols.yfyl <- colfunc(length(cut.seq))[cutval]
		#set age <5 to  values to red
		df$cols.yfyl[df$age <= 5] <- '#ff0004'
		#set age >150 to  values to blue
		df$cols.yfyl[df$age >= 145] <- '#0052ff'
		#set NA values to white
		df$cols.yfyl[is.na(df$cols.yfyl)] <- 'white'
		
		#..........
		# .. yfnl
		#binned ages
		cutval  <- cut(x = df$age.nluc, breaks=cut.seq)
		#add colors to dataframe
		df$cols.yfnl <- colfunc(length(cut.seq))[cutval]
		#set age <5 to  values to red
		df$cols.yfnl[df$age.nluc <= 5] <- '#ff0004'
		#set age >150 to  values to blue
		df$cols.yfnl[df$age.nluc >= 145] <- '#0052ff'
		#set NA values to white
		df$cols.yfnl[is.na(df$cols.yfnl)] <- 'white'
		
		#..........
		# .. nfyl
		#binned ages
		cutval  <- cut(x = df$age.nfire, breaks=cut.seq)
		#add colors to dataframe
		df$cols.nfyl <- colfunc(length(cut.seq))[cutval]
		#set age <5 to  values to red
		df$cols.nfyl[df$age.nfire <= 5] <- '#ff0004'
		#set age >150 to  values to blue
		df$cols.nfyl[df$age.nfire >= 145] <- '#0052ff'
		#set NA values to white
		df$cols.nfyl[is.na(df$cols.nfyl)] <- 'white'
		
		df <- df[which(df$date >= as.Date("1875-01-01")),]

}

#................
#   main plot
#................
MAKEPDF=TRUE
ADDGFAD=TRUE
for(foldcode_hovmoller_plots in 1:1){
	if(MAKEPDF==TRUE){
		if(ADDGFAD==TRUE){
			pdf(file = paste0(fdir.main,"/figures/figures_agemaps/hovmoller_ecosystemage_18602016_yfyl_wGFADv1.1.pdf"),
				width=14,height=4.5)
		}else{
			pdf(file = paste0(fdir.main,"/figures/figures_agemaps/hovmoller_ecosystemage_18602016_yfyl.pdf"),
				width=14,height=4.5)
		}
	}
	par(mar=c(0.5,2,1,0.5))
	if(ADDGFAD==FALSE){
		#try plotting
		plot.default(x = as.Date(df$date),y = df$lat,pch=95,col=df$cols.yfyl,cex=2,
			yaxt='n',xaxt='n',bty='n',cex.main=1.3,
			xlim=c(as.Date("1875-01-01"),as.Date("2016-01-01")),
			#main='Ecosystem Age Hovmoller Plot',
			main='',ylab='',xlab='')
		
		axis(side = 1,line = -3.5,cex=1.1,tick = TRUE,
             at = c(seq(as.Date("1875-01-01"),as.Date("2000-01-01"),by='25 years'),as.Date("2016-01-01")),
             labels=c(seq(1875,2000,by=25),2016))
		axis(side = 2,line = -1.9,cex=1.1,
             at = seq(-60,90,by=30),
             labels=seq(-60,90,by=30))
	}else if(ADDGFAD==TRUE){
		par(mar=c(0.5,2,1,1))
		#try plotting
		plot.default(x = as.Date(df$date),y = df$lat,pch=95,col=df$cols.yfyl,cex=2,
			yaxt='n',xaxt='n',bty='n',cex.main=1.3,
			xlim=c(as.Date("1875-01-01"),as.Date("2030-01-01")),
			#main='Ecosystem Age Hovmoller Plot',
			main='',ylab='',xlab='')

		#try plotting GFADv1.1
		points(x = rep(as.Date("2023-01-01"),times=length(df.gfad$lat)),
               y = df.gfad$lat,pch=95,
			   col = df.gfad$cols.age,cex=2)
		
		axis(side = 1,line = -3.5,cex=1.1,tick = TRUE,
             at = c(seq(as.Date("1875-01-01"),as.Date("2000-01-01"),by='25 years'),as.Date("2016-01-01")),
             labels=c(as.character(c(seq(1875,2000,by=25),2016))))
		axis(side = 1,line = -3.5,cex=1.1,tick = FALSE,
             at = as.Date("2025-01-01"),
             labels="GFADv1.1\nca. 2005")
		axis(side = 2,line = -1.9,cex=1.1,
             at = seq(-60,90,by=30),
             labels=seq(-60,90,by=30))
	}
	if(MAKEPDF==TRUE){dev.off()}
	
	#..yfnl
	if(MAKEPDF==TRUE){pdf(file = paste0(fdir.main,"/figures/figures_agemaps/hovmoller_ecosystemage_18602016_yfnl.pdf"),
		width=14,height=4.5)}
	par(mar=c(0.5,2,1,0.5))
	#try plotting
	plot.default(x = as.Date(df$date),y = df$lat,pch=95,col=df$cols.yfnl,cex=2,
		yaxt='n',xaxt='n',bty='n',cex.main=1.3,
		xlim=c(as.Date("1875-01-01"),as.Date("2016-01-01")),
		#main='Ecosystem Age Hovmoller Plot',
		main='',ylab='',xlab='')
	axis(side = 1,line = -3.5,cex=1.1,tick = TRUE,
				 at = c(seq(as.Date("1875-01-01"),as.Date("2000-01-01"),by='25 years'),as.Date("2016-01-01")),
				 labels=c(seq(1875,2000,by=25),2017))
	axis(side = 2,line = -1.9,cex=1.1,
				 at = seq(-60,90,by=30),
				 labels=seq(-60,90,by=30))
	if(MAKEPDF==TRUE){dev.off()}
	
	#..nfyl
	if(MAKEPDF==TRUE){pdf(file = paste0(fdir.main,"/figures/figures_agemaps/hovmoller_ecosystemage_18602016_nfyl.pdf"),
		width=14,height=4.5)}
	par(mar=c(0.5,2,1,0.5))
	#try plotting
	plot.default(x = as.Date(df$date),y = df$lat,pch=95,col=df$cols.nfyl,cex=2,
		yaxt='n',xaxt='n',bty='n',cex.main=1.3,
		xlim=c(as.Date("1875-01-01"),as.Date("2016-01-01")),
		#main='Ecosystem Age Hovmoller Plot',
		main='',ylab='',xlab='')
	axis(side = 1,line = -3.5,cex=1.1,tick = TRUE,
         at = c(seq(as.Date("1875-01-01"),as.Date("2000-01-01"),by='25 years'),as.Date("2016-01-01")),
         labels=c(seq(1875,2000,by=25),2017))
	axis(side = 2,line = -1.9,cex=1.1,
         at = seq(-60,90,by=30),
         labels=seq(-60,90,by=30))
	if(MAKEPDF==TRUE){dev.off()}
}

#.................
#    legend
#.................
# pdf(file = paste0(fdir.main,"/figures_agemaps/hovmoller_ecosystemage_18602014_legend.pdf"),
# 		width=5,height=5)
# par(mar=c(3,3,3,3))
# #add legend
# plot(x=0:40,y=0:40,col=NA,bty='n',xaxt='n',yaxt='n',ylab='',xlab='')
# points(x= rep(25,times=length(cut.seq)),y=seq(10,30,length.out = length(cut.seq)),cex=15,
# 	pch=95,col=colfunc(length(cut.seq)))
# text.default(x=rep(35,times=length(cut.seq)),y=seq(10,30,length.out = length(cut.seq)),labels = cut.seq)
# dev.off()

#----------------------------------------
# hovmöller simple regression age trend
#----------------------------------------
library(plyr)
df.trend  = df[which(!is.na(df$zonalband) & !is.na(df$age.nluc)),]

#get zonal band avg age
df.trend <- ddply(.data = df.trend,.variables = c('zonalband','date'),.fun = summarise,
										age.yfnl = mean(age.nluc),
										age.nfyl = mean(age.nfire),
										age.yfyl = mean(age))
df.trend <- df.trend[which(df.trend$date < as.Date("2016-01-01")),]

#sort by zonal band, then date
#..add sequence 1:157 for date
df.trend          <- df.trend[order(df.trend$zonalband,df.trend$date),]
#..get 1901 onwards
df.trend          = df.trend[which(df.trend$date >= as.Date("1901-01-01")),]
#unique date baseline as 1901
df.trend$date_ref = rep(1:length(unique(df.trend$date)), times=3)

#simple linear model 
#..only fire, both fire and luc
mod.lm1.bo <- lm(data = df.trend[which(df.trend$zonalband=='boreal'),],formula = age.yfnl ~ date_ref)
summary(mod.lm1.bo)
mod.lm1.te <- lm(data = df.trend[which(df.trend$zonalband=='temperate'),],formula = age.yfnl ~ date_ref)
summary(mod.lm1.te)
mod.lm1.tr <- lm(data = df.trend[which(df.trend$zonalband=='tropics'),],formula = age.yfnl ~ date_ref)
summary(mod.lm1.tr)

mod.lm2.bo <- lm(data = df.trend[which(df.trend$zonalband=='boreal'),],formula = age.nfyl ~ date_ref)
summary(mod.lm2.bo)
mod.lm2.te <- lm(data = df.trend[which(df.trend$zonalband=='temperate'),],formula = age.nfyl ~ date_ref)
summary(mod.lm2.te)
mod.lm2.tr <- lm(data = df.trend[which(df.trend$zonalband=='tropics'),],formula = age.nfyl ~ date_ref)
summary(mod.lm2.tr)

mod.lm3.bo <- lm(data = df.trend[which(df.trend$zonalband=='boreal'),],formula = age.yfyl ~ date_ref)
summary(mod.lm3.bo)
mod.lm3.te <- lm(data = df.trend[which(df.trend$zonalband=='temperate'),],formula = age.yfyl ~ date_ref)
summary(mod.lm3.te)
mod.lm3.tr <- lm(data = df.trend[which(df.trend$zonalband=='tropics'),],formula = age.yfyl ~ date_ref)
summary(mod.lm3.tr)

#plot trends
pdf(file = paste0(fdir.main,"/figures/figures_agemaps/hovmoller_trend_botetr.pdf"),
		width=6,height=5)
par(mfrow=c(1,1),mar=c(4,5,4,1))
#yes fire no luc
plot(x=df.trend[which(df.trend$zonalband=='boreal'),'date'],
	   y= mod.lm1.bo$coefficients[1] +  mod.lm1.bo$coefficients[2]*1:length(unique(df.trend$date)),
				col='darkorchid',type='l',lty=1,lwd=2,ylim=c(60,153),
			xaxt='n',
			ylab='Ecosystem Age',xlab='year',main='trend in ecosystem age yfnluc and yfyluc')
	axis(side = 1,cex=1.1,tick = TRUE,
				 at = c(seq(as.Date("1875-01-01"),as.Date("2000-01-01"),by='25 years'),as.Date("2016-01-01")),
				 labels=c(seq(1875,2000,by=25),2016))
lines(x=df.trend[which(df.trend$zonalband=='temperate'),'date'],
	   y= mod.lm1.te$coefficients[1] +  mod.lm1.te$coefficients[2]*1:length(unique(df.trend$date)),
				col='green',lty=1,lwd=2)
lines(x=df.trend[which(df.trend$zonalband=='tropics'),'date'],
	   y= mod.lm1.tr$coefficients[1] +  mod.lm1.tr$coefficients[2]*1:length(unique(df.trend$date)),
				col='red',lty=1,lwd=2)
#yes fire yes luc
lines(x=df.trend[which(df.trend$zonalband=='boreal'),'date'],
	   y= mod.lm3.bo$coefficients[1] +  mod.lm3.bo$coefficients[2]*1:length(unique(df.trend$date)),
				col='darkorchid',lty=2,lwd=2)
lines(x=df.trend[which(df.trend$zonalband=='temperate'),'date'],
	   y= mod.lm3.te$coefficients[1] +  mod.lm3.te$coefficients[2]*1:length(unique(df.trend$date)),
				col='green',lty=2,lwd=2)
lines(x=df.trend[which(df.trend$zonalband=='tropics'),'date'],
	   y= mod.lm3.tr$coefficients[1] +  mod.lm3.tr$coefficients[2]*1:length(unique(df.trend$date)),
				col='red',lty=2,lwd=2)
text(x=as.Date("1901-01-01"),y=df.trend[which(df.trend$zonalband=='boreal'),'age.yfnl'][1]+6,
	col='darkorchid',adj=0,labels='Boreal 50N-90N')
text(x=as.Date("1901-01-01"),y=df.trend[which(df.trend$zonalband=='temperate'),'age.yfnl'][1]+6,
	col='green',adj=0,labels='Temperate 23N-50N')
text(x=as.Date("1901-01-01"),y=100,
	col='red',adj=0,labels='Tropics 23S-23N')

# lines(x=c(as.Date("1901-01-01"),as.Date("1907-01-01")),y=c(60,60),col='darkorchid',lwd=2)
# lines(x=c(as.Date("1901-01-01"),as.Date("1907-01-01")),y=c(55,55),col='darkorchid',lwd=2,lty=2)
# lines(x=c(as.Date("1901-01-01"),as.Date("1907-01-01")),y=c(45,45),col='green',lwd=2)
# lines(x=c(as.Date("1901-01-01"),as.Date("1907-01-01")),y=c(40,40),col='green',lwd=2,lty=2)
# lines(x=c(as.Date("1901-01-01"),as.Date("1907-01-01")),y=c(30,30),col='red',lwd=2)
# lines(x=c(as.Date("1901-01-01"),as.Date("1907-01-01")),y=c(25,25),col='red',lwd=2,lty=2)

#..
dev.off()

# arrows(x0 = as.Date("1940-01-01"),x1 = as.Date("1955-01-01"),y0 = 133,y1 = 136,lty=1,length = 0.1,col='black')
# text(x=as.Date("1920-01-01"),y=130,cex=0.8,lty=3,
# 	col='black',adj=0,labels='Fire and LUCLM')
# arrows(x0 = as.Date("1990-01-01"),x1 = as.Date("2000-01-01"),y0 = 145,y1 = 141,lty=1,length = 0.1,col='black')
# text(x=as.Date("1980-01-01"),y=147,cex=0.8,
# 	col='black',adj=0,labels='Fire Only')

# #no fire yes luc
# lines(x=df.trend[which(df.trend$zonalband=='boreal'),'date'],
# 	   y= mod.lm2.bo$coefficients[1] +  mod.lm2.bo$coefficients[2]*1:length(unique(df.trend$date)),
# 				col='blue',lty=3,lwd=2)
# lines(x=df.trend[which(df.trend$zonalband=='temperate'),'date'],
# 	   y= mod.lm2.te$coefficients[1] +  mod.lm2.te$coefficients[2]*1:length(unique(df.trend$date)),
# 				col='blue',lty=3,lwd=2)
# lines(x=df.trend[which(df.trend$zonalband=='tropics'),'date'],
# 	   y= mod.lm2.tr$coefficients[1] +  mod.lm2.tr$coefficients[2]*1:length(unique(df.trend$date)),
# 				col='blue',lty=3,lwd=2)

#================================
# 2000s average age by continent
# ..violinplots
#================================
library(vioplot)

#get zonal time series data
#get data
for(foldcode_getdata_ageclass_continent in 1:1){
		# plant cover > 1% fpc all plants
		age.africa    <- fn_getvar(var = 'ageclass_prisec_frac',fpath = paste0(fdir.main,"/data/data_simulation/github_lpj_agems_prisec_luhv2/meanAge_gridcell_prisec_2000s_plantonly_fpc_gtc_0d01_continent_africa.nc"))
		age.asia      <- fn_getvar(var = 'ageclass_prisec_frac',fpath = paste0(fdir.main,"/data/data_simulation/github_lpj_agems_origresp_prisec_luhv2/meanAge_gridcell_prisec_2000s_plantonly_fpc_gtc_0d01_continent_asia.nc"))
		age.australia <- fn_getvar(var = 'ageclass_prisec_frac',fpath = paste0(fdir.main,"/data/data_simulation/github_lpj_agems_origresp_prisec_luhv2/meanAge_gridcell_prisec_2000s_plantonly_fpc_gtc_0d01_continent_australia.nc"))
		age.namer     <- fn_getvar(var = 'ageclass_prisec_frac',fpath = paste0(fdir.main,"/data/data_simulation/github_lpj_agems_origresp_prisec_luhv2/meanAge_gridcell_prisec_2000s_plantonly_fpc_gtc_0d01_continent_northamerica.nc"))
		age.samer     <- fn_getvar(var = 'ageclass_prisec_frac',fpath = paste0(fdir.main,"/data/data_simulation/github_lpj_agems_origresp_prisec_luhv2/meanAge_gridcell_prisec_2000s_plantonly_fpc_gtc_0d01_continent_southamerica.nc"))
		age.europe    <- fn_getvar(var = 'ageclass_prisec_frac',fpath = paste0(fdir.main,"/data/data_simulation/github_lpj_agems_origresp_prisec_luhv2/meanAge_gridcell_prisec_2000s_plantonly_fpc_gtc_0d01_continent_europe.nc"))

		age.africa <- as.vector(age.africa)
		age.africa <- age.africa[!is.na(age.africa)]
		
		age.asia <- as.vector(age.asia)
		age.asia <- age.asia[!is.na(age.asia)]
		
		age.australia <- as.vector(age.australia)
		age.australia <- age.australia[!is.na(age.australia)]
		
		age.namer <- as.vector(age.namer)
		age.namer <- age.namer[!is.na(age.namer)]
		
		age.samer <- as.vector(age.samer)
		age.samer <- age.samer[!is.na(age.samer)]
		
		age.europe <- as.vector(age.europe)
		age.europe <- age.europe[!is.na(age.europe)]
		
		# hist(c(age.africa, age.asia, age.australia,
		# 				age.namer,age.samer,age.europe))
	
}

pdf(file = paste0(fdir.main,"/figures/figures_agemaps/boxplots_ecosystemage_continent_lpj.pdf"),
		width=12,height=5)
par(mfrow=c(1,1),mar=c(4,4,3,1))
# set up empty plot
plot(0:1,0:1,type="n",xlim=c(0.5,6.5),ylim=c(0,160),
     axes=FALSE,ann=FALSE)
vioplot(age.africa, age.asia, age.australia, age.namer,age.samer,age.europe,
	 add=TRUE,
	 names=c("Africa","Asia","Australia","Europe","\nNorth\nAmerica","\nSouth\nAmerica"), 
     col=c("forestgreen"))
axis(side=3,at=3.5,tick=FALSE,labels="LPJ Ageclass Simulation")
axis(side=1,at=1:4,labels=c("Africa","Asia","Australia","Europe"))
axis(side=1,at=5:6,line = 0,labels=c("",""))
axis(side=1,at=5:6,line = 1,tick = FALSE,labels=c("North\nAmerica","South\nAmerica"))
axis(side=2,at= seq(0,151,by=25),cex.axis=0.95,labels= seq(0,151,by=25))
axis(side=2,at= 75,tick=FALSE,line = 1.5,labels= 'Age (years)')
text(x = 1:6,y=rep(157,times=6),
	labels=c(paste0("n=",length(age.africa)),
             paste0("n=",length(age.asia)),
             paste0("n=",length(age.australia)),
             paste0("n=",length(age.namer)),
             paste0("n=",length(age.samer)),
             paste0("n=",length(age.europe)))
	)
#.
dev.off()

#..............................
# plot gfad data violin plots

ls.tsum    = list() ; counter=1
for(foldcode_get_gfad in 1:1){
		#get data
		#from ben poulter gfadv1 inventory
		f.path = paste0(fdir.main,"/data/data_ancillary/GFAD_V1-1_ageonly_invertlat.nc")
		t.gfad     <- fn_getvar(var = "age", fpath = f.path) #dims lon lat pft_frac ageclass
		t.gfad.lat <- fn_getvar(var = "lat", fpath = f.path) #dims lon lat pft_frac ageclass
		t.gfad.lon <- fn_getvar(var = "lon", fpath = f.path) #dims lon lat pft_frac ageclass
		
		#just get the ageclass data
		#sum age fraction over pfts
		#average the median age for gridcell
		t.gfad.age <- array(data = rep(NA,times=720*360),dim = c(720,360))
		median.age = c(5,15,25,35,45,55,65,75,85,95,105,115,125,135,145)
		for(i in 1:720){
			for(j in 1:360){
				t.sum = 0; t.val = 0 
				# #.......................................
				# # adjusted mean
				# # ..correct for pft fractional cover
				# #.......................................
				for(k in 1:15){
					#take the average age of forested lands only
					#..by making fractional values relative to total fraction of pfts
					t.sum <- t.sum + sum(t.gfad[i,j,1:4,k])
				}
				#print(t.sum)
				ls.tsum[[counter]] <- t.sum
				
                for(k in 1:15){
                    #take the average age of forested lands only
                    #..by making fractional values relative to total fraction of pfts
                    t.val <- t.val + (median.age[k]*sum(t.gfad[i,j,1:4,k])/t.sum)
                }

				t.gfad.age[i,j] <- t.val
				counter          = counter+1
			}
		}
		
		#set 0 to NA
		t.gfad.age[t.gfad.age==0] <- -9999
		#set NA to -9999
		t.gfad.age[is.na(t.gfad.age)]  <- -9999
		t.gfad.age[is.nan(t.gfad.age)] <- -9999
		# rotate <- function(x) t(apply(x, 1, rev))
		# image(rotate(t.gfad.age))
		#save to file
		for(foldcode_write_gfvad_ncdf in 1:1){
			#   t.time            = ncdim_def("time", units="years since 2005-01-01", vals=0, calendar="standard",unlim=TRUE)
			# 	t.lon							= ncdim_def("lon", paste("degrees_east",sep=''), t.gfad.lon,create_dimvar=TRUE)
			# 	t.lat							= ncdim_def("lat", paste("degrees_north", sep=''), t.gfad.lat,create_dimvar=TRUE)
			#   variable.def.ncdf = ncvar_def(name = "mean_age", units = "years",dim =  list(t.lon,t.lat,t.time),missval = -9999, longname="mean age of grid-cell")
			# 
			#   #................
			#   #create ncdf
			#   #................
			#   global.nc <- nc_create(filename = paste0(fdir.main,"/data/data_ancillary/GFAD_V1-1_ageonly_invertlat_meanage.nc"),
			#   	variable.def.ncdf)
			# 
			#   #Assign global attributes
			#   ncatt_put(global.nc, 0, "Description", "Global Forest Age Dataset (GFADv1.1)")
			#   ncatt_put(global.nc, 0, "Notes.1", "Average age of grid-cell based on weighted average over all PFT classes, divided by total PFT fractional cover to correct for forested area")
			#   ncatt_put(global.nc, 0, "Contact GFADv1.1", "Benjamin Poulter NASA GSFC")
			#   ncatt_put(global.nc, 0, "Contact processed data", "Leonardo Calle leonardo.calle@montana.edu")
			#   ncvar_put(global.nc, variable.def.ncdf, t.gfad.age)
			#   nc_close(global.nc)
		}#..end write
		
		#get transcom mask 
		transcom <- fn_getvar(var = 'transcom_region',fpath = paste0(fdir.main,"/data/data_ancillary/transcom_24regions_05deg_continent.nc"))

		#create masks
		mask = transcom
		mask[mask != 1] <- NA
		mask[mask == 1] <- 1
        t.gfad.age.namer <- as.vector(mask*t.gfad.age)
        t.gfad.age.namer <- t.gfad.age.namer[!is.na(t.gfad.age.namer)]

        mask = transcom
            mask[mask != 2] <- NA
            mask[mask == 2] <- 1
        t.gfad.age.samer <- as.vector(mask*t.gfad.age)
        t.gfad.age.samer <- t.gfad.age.samer[!is.na(t.gfad.age.samer)]

        mask = transcom
            mask[mask != 3] <- NA
            mask[mask == 3] <- 1
        t.gfad.age.europe <- as.vector(mask*t.gfad.age)
        t.gfad.age.europe <- t.gfad.age.europe[!is.na(t.gfad.age.europe)]

        mask = transcom
            mask[mask != 4] <- NA
            mask[mask == 4] <- 1
        t.gfad.age.africa <- as.vector(mask*t.gfad.age)
        t.gfad.age.africa <- t.gfad.age.africa[!is.na(t.gfad.age.africa)]

        mask = transcom
            mask[mask != 5] <- NA
            mask[mask == 5] <- 1
        t.gfad.age.asia <- as.vector(mask*t.gfad.age)
        t.gfad.age.asia <- t.gfad.age.asia[!is.na(t.gfad.age.asia)]

        mask = transcom
            mask[mask != 6] <- NA
            mask[mask == 6] <- 1
        t.gfad.age.australia <- as.vector(mask*t.gfad.age)
        t.gfad.age.australia <- t.gfad.age.australia[!is.na(t.gfad.age.australia)]
}

# set up empty plot
plot(0:1,0:1,type="n",xlim=c(0.5,6.5),ylim=c(0,151),
     axes=FALSE,ann=FALSE)
vioplot(t.gfad.age.africa,t.gfad.age.asia,t.gfad.age.australia,
		t.gfad.age.europe,t.gfad.age.namer, t.gfad.age.samer,
	 add=TRUE,
	 names=c("Africa","Asia","Australia","Europe","\nNorth\nAmerica","\nSouth\nAmerica"), 
     col=c("forestgreen"))
axis(side=3,at=3.5,tick=FALSE,labels="GFADv1.1")
axis(side=1,at=1:4,labels=c("Africa","Asia","Australia","Europe"))
axis(side=1,at=5:6,line = 0,labels=c("",""))
axis(side=1,at=5:6,line = 1,tick = FALSE,labels=c("North\nAmerica","South\nAmerica"))
axis(side=2,at= seq(0,151,by=25),cex.axis=0.95,labels= seq(0,151,by=25))
axis(side=2,at= 75,tick=FALSE,line = 1.5,labels= 'Age (years)')
#
# dev.off()

#...................................................
# cumulative contribution by age for each continent
fn_addageclass <- function(t.df){
				#add ageclass
				t.df$ageclass <- ifelse(t.df$age >=  1 & t.df$age <= 10, 1,
                                 ifelse(t.df$age > 10 & t.df$age <= 20, 2,
                                 ifelse(t.df$age > 20 & t.df$age <= 30, 3,
                                 ifelse(t.df$age > 30 & t.df$age <= 40, 4,
                                 ifelse(t.df$age > 40 & t.df$age <= 50, 5,
                                 ifelse(t.df$age > 50 & t.df$age <= 60, 6,
                                 ifelse(t.df$age > 60 & t.df$age <= 70, 7,
                                 ifelse(t.df$age > 70 & t.df$age <= 80, 8,
                                 ifelse(t.df$age > 80 & t.df$age <= 90, 9,
                                 ifelse(t.df$age > 90 & t.df$age <= 100, 10,
                                 ifelse(t.df$age > 100 & t.df$age <= 150, 11,
                                 ifelse(t.df$age > 150, 12,
                                 ifelse(t.df$age == 0, 'zero','NA')))))))))))))
				#sum by ageclass
				t.df <- plyr::ddply(.data = t.df,.variables = c("ageclass"),.fun = plyr::summarise,
												sum_gridcells = length(ageclass))
				t.df$ageclass.f <- factor(t.df$ageclass,levels = 1:12)
				#sort by ageclass 
				t.df         = t.df[order(t.df$ageclass.f),]
	
				return(t.df)
}
df.africa = data.frame(age = age.africa)
df.africa = fn_addageclass(df.africa)
df.asia = data.frame(age = age.asia)
df.asia = fn_addageclass(df.asia)
df.australia = data.frame(age = age.australia)
df.australia = fn_addageclass(df.australia)
df.europe = data.frame(age = age.europe)
df.europe = fn_addageclass(df.europe)
df.namer = data.frame(age = age.namer)
df.namer = fn_addageclass(df.namer)
df.samer = data.frame(age = age.samer)
df.samer = fn_addageclass(df.samer)

pdf(file = paste0(fdir.main,"/figures/figures_agemaps/cumsum_ageclass_continent.pdf"),
		width=6,height=5)
par(mfrow=c(1,1))
plot(x=1:12,y=cumsum(df.africa$sum_gridcells)/sum(df.africa$sum_gridcells),col='blue',
     ylim=c(0,1),xaxt='n',
     xlab='ageclass',ylab='cumulative fractional area of continent',
     type='l',lwd=2)
lines(x=1:12,y=cumsum(df.asia$sum_gridcells)/sum(df.asia$sum_gridcells),col='green',lwd=2)
lines(x=1:12,y=cumsum(df.australia$sum_gridcells)/sum(df.australia$sum_gridcells),col='goldenrod1',lwd=2)
lines(x=1:12,y=cumsum(df.europe$sum_gridcells)/sum(df.europe$sum_gridcells),col='magenta',lwd=2)
lines(x=1:12,y=cumsum(df.namer$sum_gridcells)/sum(df.namer$sum_gridcells),col='cyan',lwd=2)
lines(x=1:12,y=cumsum(df.samer$sum_gridcells)/sum(df.samer$sum_gridcells),col='purple',lwd=2)
axis(side = 1, at=1:12,labels = 1:12,tick = TRUE)
lines(x=c(1,1.3),y=c(1.0,1.0), col='blue',lwd=3)
text(x=c(1.5),y=c(1.0,1.0), adj=0, labels='africa')
lines(x=c(1,1.3),y=c(0.90,0.90), col='green',lwd=3)
text(x=c(1.5),y=c(0.90,0.90), adj=0, labels='asia')
lines(x=c(1,1.3),y=c(0.80,0.80), col='goldenrod1',lwd=3)
text(x=c(1.5),y=c(0.80,0.80), adj=0, labels='australia')
lines(x=c(1,1.3),y=c(0.70,0.70), col='magenta',lwd=3)
text(x=c(1.5),y=c(0.70,0.70), adj=0, labels='europe')
lines(x=c(1,1.3),y=c(0.60,0.60), col='cyan',lwd=3)
text(x=c(1.5),y=c(0.60,0.60), adj=0, labels='north america')
lines(x=c(1,1.3),y=c(0.50,0.50), col='purple',lwd=3)
text(x=c(1.5),y=c(0.50,0.50), adj=0, labels='south america')
#.
dev.off()

#==================================
# GFADv1.1 Hovmöller plot (1 year)
# ..effectively stacked bar
#==================================
dat.age <- fn_getvar(var = 'mean_age',fpath = paste0(fdir.main,"/data/data_ancillary/zonmean_GFAD_V1-1_ageonly_invertlat_meanage.nc"))
dat.lat <- fn_getvar(var = 'lat',fpath = paste0(fdir.main,"/data/data_ancillary/zonmean_GFAD_V1-1_ageonly_invertlat_meanage.nc"))

#put into dataframe
df.gfad <- data.frame(age=dat.age,lat=dat.lat)

#color fn
#colfunc <-colorRampPalette(c("red","yellow","springgreen","royalblue","magenta"))
colfunc <-colorRampPalette(c("#ff0004","#ff3302","#ff6601","#ff9900","#ffbd00","#ffd300",
                             "#ffe900","#ffff00","#b6ff48","#6dff91","#24ffda","#00e6ff",
                             "#00b4ff","#0083ff","#0052ff"))
cut.seq = seq(5,150,10)
for(foldcode_hovmoller_colors in 1:1){
		#.......................
		# GFAD mean age colors
	    #.......................
		#binned ages
		cutval  <- cut(x = df.gfad$age, breaks=cut.seq)
		#add colors to dataframe
		df.gfad$cols.age <- colfunc(length(cut.seq))[cutval]
		#set age <5 to  values to red
		df.gfad$cols.age[df.gfad$age <= 5] <- '#ff0004'
		#set age >150 to  values to blue
		df.gfad$cols.age[df.gfad$age >= 145] <- '#0052ff'
		#set NA values to white
		df.gfad$cols.age[is.na(df.gfad$cols.age)] <- 'white'
}


#----------------------
# make plot
	par(mar=c(0.5,2,1,0.5))
	#try plotting
	plot.default(x = rep(1,times=length(df.gfad$lat)),y = df.gfad$lat,pch=95,col=df.gfad$cols.age,cex=2,
		yaxt='n',xaxt='n',bty='n',cex.main=1.3,
		xlim=c(0.9,1.1),
		#main='Ecosystem Age Hovmoller Plot',
		main='',ylab='',xlab='')
	axis(side = 1,line = -5.5,cex=1.1,tick = FALSE,
				 at = 1,
				 labels="ca. 2005")
	axis(side = 2,line = -9.5,cex=1.1,
				 at = seq(-60,90,by=30),
				 labels=seq(-60,90,by=30))
