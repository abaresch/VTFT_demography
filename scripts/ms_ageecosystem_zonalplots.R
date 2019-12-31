#================================
#     latitudinal plots
# ..vegc,soilc,gpp,rh,ra
#================================
library(ncdf4)
fn_getvar <- function(var,fpath){
       t <- nc_open(fpath)
       v <- ncvar_get(t,var)
       nc_close(t)
       return(v)
}
#NOTE: averages for years 2000-2010
fdir.main = "~/Downloads/" #or directory of input,output data

#---------------------------------------------
# get data
#---------------------------------------------
fdir      = paste0(fdir.main,"/simulation_data/zonal_data/")
for(foldcode_getdata in 1:1){
	jung.mte.lat   <- fn_getvar('lat',paste0(fdir.main,"/ancillary_data/zonmean_jung_gppGL_mul86400_muldpm_yearsum_2000avg_kgCm2yr.nc"))
	jung.mte.gpp   <- fn_getvar('gpp',paste0(fdir.main,"/ancillary_data/zonmean_jung_gppGL_mul86400_muldpm_yearsum_2000avg_kgCm2yr.nc"))

	jung2.mte.lat   <- fn_getvar('lat',paste0(fdir.main,"/ancillary_data/zonmean_jung_reco_ilambproduct_2000avg_kgm2yr.nc"))
	jung2.mte.reco  <- fn_getvar('reco',paste0(fdir.main,"/ancillary_data/zonmean_jung_reco_ilambproduct_2000avg_kgm2yr.nc"))

	globbiomass.lat   <- fn_getvar('lat',paste0(fdir.main,"/ancillary_data/zonmean_GlobBiomass_AGC_kgCm2_zeromiss.nc"))
	globbiomass.meankg   <- fn_getvar('AGC',paste0(fdir.main,"/ancillary_data/zonmean_GlobBiomass_AGC_kgCm2_zeromiss.nc"))
	globbiomass.sumPg    <- fn_getvar('AGC',paste0(fdir.main,"/ancillary_data/zonsum_GlobBiomass_AGC_PgC_zeromiss.nc"))

	avit.agc.lat   <- fn_getvar('lat',paste0(fdir.main,"/ancillary_data/zonmean_Avitabile_AGC_kgCm2.nc"))
	avit.agc.meankg  <- fn_getvar('AGC',paste0(fdir.main,"/ancillary_data/zonmean_Avitabile_AGC_kgCm2.nc"))
	avit.agc.sumPg   <- fn_getvar('cell_area',paste0(fdir.main,"/ancillary_data/zonsum_Avitabile_AGC_PgC.nc"))

	fao.soil.lat      <- fn_getvar('lat',paste0(fdir.main,"/ancillary_data/zonmean_FAO_soils_carbon_kgCm2.nc"))
	fao.soil.meankg   <- fn_getvar('soilc',paste0(fdir.main,"/ancillary_data/zonmean_FAO_soils_carbon_kgCm2.nc"))
	fao.soil.sumPg    <- fn_getvar('soilc',paste0(fdir.main,"/ancillary_data/zonsum_FAO_soils_carbon_PgC.nc"))
	for(folcode_binsoilbylat05deg in 1:1){
		t <- nc_open(paste0(fdir.main,"/ancillary_data/zonsum_FAO_soils_carbon_1kmres_PgC.nc"))
		t.dat <- ncvar_get(t,'GSOCmapV1.2.0')
		t.lat <- ncvar_get(t,'lat')
		nc_close(t)
		#create data frame
		df.fao          <- data.frame(lat=t.lat,soilc=t.dat)
		
		#create the latitude bins 0.5 deg
		df.fao$latbin   <- .bincode(df.fao$lat, breaks = seq(-89.75,89.75,by=0.5), right = TRUE, include.lowest = FALSE)
		v.seq           <- seq(-89.75,89.75,by=0.5)
		df.fao$lat05deg <- NULL
		for(i in 1:nrow(df.fao)){
			df.fao$lat05deg[i] <- v.seq[df.fao$latbin[i]]
		}
		
    #now summarize by 0.5 deg latitude bins	
		df.fao2 <- plyr::ddply(.data = df.fao,.variables = c('lat05deg'),plyr::summarise,
									soilc =ifelse(is.na(sum(soilc,na.rm=TRUE)),"NA",sum(soilc,na.rm=TRUE)))
		df.fao2$lat05deg <- as.numeric(df.fao2$lat05deg)
		df.fao2 <- df.fao2[order(df.fao2$lat05deg),]
	
	}#..end bin fao soil

	lpj.lat   <- fn_getvar('lat',paste0(fdir,"/zonmean_origresp_noage_noluc_timmean_20002010_vegc.nc"))
	t.var = "vegc"
	lpj.vegc.mean.noage.noluc   <- fn_getvar(t.var,paste0(fdir,"/zonmean_origresp_noage_noluc_timmean_20002010_",t.var,".nc"))
	lpj.vegc.mean.noage.yesluc  <- fn_getvar(t.var,paste0(fdir,"/zonmean_origresp_noage_yesluc_timmean_20002010_",t.var,".nc"))
  lpj.vegc.mean.yesage.noluc  <- fn_getvar(t.var,paste0(fdir,"/zonmean_origresp_yesage_noluc_timmean_20002010_",t.var,".nc"))
	lpj.vegc.mean.yesage.yesluc <- fn_getvar(t.var,paste0(fdir,"/zonmean_origresp_yesage_yesluc_timmean_20002010_",t.var,".nc"))
  #NOTE: sums converted here from kg in Pg by dividing 1e12 (see right of fn_getvar)
	lpj.vegc.sum.noage.noluc   <- fn_getvar(t.var,paste0(fdir,"/zonsum_origresp_noage_noluc_timmean_20002010_",t.var,".nc"))/1e12
	lpj.vegc.sum.noage.yesluc  <- fn_getvar(t.var,paste0(fdir,"/zonsum_origresp_noage_yesluc_timmean_20002010_",t.var,".nc"))/1e12
  lpj.vegc.sum.yesage.noluc  <- fn_getvar(t.var,paste0(fdir,"/zonsum_origresp_yesage_noluc_timmean_20002010_",t.var,".nc"))/1e12
	lpj.vegc.sum.yesage.yesluc <- fn_getvar(t.var,paste0(fdir,"/zonsum_origresp_yesage_yesluc_timmean_20002010_",t.var,".nc"))/1e12

	t.var = "soilc"
	lpj.soilc.mean.noage.noluc   <- fn_getvar(t.var,paste0(fdir,"/zonmean_origresp_noage_noluc_timmean_20002010_",t.var,".nc"))
	lpj.soilc.mean.noage.yesluc  <- fn_getvar(t.var,paste0(fdir,"/zonmean_origresp_noage_yesluc_timmean_20002010_",t.var,".nc"))
  lpj.soilc.mean.yesage.noluc  <- fn_getvar(t.var,paste0(fdir,"/zonmean_origresp_yesage_noluc_timmean_20002010_",t.var,".nc"))
	lpj.soilc.mean.yesage.yesluc <- fn_getvar(t.var,paste0(fdir,"/zonmean_origresp_yesage_yesluc_timmean_20002010_",t.var,".nc"))
	lpj.soilc.sum.noage.noluc   <- fn_getvar(t.var,paste0(fdir,"/zonsum_origresp_noage_noluc_timmean_20002010_",t.var,".nc"))/1e12
	lpj.soilc.sum.noage.yesluc  <- fn_getvar(t.var,paste0(fdir,"/zonsum_origresp_noage_yesluc_timmean_20002010_",t.var,".nc"))/1e12
  lpj.soilc.sum.yesage.noluc  <- fn_getvar(t.var,paste0(fdir,"/zonsum_origresp_yesage_noluc_timmean_20002010_",t.var,".nc"))/1e12
	lpj.soilc.sum.yesage.yesluc <- fn_getvar(t.var,paste0(fdir,"/zonsum_origresp_yesage_yesluc_timmean_20002010_",t.var,".nc"))/1e12

	t.var = "litc"
	lpj.litc.mean.noage.noluc   <- fn_getvar(t.var,paste0(fdir,"/zonmean_origresp_noage_noluc_timmean_20002010_",t.var,".nc"))
	lpj.litc.mean.noage.yesluc  <- fn_getvar(t.var,paste0(fdir,"/zonmean_origresp_noage_yesluc_timmean_20002010_",t.var,".nc"))
  lpj.litc.mean.yesage.noluc  <- fn_getvar(t.var,paste0(fdir,"/zonmean_origresp_yesage_noluc_timmean_20002010_",t.var,".nc"))
	lpj.litc.mean.yesage.yesluc <- fn_getvar(t.var,paste0(fdir,"/zonmean_origresp_yesage_yesluc_timmean_20002010_",t.var,".nc"))
	lpj.litc.sum.noage.noluc   <- fn_getvar(t.var,paste0(fdir,"/zonsum_origresp_noage_noluc_timmean_20002010_",t.var,".nc"))/1e12
	lpj.litc.sum.noage.yesluc  <- fn_getvar(t.var,paste0(fdir,"/zonsum_origresp_noage_yesluc_timmean_20002010_",t.var,".nc"))/1e12
  lpj.litc.sum.yesage.noluc  <- fn_getvar(t.var,paste0(fdir,"/zonsum_origresp_yesage_noluc_timmean_20002010_",t.var,".nc"))/1e12
	lpj.litc.sum.yesage.yesluc <- fn_getvar(t.var,paste0(fdir,"/zonsum_origresp_yesage_yesluc_timmean_20002010_",t.var,".nc"))/1e12

}

#---------------------------------------------
# make zonal plots
#---------------------------------------------
pdf(file = paste0(fdir.main,"figures_zonal/zonal_plots_carbonstocks.pdf"),
	width=16,height=10)
par(mfrow=c(2,3),mar=c(4.5,4.5,3,0.5))
for(foldcode_latplots_stocks in 1:1){
  #.............
	#vegc
	x.max = max(lpj.vegc.mean.noage.noluc,lpj.vegc.mean.noage.yesluc,
                lpj.vegc.mean.yesage.noluc,lpj.vegc.mean.yesage.yesluc,
                na.rm=TRUE)
	plot(y=lpj.lat, x=lpj.vegc.mean.noage.noluc, type='l',lwd=1,lty=1,
		xlim=c(0,x.max),
		main="Vegetation C Density",col='grey75',
		cex.lab=c.lab,
		ylab='Latitude',
		yaxt='n',
		xlab=expression(kg~C~m^-2))
	axis(side = 2,at = seq(-90,90,by=30),labels= seq(-90,90,by=30))	 
	lines(y=lpj.lat, x=lpj.vegc.mean.noage.yesluc, col='black',lty=1)
	lines(y=lpj.lat, x=lpj.vegc.mean.yesage.noluc, col='darkslategray1',lty=1)
	lines(y=lpj.lat, x=lpj.vegc.mean.yesage.yesluc, col='dodgerblue',lty=1)

	lines(y=globbiomass.lat, x=globbiomass.meankg, col='goldenrod1',lty=3,lwd=2)
	lines(y=avit.agc.lat, x=avit.agc.meankg, col='red',lty=3,lwd=2)
	
	#Legend
	lines(x=c(0,0.75),y=c(-70,-70),lwd=2,col='grey75',lty=1)
		text(x=c(1),y=-70,adj=0,labels='LPJ-wsl no Age no  LUC',cex=cex.name)
	lines(x=c(0,0.75),y=c(-80,-80),lwd=2,lty=1,col='black')
		text(x=c(1),y=-80,adj=0,labels='LPJ-wsl no Age yes LUC',cex=cex.name)
	lines(x=c(8,8.75),y=c(-70,-70),lwd=2,lty=1,col='darkslategray1')
		text(x=c(9),y=-70,adj=0,labels='LPJ-wsl yes Age no  LUC',cex=cex.name)
	lines(x=c(8,8.75),y=c(-80,-80),lwd=2,lty=1,col='dodgerblue')
		text(x=c(9),y=-80,adj=0,labels='LPJ-wsl yes Age yes LUC',cex=cex.name)
	
	lines(x=c(5,5.75),y=c(40,40),lwd=2,col='goldenrod1',lty=2)
		text(x=c(6),y=40,adj=0,labels='GlobBiomass Wood AGC',cex=cex.name)
	lines(x=c(5,5.75),y=c(30,30),lwd=2,col='red',lty=2)
		text(x=c(6),y=30,adj=0,labels='Avitabile Wood AGC',cex=cex.name)
	
	#.............
	#soilc density
	x.max = max(lpj.soilc.mean.noage.noluc,lpj.soilc.mean.noage.yesluc,
                lpj.soilc.mean.yesage.noluc,lpj.soilc.mean.yesage.yesluc,
				na.rm=TRUE)
	plot(y=lpj.lat, x=lpj.soilc.mean.noage.noluc, type='l',lwd=1,lty=1,
		xlim=c(0,x.max),
		main="Soil C Density",col='grey75',
		cex.lab=c.lab,
		ylab='Latitude',
		yaxt='n',
		xlab=expression(kg~C~m^-2))
	axis(side = 2,at = seq(-90,90,by=30),labels= seq(-90,90,by=30))	 
	lines(y=lpj.lat, x=lpj.soilc.mean.noage.yesluc, col='black',lty=1)
	lines(y=lpj.lat, x=lpj.soilc.mean.yesage.noluc, col='darkslategray1',lty=1)
	lines(y=lpj.lat, x=lpj.soilc.mean.yesage.yesluc, col='dodgerblue',lty=1)

	lines(y=fao.soil.lat, x=fao.soil.meankg, col='red',lty=3,lwd=2)
	lines(x=c(5,7),y=c(-80,-80),lwd=2,col='red',lty=3)
		text(x=c(8),y=-80,adj=0,labels='FAO Soils 2018',cex=cex.name)

  #.............
	#litterc density
	x.max = max(lpj.litc.mean.noage.noluc,lpj.litc.mean.noage.yesluc,
                lpj.litc.mean.yesage.noluc,lpj.litc.mean.yesage.yesluc,
				na.rm=TRUE)
	plot(y=lpj.lat, x=lpj.litc.mean.noage.noluc, type='l',lwd=1,lty=1,
		xlim=c(0,x.max),
		main="Litter C Density",col='grey75',
		cex.lab=c.lab,
		ylab='Latitude',
		yaxt='n',
		xlab=expression(kg~C~m^-2))
	axis(side = 2,at = seq(-90,90,by=30),labels= seq(-90,90,by=30))	 
	lines(y=lpj.lat, x=lpj.litc.mean.noage.yesluc, col='black',lty=1)
	lines(y=lpj.lat, x=lpj.litc.mean.yesage.noluc, col='darkslategray1',lty=1)
	lines(y=lpj.lat, x=lpj.litc.mean.yesage.yesluc, col='dodgerblue',lty=1)

	#...................................
	#total stocks (PgC) by latitude
	#vegc stocks
	x.max = max(0.7*lpj.vegc.sum.noage.noluc,0.7*lpj.vegc.sum.noage.yesluc,
                0.7*lpj.vegc.sum.yesage.noluc,0.7*lpj.vegc.sum.yesage.yesluc,
                globbiomass.sumPg,avit.agc.sumPg,
                na.rm=TRUE)
	plot(y=lpj.lat, x=0.7*lpj.vegc.sum.noage.noluc, type='l',lwd=1,lty=1,
		xlim=c(0,x.max),
		main="Vegetation C Stocks",col='grey75',
		cex.lab=c.lab,
		ylab='Latitude',
		yaxt='n',
		xlab=expression(Pg~C~~"in 5"~degree~~"Latitudinal bins"))
	axis(side = 2,at = seq(-90,90,by=30),labels= seq(-90,90,by=30))	 
	lines(y=lpj.lat, x=0.7*lpj.vegc.sum.noage.yesluc, col='black',lty=1)
	lines(y=lpj.lat, x=0.7*lpj.vegc.sum.yesage.noluc, col='darkslategray1',lty=1)
	lines(y=lpj.lat, x=0.7*lpj.vegc.sum.yesage.yesluc, col='dodgerblue',lty=1)

	lines(y=globbiomass.lat, x=globbiomass.sumPg, col='goldenrod1',lty=3,lwd=2)
	lines(y=avit.agc.lat, x=avit.agc.sumPg, col='red',lty=3,lwd=2)
	lines(x=c(1,1.5),y=c(-50,-50),lwd=2,col='goldenrod1',lty=2)
		text(x=c(1.75),y=-50,adj=0,labels='GlobBiomass Wood AGC',cex=cex.name)
	lines(x=c(1,1.5),y=c(-70,-70),lwd=2,col='red',lty=2)
		text(x=c(1.75),y=-70,adj=0,labels='Avitabile Wood AGC',cex=cex.name)

	#..............
	#soilc stocks
	x.max = max(lpj.soilc.sum.noage.noluc,lpj.soilc.sum.noage.yesluc,
                lpj.soilc.sum.yesage.noluc,lpj.soilc.sum.yesage.yesluc,
				na.rm=TRUE)
	plot(y=lpj.lat, x=lpj.soilc.sum.noage.noluc, type='l',lwd=1,lty=1,
		xlim=c(0,x.max),
		main="Soil C Stocks",col='grey75',
		cex.lab=c.lab,
		ylab='Latitude',
		yaxt='n',
		xlab=expression(Pg~C~~"in 5"~degree~~"Latitudinal bins"))
	axis(side = 2,at = seq(-90,90,by=30),labels= seq(-90,90,by=30))	 
	lines(y=lpj.lat, x=lpj.soilc.sum.noage.yesluc, col='black',lty=1)
	lines(y=lpj.lat, x=lpj.soilc.sum.yesage.noluc, col='darkslategray1',lty=1)
	lines(y=lpj.lat, x=lpj.soilc.sum.yesage.yesluc, col='dodgerblue',lty=1)

	lines(y=fao.soil.lat, x=fao.soil.sumPg, col='red',lty=3,lwd=2)
	lines(x=c(5,7),y=c(-60,-60),lwd=2,col='red',lty=3)
		text(x=c(8),y=-60,adj=0,labels='FAO Soils 2018',cex=cex.name)

	#..............
	#litterc stocks
	x.max = max(lpj.litc.sum.noage.noluc,lpj.litc.sum.noage.yesluc,
                lpj.litc.sum.yesage.noluc,lpj.litc.sum.yesage.yesluc,
				na.rm=TRUE)
	plot(y=lpj.lat, x=lpj.litc.sum.noage.noluc, type='l',lwd=1,lty=1,
		xlim=c(0,x.max),
		main="Litter C Stocks",col='grey75',
		cex.lab=c.lab,
		ylab='Latitude',
		yaxt='n',
		xlab=expression(Pg~C~~"in 5"~degree~~"Latitudinal bins"))
	axis(side = 2,at = seq(-90,90,by=30),labels= seq(-90,90,by=30))	 
	lines(y=lpj.lat, x=lpj.litc.sum.noage.yesluc, col='black',lty=1)
	lines(y=lpj.lat, x=lpj.litc.sum.yesage.noluc, col='darkslategray1',lty=1)
	lines(y=lpj.lat, x=lpj.litc.sum.yesage.yesluc, col='dodgerblue',lty=1)
}
dev.off()
