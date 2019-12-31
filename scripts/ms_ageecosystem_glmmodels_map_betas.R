#===============================================
#
#  Global Maps of Beta Coefficients
#
# -- glm model 
#  ..Y ~ precip + temp + factor(ageclass)
#  .. .., where Y is NEP,NPP,Rh
#  .. .., and ageclass is 1:12
#  .. .., 10yr agewidth simulation
#
#  ..data annual from 2000-2017
#  ..glm results and R2
#
# -- objectives
#  ..determine prediction power
#  ..and spatial patterns of
#  .. ..precip,temp,age effects 
#===============================================

library(raster)
library(ncdf4)
library(ggplot2)
library(gridExtra)

fdir.main = "~/Downloads/" #or directory of main folder

#--------------------------------------
# ancillary functions
#--------------------------------------
fn_heatscatter <- function(x,y){
	  t.df      <- data.frame(x=x,y=y)
  	## Use densCols() output to get density at each point
		t.x       <- densCols(x = t.df$x, y=t.df$y,colramp=colorRampPalette(c("black", "white")))
		t.df$dens <- col2rgb(t.x)[1,] + 1L
		
		## Map densities to colors
		cols      <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
		t.df$cols <- cols[t.df$dens]
		
		return(t.df)
}
#simplifies code for retrieving netcdf variables
fn_getvar <- function(var,fpath){
	            t <- nc_open(fpath)
              v <- ncvar_get(t,var)
       nc_close(t)
       return(v)
}

#===============
#  get data
#  ..all data
#===============
#..rbind
# start, end ids of databases
t.st.vec  = c(1,6,11,16,21,26)
t.end.vec = c(5,10,15,20,25,30)

PRISEC.vec  = c('pri','sec')
modflux.vec = c('nep','npp','rh')
stattype    = 'stat_output_nointc'  #stat_output or stat_output_nointc
counter=1
ls.df.aic <- ls.df.glm <- ls.df.prd <- list()
for(PRISEC in PRISEC.vec){
	#prisec
	for(modflux in modflux.vec){
        #modflux
        for(i in 1:length(t.st.vec)){
            print(paste0(PRISEC,' ',modflux,' ',i))
            t.aic  <- readRDS(file = paste0(fdir.main,"/DATA_analysis/glm_models/",stattype,"/",PRISEC,"_",modflux,"_table_aic_",t.st.vec[i],"_",t.end.vec[i],".RDS"))
            t.glm  <- readRDS(file = paste0(fdir.main,"/DATA_analysis/glm_models/",stattype,"/",PRISEC,"_",modflux,"_table_paramtopmodel_",t.st.vec[i],"_",t.end.vec[i],".RDS"))
            t.prd  <- readRDS(file = paste0(fdir.main,"/DATA_analysis/glm_models/",stattype,"/",PRISEC,"_",modflux,"_table_predictions_",t.st.vec[i],"_",t.end.vec[i],".RDS"))
            
            t.aic$prisec  <- PRISEC
            t.glm$prisec  <- PRISEC
            t.prd$prisec  <- PRISEC
            
            t.aic$modflux <- modflux
            t.glm$modflux <- modflux
            t.prd$modflux <- modflux
            
            ls.df.aic[[counter]] <- t.aic
            ls.df.glm[[counter]] <- t.glm
            ls.df.prd[[counter]] <- t.prd
            #update counter
            counter=counter+1
        }
	}#..end modflux loop
}#..end prisec loop

#rbind
df.aic <- do.call('rbind',ls.df.aic)
df.glm <- do.call('rbind',ls.df.glm)
df.prd <- do.call('rbind',ls.df.prd)

#drop NAs
df.aic = df.aic[which(!is.na(df.aic$lon)),]
df.glm = df.glm[which(!is.na(df.glm$lon)),]
df.prd = df.prd[which(!is.na(df.prd$lon)),]

#=====================
# descriptive plots
#=====================
PRISEC ='pri'
modflux='npp'
#.............
# histograms
#.............
df.glm2 <- df.glm[which(df.glm$prisec==PRISEC & df.glm$modflux=modflux),]
par(mfrow=c(4,4))
hist(df.aic$R2,main='R2 for glms by grid-cell',xlab='R2')
t.var = "prec"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ',t.var),xlab='beta coef.')
t.var = "temp"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ',t.var),xlab='beta coef.')

#intercept is first factor, ageclass_1
t.var = "(Intercept)"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ','as.factor(ageclass_code)1'),xlab='beta coef.')
t.var = "as.factor(ageclass_code)2"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ',t.var),xlab='beta coef.')
t.var = "as.factor(ageclass_code)3"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ',t.var),xlab='beta coef.')
t.var = "as.factor(ageclass_code)4"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ',t.var),xlab='beta coef.')
t.var = "as.factor(ageclass_code)5"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ',t.var),xlab='beta coef.')
t.var = "as.factor(ageclass_code)6"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ',t.var),xlab='beta coef.')
t.var = "as.factor(ageclass_code)7"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ',t.var),xlab='beta coef.')
t.var = "as.factor(ageclass_code)8"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ',t.var),xlab='beta coef.')
t.var = "as.factor(ageclass_code)9"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ',t.var),xlab='beta coef.')
t.var = "as.factor(ageclass_code)10"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ',t.var),xlab='beta coef.')
t.var = "as.factor(ageclass_code)11"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ',t.var),xlab='beta coef.')
t.var = "as.factor(ageclass_code)12"
hist(df.glm2[which(df.glm2$varname==t.var & df.glm2$Pval <= 0.05),'B'],main=paste0('betas ',t.var),xlab='beta coef.')

#...................
#  heat map
#  ..global maps
#...................
par(mfrow=c(1,2))
makeplot     = TRUE
counter      = 1
ls.hexplots <- list()
PRISEC.vec   = c('pri','sec')
modflux.vec  = c('npp','rh')
stattype     = 'stat_nointc'  #stat_nointc

for(PRISEC in PRISEC.vec){
	for(modflux in modflux.vec){
		  #subset
			df.prd.sub <- df.prd[which(df.prd$prisec==PRISEC & df.prd$modflux==modflux),]
			
			df.gg <- data.frame(x= df.prd.sub$glm, y=df.prd.sub$lpj)
			z.main='Global'
		
			#drop NAs
			df.gg <- df.gg[which(!is.na(df.gg$x) & !is.na(df.gg$y)),]
			x.range <- range(c(df.gg$x,df.gg$y),na.rm=T)
			y.range <- range(c(df.gg$x,df.gg$y),na.rm=T)
			
			if(modflux=='nep'){
					z.tit.flux <- 'NEP'
			}else if(modflux=='npp'){
					z.tit.flux <- 'NPP'
			}else if(modflux=='rh'){
					z.tit.flux <- 'Rh'
			}
			if(PRISEC=='pri'){
				z.main.ps <- paste0('Primary Stands ', z.tit.flux)
			}else if(PRISEC=='sec'){
				z.main.ps <- paste0('Secondary Stands ', z.tit.flux)
			}
		  
			print(PRISEC)
			print(modflux)
			print(summary(lm(df.gg$y ~ df.gg$x)))
			ls.hexplots[[counter]] <- ggplot(data=df.gg,aes(x = x, y = y)) + 
					stat_binhex(aes(fill=log10(..count..)),bins=30) +
				  	viridis::scale_fill_viridis(limits  = c(0, 4.5),na.value='yellow',name='log10\n(count)\n') + 
				  	theme_bw() + 
		  			geom_abline(intercept = 0,slope = 0, col='black') +
		  			geom_abline(intercept = 0,slope = 1, col='red') +
		  	        # coord_cartesian(ylim = y.range, xlim= x.range)	+
				  	labs(title=paste0(z.main,' ', z.tit.flux,' ',z.main.ps),
		              y= ifelse(counter %in% c(1,2),paste0("LPJ Simulation ",z.tit.flux," (kgC/m2)"),""),
		              x= ifelse(counter %in% c(2,4),paste0("GLM Model ",z.tit.flux," (kgC/m2)"),"")) +
		              # y= paste0("LPJ Simulation ",z.tit.flux," (kgC/m2)"),
		              # x= paste0("GLM Model ",z.tit.flux," (kgC/m2)")) +
		    		theme(plot.title = element_text(hjust=0.5),
			          legend.text=element_text(size=rel(0.9)),
		    		  legend.title=element_text(size=rel(0.9)),
			          axis.title.y=element_text(hjust=0.5,size=rel(1.2)),
			          axis.title.x=element_text(hjust=0.5,size=rel(1.2)),
			          axis.text.y=element_text(size=rel(1.2)),
			          axis.text.x=element_text(size=rel(1.2))) +
		  	        theme(plot.margin = unit(c(1,1,1,1), "lines")) + 
		  			 theme(legend.position=ifelse(counter %in% c(3,4),'right','none'))
			#update counter
		    counter = counter+1
	}
}#..end prisec global 

if(makeplot==TRUE){
		t.modflux = paste0('_',modflux)
		if(modflux=='nep'){
			pdf(file = paste0(fdir.main,"/DATA_analysis/figures_models/prisec_glm_gridcell_predictions_global_ageclassmodel_standardized_",stattype,t.modflux,".pdf"),
			width=12,height=5)
			
			grid.arrange(
			 grobs         = ls.hexplots[c(1:2)],
			 layout_matrix = rbind(c(1,2))
			)					
		}else{
			pdf(file = paste0(fdir.main,"/DATA_analysis/figures_models/prisec_glm_gridcell_predictions_global_ageclassmodel_standardized_",stattype,t.modflux,".pdf"),
			width=12,height=10)
	
			grid.arrange(
			  grobs         =ls.hexplots[c(1:4)],
			  layout_matrix = rbind(c(1,3),c(2,4))
			)		
		}
	
}
if(makeplot==TRUE){dev.off()}

#...................
#  heat map
#  ..by zonal band
#...................
par(mfrow=c(1,3))

PRISEC       = 'pri'
modflux      = 'npp'
stattype     = 'stat_nointc'  #stat_nointc

makeplot     = TRUE
counter      = 1
ls.hexplots <- list()

df.prd2 <- df.prd[which(df.prd$prisec==PRISEC & df.prd$modflux==modflux),] 
for(z in 1:3){
	if(z==1){
		 t.p = df.prd2[which(df.prd2$lat >= 50 & df.prd2$lat <= 90),]
		 z.main='Boreal 50N to 90N'
	}else if(z==2){
		 t.p = df.prd2[which(df.prd2$lat >= 23 & df.prd2$lat <= 50),]
		 z.main='Temperate 23N to 50N'
	}else if(z==3){
		 t.p = df.prd2[which(df.prd2$lat >= -23 & df.prd2$lat <= 23),]
		 z.main='Tropics 23S to 23N'
	}
	df.gg <- data.frame(x= t.p$glm, y=t.p$lpj)

	#drop NAs
	df.gg <- df.gg[which(!is.na(df.gg$x) & !is.na(df.gg$y)),]
	x.range <- range(c(df.gg$x,df.gg$y))
	y.range <- range(c(df.gg$x,df.gg$y))
	
	if(modflux=='nep'){
			z.tit.flux <- 'NEP'
	}else if(modflux=='npp'){
			z.tit.flux <- 'NPP'
	}else if(modflux=='rh'){
			z.tit.flux <- 'Rh'
	}
	if(PRISEC=='pri'){
		z.main.ps <- 'Primary Stands'
	}else if(PRISEC=='sec'){
		z.main.ps <- 'Secondary Stands'
	}

	ls.hexplots[[counter]] <- ggplot(data=df.gg,aes(x = x, y = y)) + 
			stat_binhex(aes(fill=log10(..count..)),bins=30) +
		  	viridis::scale_fill_viridis(limits  = c(0, 4.5),na.value='yellow',name='log10\n(count)\n') + 
		  	theme_bw() + 
  			geom_abline(intercept = 0,slope = 0, col='black') +
  			geom_abline(intercept = 0,slope = 1, col='red') +
  	        coord_cartesian(ylim = y.range, xlim= x.range)	+
		  	labs(title=paste0(z.main,' ', z.tit.flux,' ',z.main.ps),
              # y= ifelse(counter %in% seq(1,36,by=3),paste0("LPJ Simulation ",modflux," (kgC/m2)"),""),
              # x= ifelse(counter %in% c(seq(7,36,by=9),seq(8,36,by=9),seq(9,36,by=9)),paste0("GLM Model ",modflux," (kgC/m2)"),"")) +
              y= paste0("LPJ Simulation ",z.tit.flux," (kgC/m2)"),
              x= paste0("GLM Model ",z.tit.flux," (kgC/m2)")) +
    		theme(plot.title = element_text(hjust=0.5),
	          legend.text=element_text(size=rel(0.9)),
    		  legend.title=element_text(size=rel(0.9)),
	          axis.title.y=element_text(hjust=0.5,size=rel(1.2)),
	          axis.title.x=element_text(hjust=0.5,size=rel(1.2)),
	          axis.text.y=element_text(size=rel(1.2)),
	          axis.text.x=element_text(size=rel(1.2))) +
  	        theme(plot.margin = unit(c(1,1,1,1), "lines")) + 
  			theme(legend.position=ifelse(counter %in% seq(3,36,by=3),'right','none'))
	#update counter
    counter = counter+1
}#..end z loop

if(makeplot==TRUE){
		t.modflux = paste0('_',modflux)
		pdf(file = paste0(fdir.main,"/DATA_analysis/figures_models/",PRISEC,"_glm_gridcell_predictions_zonalband_ageclassmodel_standardized_",stattype,t.modflux,".pdf"),
		width=18,height=5)
		
				grid.arrange(
				  grobs=	ls.hexplots[c(1:3)],
				  layout_matrix = rbind(c(1, 2, 3))
				)			
}
if(makeplot==TRUE){dev.off()}

#=================================================
#  convert to maps
#  ..betas
#  
#  NOTE: 
#  ..betas for ageclass 1:12
#  ..actual value, not relative to intercept
#  .. ..but when only one age-class in grid-cell,
#  .. ..intercept term is the age-class mean 
#  ..requires translating intercept to age-class
#  
#=================================================
#unique(df.glm$varname)
z.vars   = c("prec","temp","(Intercept)","as.factor(ageclass_code)1","as.factor(ageclass_code)2","as.factor(ageclass_code)3","as.factor(ageclass_code)4","as.factor(ageclass_code)5","as.factor(ageclass_code)6","as.factor(ageclass_code)7","as.factor(ageclass_code)8","as.factor(ageclass_code)9","as.factor(ageclass_code)10","as.factor(ageclass_code)11","as.factor(ageclass_code)12")
z.long   = c('precipitation','temperature','intercept','ageclass_1','ageclass_2','ageclass_3','ageclass_4','ageclass_5','ageclass_6','ageclass_7','ageclass_8','ageclass_9','ageclass_10','ageclass_11','ageclass_12')
z.unit   = rep('unitless',times=length(z.long))
f.out    = paste0(fdir.main,"/DATA_analysis/glm_models/netcdf_nointc/")
stattype = 'stat_nointc'  #stat_nointc

PRISEC.vec  = c('pri','sec')
modflux.vec = c('npp','rh')
for(PRISEC in PRISEC.vec){
	for(modflux in modflux.vec){
			print(paste0(PRISEC, ' ',modflux))
			df.glm2 = df.glm[which(df.glm$prisec==PRISEC & df.glm$modflux==modflux),]
			mx.beta     <- matrix(NA,ncol=360,nrow=720)
			ls.betas <- list(prec=mx.beta, temp=mx.beta,empty_slot=1,
				             age_1=mx.beta,age_2=mx.beta,age_3=mx.beta,age_4=mx.beta,
				             age_5=mx.beta,age_6=mx.beta,age_7=mx.beta,age_8=mx.beta,
				             age_9=mx.beta,age_10=mx.beta,age_11=mx.beta,age_12=mx.beta) 
			for(z in 1:length(z.vars)){
				print(z.vars[z])
				t.var   = z.vars[z]
				nc.long = z.long[z]
				nc.unit = z.unit[z]
			
				lat.vec <- seq(-89.75,89.75,by=0.5)
				lon.vec <- seq(-179.75,179.75,by=0.5)
				
				#loop t.glm and populate betas
				#populate matrix with betas of ageclass1
				#..only if p-value is <= 0.05
				t.sub = df.glm2[which(df.glm2$prisec==PRISEC & df.glm2$modflux==modflux & 
				                      df.glm2$varname==t.var & df.glm2$Pval <= 0.05),]
				for(k in 1:nrow(t.sub)){
					g.lon = which(t.sub$lon[k] == lon.vec, arr.ind = TRUE)
					g.lat = which(t.sub$lat[k] == lat.vec, arr.ind = TRUE)
					
					if(z.vars[z]=="(Intercept)"){
						#only single age-class in the grid-cell
			            #..populate the corresponding age-class matrix 
						#..identified by the ageclass code
						ls.betas[[t.sub$intcage[k]]][g.lon,g.lat]  <- t.sub$B[k]
					}else{
						ls.betas[[z]][g.lon,g.lat] <- t.sub$B[k]
					}
				}
				
				#intercept is the age-class effect when only one age-class in grid-cell
				#..intercept values are added to the corresponding matrix for each ageclass
				if(z.vars[z]!="(Intercept)"){
					#................
					#  make netcdf
					#................
					nodata = -9999
					nyears = 1
					lon.def  <- ncdim_def("lon", paste("degrees_east",sep=''), lon.vec,create_dimvar=TRUE)
					lat.def  <- ncdim_def("lat", paste("degrees_north", sep=''), lat.vec,create_dimvar=TRUE)
					time.def <- ncdim_def("time", units=paste0("years since ",paste0("2008-01-01")), vals=0, calendar="standard",unlim=TRUE)
					var.def  <- ncvar_def("beta", nc.unit, list(lon.def,lat.def,time.def), nodata, longname= nc.long)
					
					global.nc <- nc_create(paste0(f.out,PRISEC,"_",stattype,"_",modflux,"_beta_",nc.long,".nc"), var.def)
					
					#Assign global attributes
					ncatt_put(global.nc, 0, "Description", "Beta coefficient from GLM model")
					ncatt_put(global.nc, 0, "Project", "LPJ Ageclass MS")
					ncatt_put(global.nc, 0, "Contact", "leonardo.calle@montana.edu")
					ncatt_put(global.nc, 0, "Institution", "Montana State University")
					ncvar_put(global.nc, var.def, ls.betas[[z]])
					nc_close(global.nc)
				}
					
			}#..end z loop all vars
		
	}#..end modflux loop
}#..end prisec loop

#clean up
rm(ls.betas)

#..................................................
# make map of effective range of predictor
# ..for ageclassses:
#   ..max ageclass integer
#   ..effective range all ageclasses
#
# for ageclass range:
# ..read data from list structure from previous step
#
#  NOTE: 
#  ..betas for ageclass 1:12
#  ..actual value, not relative to intercept
#  .. ..but when only one age-class in grid-cell,
#  .. ..intercept term is the age-class mean 
#  ..required to translate intercept to age-class
#  
#..................................................
PRISEC.vec   = c('pri','sec')
modflux.vec  = c('npp','rh')
stattype     = 'stat_nointc'  #stat_nointc

f.path   = paste0(fdir.main,"/DATA_analysis/glm_models/netcdf_nointc/")
z.vars1  = c("prec","temp")
z.vars2  = c('precipitation','temperature','empty_slot',
			 'ageclass_1','ageclass_2','ageclass_3','ageclass_4','ageclass_5','ageclass_6',
			 'ageclass_7','ageclass_8','ageclass_9','ageclass_10','ageclass_11','ageclass_12')

for(PRISEC in PRISEC.vec){
	for(modflux in modflux.vec){
		print(paste0(PRISEC," ",modflux))
		#empty matrix
		mx.beta     <- matrix(NA,ncol=360,nrow=720)
		mx.beta.max <- matrix(NA,ncol=360,nrow=720)
		mx.beta.min <- matrix(NA,ncol=360,nrow=720)
		mx.beta.max.eff <- matrix(NA,ncol=360,nrow=720)
		mx.beta.min.eff <- matrix(NA,ncol=360,nrow=720)
		lat.vec <- seq(-89.75,89.75,by=0.5)
		lon.vec <- seq(-179.75,179.75,by=0.5)

		#..mask land
		mask.land <- fn_getvar(var = 'fpc',fpath = paste0(f.path,"mask_gtc_0d01_lpj_trendy7_s2_19502017_timmean_vertsum_plantonly_fpc.nc"))

		#.................................
		#for ageclass range and max flux
		#read data from processed netcdfs from previous step
		countage = 1
		for(z in 4:length(z.vars2)){
			print(z.vars2[z])
			t.var = z.vars2[z]
			
			t.dat <- fn_getvar(var = 'beta',fpath = paste0(f.path,PRISEC,"_",stattype,"_",modflux,"_beta_",t.var,".nc"))
			for(i in 1:nrow(t.dat)){
				for(j in 1:ncol(t.dat)){
						if(z==4){
							 if(!is.na(t.dat[i,j])){
							 	 mx.beta[i,j]     <- countage
								 mx.beta.max[i,j] <- t.dat[i,j]	
								 mx.beta.min[i,j] <- t.dat[i,j]					 	 
							 }
						}else{
							 if(!is.na(t.dat[i,j])){
							 		 if(is.na(mx.beta[i,j])){
							 		 		 #regular update if no value
										 	 mx.beta[i,j]     <- countage
											 mx.beta.max[i,j] <- t.dat[i,j]	
											 mx.beta.min[i,j] <- t.dat[i,j]								 		 	
							 		 }else{
											 #update maximum
											 if(t.dat[i,j] > mx.beta.max[i,j]){
													mx.beta.max[i,j] <- t.dat[i,j]
													#update ageclass of maximum flux
													mx.beta[i,j]     <- countage			 	
											 }
											 #update minimum
											 if(t.dat[i,j] < mx.beta.min[i,j]){
											  	mx.beta.min[i,j] <- t.dat[i,j]
											 }
							 		 }
							 }#..end na check
						}
				}#..end j loop
			}#..end i loop
			
			#update counter countage
		  countage = countage+1
		}#..end z loop all vars

		#after assesing min max and ageclass of max flux get range and save to netcdf
		#..also mask land
		#..use prisec map of mx.beta to create mask where primary,secondary lands occur
		#.. .. everything else set to NA
		for(i in 1:2){
			if(i==1){
				#create prisec mask
				#prisec.mask = mx.beta
                #prisec.mask[prisec.mask > 0]  <- 1
                #prisec.mask[prisec.mask != 1] <- NA
        
				#remove grid-cells where only single age-class exists
				#..no real 'max' flux when only single age-class
				#..otherwise skews the data
				t.mx.chk  =  mx.beta.max - mx.beta.min
				t.send.na =  which(t.mx.chk==0,arr.ind=TRUE)
				
				t.mx.beta =  mx.beta
				for(j in 1:nrow(t.send.na)){
					t.r = t.send.na[j,1]
				  t.c = t.send.na[j,2]
					t.mx.beta[t.r,t.c] =  NA
				}
				#get vegetated land only
				t.mx.beta = t.mx.beta*mask.land
				
				#get pri/sec lands only
			    #t.mx.beta = t.mx.beta*prisec.mask 
				#set zeros to NA
			    t.mx.beta[t.mx.beta==0] <- NA 

				nc.name   = 'ageclasscode_maxflux'
				nc.long   = 'ageclasscode_maxflux'
				nc.unit   = 'ageclass_code'	  	
			}else{
				t.mx.beta =  mx.beta.max - mx.beta.min
		
				#get vegetated land only
				t.mx.beta = t.mx.beta*mask.land
				
				#get pri/sec lands only
			    #t.mx.beta = t.mx.beta*prisec.mask 
				#set zeros to NA
			    t.mx.beta[t.mx.beta==0] <- NA 
			  
				nc.name   = 'ageclass_maxmin_effectiverange'
				nc.long   = 'ageclass_maxmin_effectiverange'
				nc.unit   = 'kg C m-2 yr-2'	  	
			}
				
			#................
			#  make netcdf
			#................
			nodata = -9999
			nyears = 1
			lon.def  <- ncdim_def("lon", paste("degrees_east",sep=''), lon.vec,create_dimvar=TRUE)
			lat.def  <- ncdim_def("lat", paste("degrees_north", sep=''), lat.vec,create_dimvar=TRUE)
			time.def <- ncdim_def("time", units=paste0("years since ",paste0("2008-01-01")), vals=0, calendar="standard",unlim=TRUE)
			var.def  <- ncvar_def("beta", nc.unit, list(lon.def,lat.def,time.def), nodata, longname= nc.long)
			
			global.nc <- nc_create(paste0(f.path,PRISEC,"_",stattype,"_",modflux,"_beta_",nc.name,".nc"), var.def)
			
			#Assign global attributes
			ncatt_put(global.nc, 0, "Description", "Ageclass Code: 1=10yr,..,10=91-100yr,11=101-150,12=151+ ")
			ncatt_put(global.nc, 0, "Project", "LPJ Ageclass MS")
			ncatt_put(global.nc, 0, "Contact", "leonardo.calle@montana.edu")
			ncatt_put(global.nc, 0, "Institution", "Montana State University")
			ncvar_put(global.nc, var.def, t.mx.beta)
			nc_close(global.nc)
			
		}

		#.................................
		#for precip and temp only
		#subset for quicker loop
		df.glm2 = df.glm[which(df.glm$prisec==PRISEC & df.glm$modflux==modflux),]
		#read data from raw glm output bc need predictor observed range
		#after assesing min max and ageclass of max flux get range and save to netcdf
		for(z in 1:2){
			print(z.vars1[z])
			t.var = z.vars1[z]
		
			#loop matrix and populate betas
			#only p-values <= 0.05
			t.sub = df.glm2[which(df.glm2$prisec==PRISEC & df.glm2$modflux==modflux & df.glm2$varname==t.var & df.glm2$Pval <= 0.05),]
			for(k in 1:nrow(t.sub)){
				g.lon = which(t.sub$lon[k] == lon.vec, arr.ind = TRUE)
				g.lat = which(t.sub$lat[k] == lat.vec, arr.ind = TRUE)
				
			    mx.beta.max[g.lon,g.lat] <- t.sub$B[k]	
			    mx.beta.min[g.lon,g.lat] <- t.sub$B[k]	
			    #if precip or temp, then update values for effective range
			    if(t.var=='prec'){
				  mx.beta.max.eff[g.lon,g.lat] <- mx.beta.max[g.lon,g.lat] * t.sub$prec_max[k]
				  mx.beta.min.eff[g.lon,g.lat] <- mx.beta.min[g.lon,g.lat] * t.sub$prec_min[k]		 	
			    }else if(t.var=='temp'){
				  mx.beta.max.eff[g.lon,g.lat] <- mx.beta.max[g.lon,g.lat] * t.sub$temp_max[k]
				  mx.beta.min.eff[g.lon,g.lat] <- mx.beta.min[g.lon,g.lat] * t.sub$temp_min[k]			 	
			    }
			}#..end k loop
			
			#................
			#  make netcdf
			#................
			t.mx.beta =  mx.beta.max.eff - mx.beta.min.eff
			#mask land
			t.mx.beta =  t.mx.beta*mask.land
			#get pri/sec lands only
			#t.mx.beta = t.mx.beta*prisec.mask 
			#set zeros to NA
		    t.mx.beta[t.mx.beta==0] <- NA 

			nc.name   =  paste0(z.vars1[z],'_maxmin_effectiverange')
			nc.long   =  modflux
			nc.unit   = 'kg C yr-2'
		
			nodata = -9999
			nyears = 1
			lon.def  <- ncdim_def("lon", paste("degrees_east",sep=''), lon.vec,create_dimvar=TRUE)
			lat.def  <- ncdim_def("lat", paste("degrees_north", sep=''), lat.vec,create_dimvar=TRUE)
			time.def <- ncdim_def("time", units=paste0("years since ",paste0("2008-01-01")), vals=0, calendar="standard",unlim=TRUE)
			var.def  <- ncvar_def("beta", nc.unit, list(lon.def,lat.def,time.def), nodata, longname= nc.long)
			
			global.nc <- nc_create(paste0(f.path,PRISEC,"_",stattype,"_",modflux,"_beta_",nc.name,".nc"), var.def)
			
			#Assign global attributes
			ncatt_put(global.nc, 0, "Description", "Ageclass Code: 1=10yr,..,10=91-100yr,11=101-150,12=151+ ")
			ncatt_put(global.nc, 0, "Project", "LPJ Ageclass MS")
			ncatt_put(global.nc, 0, "Contact", "leonardo.calle@montana.edu")
			ncatt_put(global.nc, 0, "Institution", "Montana State University")
			ncvar_put(global.nc, var.def, t.mx.beta)
			nc_close(global.nc)
			
		}#..end z loop all vars
		

	}#..end modflux loop
}#..end prisec loop

#======================
#  histograms max age
#  ..overlaid npp, rh
#  ..pri and sec
#======================
PRISEC.vec  = c('pri','sec')
modflux.vec = c('npp','rh')
stattype    = 'stat_nointc'  #stat_nointc or empty ('')
 
z.vars  = c('ageclasscode_maxflux','ageclass_maxmin_effectiverange')
f.path  = paste0(fdir.main,"/DATA_analysis/glm_models/netcdf_nointc/")
#empty matrix
ls.beta <- ls.beta.range <- list() #four in each list for pri,sec each npp,rh
mx.beta.int <- matrix(NA,ncol=360,nrow=720)
mx.beta     <- matrix(NA,ncol=360,nrow=720)
mx.beta.max <- matrix(NA,ncol=360,nrow=720)
mx.beta.min <- matrix(NA,ncol=360,nrow=720)
lat.vec <- seq(-89.75,89.75,by=0.5)
lon.vec <- seq(-179.75,179.75,by=0.5)

counter=1
for(PRISEC in PRISEC.vec){
	for(modflux in modflux.vec){
		#read processed data from netcdfs from previous step
		ls.beta[[counter]]       <- fn_getvar(var = 'beta',fpath = paste0(f.path,PRISEC,"_",stattype,"_",modflux,"_beta_",z.vars[1],".nc"))
        ls.beta.range[[counter]] <- fn_getvar(var = 'beta',fpath = paste0(f.path,PRISEC,"_",stattype,"_",modflux,"_beta_",z.vars[2],".nc"))

		#update counter
		counter=counter+1
	}#..end modflux loop
}#..end prisec loop
	
#data for histograms
#ls.beta       : ageclass code with max flux
#ls.beta.range : effective range in flux due to ageclass (max-min)

#table data in format for ggplot
for(foldcode_processdata in 1:1){
		df.bar.age <- data.frame(agenum  = c(as.vector(ls.beta[[1]]),as.vector(ls.beta[[2]]),as.vector(ls.beta[[3]]),as.vector(ls.beta[[4]])),
								 prisec  = c(rep('pri',360*720),rep('pri',360*720),rep('sec',360*720),rep('sec',360*720)),
							     modflux = c(rep('npp',360*720),rep('rh',360*720),rep('npp',360*720),rep('rh',360*720)))
		df.bar.flux  <- data.frame(flux    = c(as.vector(ls.beta.range[[1]]),as.vector(ls.beta.range[[2]]),as.vector(ls.beta.range[[3]]),as.vector(ls.beta.range[[4]])),
								   prisec  = c(rep('pri',360*720),rep('pri',360*720),rep('sec',360*720),rep('sec',360*720)),
								   modflux = c(rep('npp',360*720),rep('rh',360*720),rep('npp',360*720),rep('rh',360*720)))
		#drop NAs and zeros
		df.bar.age  <- df.bar.age[which(!is.na(df.bar.age$agenum)),]
		df.bar.flux <- df.bar.flux[which(!is.na(df.bar.flux$flux)),]

		#add agecode factor
		df.bar.age$agecode <- as.factor(df.bar.age$agenum)
		
		#summarize to counts
		df.bar.age   <- plyr::ddply(.data = df.bar.age,.variables = c('prisec','modflux','agecode'),plyr::summarise,
												count=length(agenum))
}

#plot and store ggplot objects
ls.gg.age      <- list()
for(foldcode_get_ggplots in 1:1){
		ls.gg.age[[1]] <- ggplot(df.bar.age[which(df.bar.age$prisec=='pri'),], aes(colour=modflux,fill=modflux, y=count, x=agecode)) + 
		    geom_bar(stat="identity") + 
		 	scale_colour_manual(values=c('npp'=scales::alpha('grey75',1),'rh'=scales::alpha('salmon',1))) +
		 	scale_fill_manual(values=c('npp'=scales::alpha('grey75',0.8),'rh'=scales::alpha('salmon',0.8))) +
			coord_cartesian(ylim=c(0,30000)) +
		    theme_bw() + 
			labs(title=paste0('primary'),
		         y= 'frequency',
		         x= 'agecode') +
			theme(plot.title = element_text(hjust=0.5),
				legend.text=element_text(size=rel(0.9)),
				legend.title=element_text(size=rel(0.9)),
				axis.title.y=element_text(hjust=0.5,size=rel(1.2)),
				axis.title.x=element_text(hjust=0.5,size=rel(1.2)),
				axis.text.y=element_text(size=rel(1.2)),
				axis.text.x=element_text(size=rel(1.2))) +
		     theme(plot.margin = unit(c(1,1,1,1), "lines")) +
			 theme(legend.position="none")
		ls.gg.age[[2]] <- ggplot(df.bar.age[which(df.bar.age$prisec=='sec'),], aes(colour=modflux,fill=modflux, y=count, x=agecode)) + 
		    geom_bar(stat="identity") + 
		 	scale_colour_manual(values=c('npp'=scales::alpha('grey75',1),'rh'=scales::alpha('salmon',1))) +
		 	scale_fill_manual(values=c('npp'=scales::alpha('grey75',0.8),'rh'=scales::alpha('salmon',0.8))) +
			coord_cartesian(ylim=c(0,30000)) +
			theme_bw() + 
			labs(title=paste0('secondary'),
		         y= 'frequency',
		         x= 'agecode') +
			theme(plot.title = element_text(hjust=0.5),
				legend.text=element_text(size=rel(0.9)),
				legend.title=element_text(size=rel(0.9)),
				axis.title.y=element_text(hjust=0.5,size=rel(1.2)),
				axis.title.x=element_text(hjust=0.5,size=rel(1.2)),
				axis.text.y=element_text(size=rel(1.2)),
				axis.text.x=element_text(size=rel(1.2))) +
		    theme(plot.margin = unit(c(1,1,1,1), "lines")) +
			theme(legend.position="none")
		
		ls.gg.age[[3]] <-  ggplot(df.bar.flux[which(df.bar.flux$prisec=='pri'),], aes(fill=modflux,colour=modflux, x=flux)) + 
			geom_histogram(binwidth=0.1,alpha=0.5) +
		 	scale_colour_manual(values=c('npp'=scales::alpha('forestgreen',0.6),'rh'=scales::alpha('darkorchid',0.6))) +
		 	scale_fill_manual(values=c('npp'=scales::alpha('forestgreen',0.6),'rh'=scales::alpha('darkorchid',0.6))) +
			coord_cartesian(xlim=c(-0.1,1.0),ylim=c(0,42000)) +
			theme_bw() + 
			labs(title=paste0('primary'),
		      x= 'kg C yr-1 m-12') +
			theme(plot.title = element_text(hjust=0.5),
				legend.text=element_text(size=rel(0.9)),
				legend.title=element_text(size=rel(0.9)),
				axis.title.y=element_text(hjust=0.5,size=rel(1.2)),
				axis.title.x=element_text(hjust=0.5,size=rel(1.2)),
				axis.text.y=element_text(size=rel(1.2)),
				axis.text.x=element_text(size=rel(1.2))) +
		 	theme(plot.margin = unit(c(1,1,1,1), "lines")) +
		    theme(legend.position="none")
		ls.gg.age[[4]] <-  ggplot(df.bar.flux[which(df.bar.flux$prisec=='sec'),], aes(fill=modflux,colour=modflux, x=flux)) + 
			geom_histogram(binwidth=0.1,alpha=0.5) +
		 	scale_colour_manual(values=c('npp'=scales::alpha('forestgreen',0.6),'rh'=scales::alpha('darkorchid',0.6))) +
		 	scale_fill_manual(values=c('npp'=scales::alpha('forestgreen',0.6),'rh'=scales::alpha('darkorchid',0.6))) +
			coord_cartesian(xlim=c(-0.1,1.0),ylim=c(0,42000)) +
			theme_bw() + 
			labs(title=paste0('secondary'),
		      x= 'kg C yr-1 m-12') +
			theme(plot.title = element_text(hjust=0.5),
				legend.text=element_text(size=rel(0.9)),
				legend.title=element_text(size=rel(0.9)),
				axis.title.y=element_text(hjust=0.5,size=rel(1.2)),
				axis.title.x=element_text(hjust=0.5,size=rel(1.2)),
				axis.text.y=element_text(size=rel(1.2)),
				axis.text.x=element_text(size=rel(1.2))) +
		 	theme(plot.margin = unit(c(1,1,1,1), "lines")) +
		    theme(legend.position="none")	
}

#print stat summaries
PRISEC='sec'
modflux='npp'
mean(df.bar.flux[which(df.bar.flux$prisec==PRISEC & df.bar.flux$modflux==modflux & df.bar.flux$flux !=0),'flux'])
sqrt(var(df.bar.flux[which(df.bar.flux$prisec==PRISEC & df.bar.flux$modflux==modflux & df.bar.flux$flux !=0),'flux']))
max(df.bar.flux[which(df.bar.flux$prisec==PRISEC & df.bar.flux$modflux==modflux & df.bar.flux$flux !=0),'flux'])
min(df.bar.flux[which(df.bar.flux$prisec==PRISEC & df.bar.flux$modflux==modflux & df.bar.flux$flux !=0),'flux']);

#......now save to pdf
if(makeplot==TRUE){
		t.modflux = paste0('_',modflux)
		pdf(file = paste0(fdir.main,"/DATA_analysis/figures_models/prisec_histograms_",stattype,"_maxage_effectiverange_glm_gridcell.pdf"),
		width=9,height=8)
		
		grid.arrange(
		  grobs         = ls.gg.age[c(1:4)],
		  layout_matrix = rbind(c(1, 2),c(3, 4))
		)			
}
if(makeplot==TRUE){dev.off()}


#================================
#       Scenario for NPP
#     'a very young world'
#             vs
#     'contemporary world'
#================================
PRISEC.vec   = c('pri','sec')
modflux      = c('npp')
stattype     = 'stat_nointc'  #stat_output or stat_output_nointc

f.path   = paste0(fdir.main,"/DATA_analysis/glm_models/netcdf_notintc/")
z.vars2  = c('ageclass_1','ageclass_2','ageclass_3','ageclass_4','ageclass_5','ageclass_6',
						'ageclass_7','ageclass_8','ageclass_9','ageclass_10','ageclass_11','ageclass_12')

mx.beta.max <- mx.beta.min <- matrix(NA,ncol=360,nrow=720)
lat.vec <- seq(-89.75,89.75,by=0.5)
lon.vec <- seq(-179.75,179.75,by=0.5)

#..mask land
mask.land <- fn_getvar(var = 'fpc',fpath = paste0(f.path,"mask_gtc_0d01_lpj_trendy7_s2_19502017_timmean_vertsum_plantonly_fpc.nc"))

#.................................
#for ageclass effect,  max min flux
#read data from processed netcdfs from previous step
counter = 1
for(PRISEC in PRISEC.vec){
	for(z in 1:length(z.vars2)){
	print(z.vars2[z])
	t.var = z.vars2[z]
		
	t.dat <- fn_getvar(var = 'beta',fpath = paste0(f.path,PRISEC,"_",stattype,"_",modflux,"_beta_",t.var,".nc"))
	for(i in 1:nrow(t.dat)){
			for(j in 1:ncol(t.dat)){
					 if(!is.na(t.dat[i,j])){
					 		 if(is.na(mx.beta.max[i,j])){
					 		 		 #regular update if no value
									 mx.beta.max[i,j] <- t.dat[i,j]	
					 		 }else{		
									 #update maximum
									 if(t.dat[i,j] > mx.beta.max[i,j]){
											mx.beta.max[i,j] <- t.dat[i,j]
									 }
					 		 }
					 	
					 	   #update minimum
					 		 if(is.na(mx.beta.min[i,j])){
					 		 		 #regular update if no value
									 mx.beta.min[i,j] <- t.dat[i,j]	
					 		 }else{		
									 #update minimum
									 if(t.dat[i,j] < mx.beta.min[i,j]){
											mx.beta.min[i,j] <- t.dat[i,j]
									 }
					 		 }
					 	
					 }
			}#end j loop
	}#..end i loop
	}#..end z loop
}#..end prisec looop
#................
#  make netcdf
#................
for(j in 1:2){
    if(j==1){
		#save a very young world -- max NPP flux
		t.mx.beta = mx.beta.max
		nc.name   =  'ageclass_maxflux'
		nc.long   =  'ageclass_maxflux'
	    f.name    =  'prisec_averyyoungworld'
    }else{
		#save a very old world -- min NPP flux
		t.mx.beta = mx.beta.min
		nc.name   =  'ageclass_minflux'
		nc.long   =  'ageclass_minflux'
	    f.name    =  'prisec_averyoldworld'
    }
	
		#get vegetated land only
		t.mx.beta = t.mx.beta*mask.land
		
		#get pri/sec lands only
		#t.mx.beta = t.mx.beta*prisec.mask 
		#set zeros to NA
		t.mx.beta[t.mx.beta==0] <- NA 
		
		nc.unit   = 'kg C m-2 yr-2'	  	
		
		nodata = -9999
		nyears = 1
		lon.def  <- ncdim_def("lon", paste("degrees_east",sep=''), lon.vec,create_dimvar=TRUE)
		lat.def  <- ncdim_def("lat", paste("degrees_north", sep=''), lat.vec,create_dimvar=TRUE)
		time.def <- ncdim_def("time", units=paste0("years since ",paste0("2008-01-01")), vals=0, calendar="standard",unlim=TRUE)
		var.def  <- ncvar_def("beta", nc.unit, list(lon.def,lat.def,time.def), nodata, longname= nc.long)
		
		global.nc <- nc_create(paste0(f.path,f.name,"_",stattype,"_",modflux,"_beta_",nc.name,".nc"), var.def)
		
		#Assign global attributes
		ncatt_put(global.nc, 0, "Description", "Ageclass Code: 1=10yr,..,10=91-100yr,11=101-150,12=151+ ")
		ncatt_put(global.nc, 0, "Project", "LPJ Ageclass MS")
		ncatt_put(global.nc, 0, "Contact", "leonardo.calle@montana.edu")
		ncatt_put(global.nc, 0, "Institution", "Montana State University")
		ncvar_put(global.nc, var.def, t.mx.beta)
		nc_close(global.nc)
}

#.........................................
#for precipitation effect,  max min beta
#read data from processed netcdfs from previous step
z.vars1  = c('precipitation','temperature')
mx.beta.max <- mx.beta.min <- matrix(NA,ncol=360,nrow=720)

for(z in 1:length(z.vars1)){
	print(z.vars1[z])
	t.var = z.vars1[z]
	mx.beta.max <- mx.beta.min <- matrix(NA,ncol=360,nrow=720)
	for(PRISEC in PRISEC.vec){
		t.dat <- fn_getvar(var = 'beta',fpath = paste0(f.path,PRISEC,"_",stattype,"_",modflux,"_beta_",t.var,".nc"))
		for(i in 1:nrow(t.dat)){
				for(j in 1:ncol(t.dat)){
						 if(!is.na(t.dat[i,j])){
						 	   #update maximum
					 		 if(is.na(mx.beta.max[i,j])){
					 		 		 #regular update if no value
									 mx.beta.max[i,j] <- t.dat[i,j]	
					 		 }else{		
									 #update maximum
									 if(t.dat[i,j] > mx.beta.max[i,j]){
											mx.beta.max[i,j] <- t.dat[i,j]
									 }
					 		 }
					 	
					 	   #update minimum
					 		 if(is.na(mx.beta.min[i,j])){
					 		 		 #regular update if no value
									 mx.beta.min[i,j] <- t.dat[i,j]	
					 		 }else{		
									 #update minimum
									 if(t.dat[i,j] < mx.beta.min[i,j]){
											mx.beta.min[i,j] <- t.dat[i,j]
									 }
					 		 }
						 }#..end na check
				}#end j loop
		}#..end i loop
	}#..end prisec loop
	
	#................
	#  make netcdf
	#................
	for(j in 1:2){
		  if(j==1){
				#save a very young world -- max beta 
				t.mx.beta =  mx.beta.max
		 		nc.name   =  paste0(t.var,'_maxbeta')
				nc.long   =  paste0(t.var,'_maxbeta')
		    f.name    =  'prisec_averyyoungworld'
		  }else{
		 		#save a very old world -- min beta flux
				t.mx.beta = mx.beta.min
		 		nc.name   =  paste0(t.var,'_minbeta')
				nc.long   =  paste0(t.var,'_minbeta')
		    f.name    =  'prisec_averyyoungworld'
		  }	
		
			#get vegetated land only
			t.mx.beta = t.mx.beta*mask.land
			
			#get pri/sec lands only
			#t.mx.beta = t.mx.beta*prisec.mask 
			#set zeros to NA
			#t.mx.beta[t.mx.beta==0] <- NA 
			
			nc.unit   = 'kg C m-2 yr-2'	  	
			
			nodata = -9999
			nyears = 1
			lon.def  <- ncdim_def("lon", paste("degrees_east",sep=''), lon.vec,create_dimvar=TRUE)
			lat.def  <- ncdim_def("lat", paste("degrees_north", sep=''), lat.vec,create_dimvar=TRUE)
			time.def <- ncdim_def("time", units=paste0("years since ",paste0("2008-01-01")), vals=0, calendar="standard",unlim=TRUE)
			var.def  <- ncvar_def("beta", nc.unit, list(lon.def,lat.def,time.def), nodata, longname= nc.long)
			
			global.nc <- nc_create(paste0(f.path,f.name,"_",stattype,"_",modflux,"_beta_",nc.name,".nc"), var.def)
			
			#Assign global attributes
			ncatt_put(global.nc, 0, "Description", "Ageclass Code: 1=10yr,..,10=91-100yr,11=101-150,12=151+ ")
			ncatt_put(global.nc, 0, "Project", "LPJ Ageclass MS")
			ncatt_put(global.nc, 0, "Contact", "leonardo.calle@montana.edu")
			ncatt_put(global.nc, 0, "Institution", "Montana State University")
			ncvar_put(global.nc, var.def, t.mx.beta)
			nc_close(global.nc)

	}#..end j loop

}#..end z loop

#==================================
#==================================
#
#  now calculate fields
#  ..mask for current prisec area
#  ..mask for land area, else NA
#
#==================================
#==================================
f.path   = paste0(fdir.main,"/DATA_analysis/glm_models/netcdf_notintc/")
mx.flux.max <- mx.flux.min <- matrix(NA,ncol=360,nrow=720)

#area by grid-cell
grid.area <- fn_getvar(var = 'cell_area',fpath = paste0(f.path,"gridarea.nc"))

#climate
cru.prec.2016 <- fn_getvar(var = 'pre',fpath = paste0(f.path,"cru_ts326_2016_prec_yearsum_sub_20002017_timmin.nc"))
cru.temp.2016 <- fn_getvar(var = 'tmp',fpath = paste0(f.path,"cru_ts326_2016_temp_yearmean_sub_20002017_timmin.nc"))

#mask for land and 2016 fractional area of primary + secondary stands
mask.land        <- fn_getvar(var = 'fpc',fpath = paste0(f.path,"mask_gtc_0d01_lpj_trendy7_s2_19502017_timmean_vertsum_plantonly_fpc.nc"))
mask.prisec.frac <- fn_getvar(var = 'ageclass_pri_frac',fpath = paste0(f.path,"vertsum_prisec_frac_2016_orgresp_ageclass_luhv2.nc"))

#betas for prec, temp
#..use max beta for now
beta.prec <- fn_getvar(var = 'beta',fpath = paste0(f.path,"prisec_averyyoungworld_stat_nointc_npp_beta_precipitation_maxbeta.nc"))
beta.temp <- fn_getvar(var = 'beta',fpath = paste0(f.path,"prisec_averyyoungworld_stat_nointc_npp_beta_temperature_maxbeta.nc"))

#get predicted fluxes
flux.age.max <- fn_getvar(var = 'beta',fpath = paste0(f.path,"prisec_averyyoungworld_stat_nointc_npp_beta_ageclass_maxflux.nc"))
flux.age.min <- fn_getvar(var = 'beta',fpath = paste0(f.path,"prisec_averyoldworld_stat_nointc_npp_beta_ageclass_minflux.nc"))
flux.prec    <- beta.prec*cru.prec.2016
flux.temp    <- beta.temp*cru.temp.2016

#set NAs to zero for addition
flux.prec[is.na(flux.prec)]       <- 0
flux.temp[is.na(flux.temp)]       <- 0
flux.age.max[is.na(flux.age.max)] <- 0
flux.age.min[is.na(flux.age.min)] <- 0

# NPP total flux
# ..correct for land area
# ..correct for fractional area of prisec
mx.flux.max = (flux.prec + flux.temp + flux.age.max)*mask.prisec.frac
mx.flux.min = (flux.prec + flux.temp + flux.age.min)*mask.prisec.frac

#set zeros to NAs
mx.flux.max[mx.flux.max==0] <- NA
mx.flux.min[mx.flux.min==0] <- NA

#get global sums in PgC
sum(mx.flux.max*grid.area,na.rm=TRUE)/1e12
sum(mx.flux.min*grid.area,na.rm=TRUE)/1e12

#.................
# save netcdf
#.................
for(j in 1:2){
	  if(j==1){
			#save a very young world -- max beta 
			t.mx.beta =  mx.flux.max
	 		nc.name   =  paste0('npp_averyyoungworld_max_ageeffect')
			nc.long   =  nc.name
			f.name    =  'prisec_npp_averyyoungworld_max_ageeffect'
	  }else{
	 		#save a very old world -- min beta flux
			t.mx.beta =  mx.flux.min
	 		nc.name   =  paste0('npp_averyoldworld_min_ageeffect')
			nc.long   =  nc.name
	    f.name    =  'prisec_npp_averyoldworld_min_ageeffect'
	  }	
	
		#get vegetated land only
		t.mx.beta = t.mx.beta*mask.land
		
		#get pri/sec lands only
		#t.mx.beta = t.mx.beta*prisec.mask 
		#set zeros to NA
		t.mx.beta[t.mx.beta==0] <- NA 
		
		nc.unit   = 'kg C m-2 yr-2'	  	
		
		nodata = -9999
		nyears = 1
		lon.def  <- ncdim_def("lon", paste("degrees_east",sep=''), lon.vec,create_dimvar=TRUE)
		lat.def  <- ncdim_def("lat", paste("degrees_north", sep=''), lat.vec,create_dimvar=TRUE)
		time.def <- ncdim_def("time", units=paste0("years since ",paste0("2008-01-01")), vals=0, calendar="standard",unlim=TRUE)
		var.def  <- ncvar_def("beta", nc.unit, list(lon.def,lat.def,time.def), nodata, longname= nc.long)
		
		global.nc <- nc_create(paste0(f.path,f.name,"_",stattype,"_",modflux,"_beta_",nc.name,".nc"), var.def)
		
		#Assign global attributes
		ncatt_put(global.nc, 0, "Description", "Ageclass Code: 1=10yr,..,10=91-100yr,11=101-150,12=151+ ")
		ncatt_put(global.nc, 0, "Project", "LPJ Ageclass MS")
		ncatt_put(global.nc, 0, "Contact", "leonardo.calle@montana.edu")
		ncatt_put(global.nc, 0, "Institution", "Montana State University")
		ncvar_put(global.nc, var.def, t.mx.beta)
		nc_close(global.nc)

}#..end j loop
