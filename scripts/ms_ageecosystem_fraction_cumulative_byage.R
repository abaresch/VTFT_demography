#===================================================================================
# plots the cumulative sum of grid-cell age by fraction of all grid-cells (globe)
#===================================================================================
library(ncdf4)
fn_getvar <- function(var,fpath){
  t <- nc_open(fpath)
  v <- ncvar_get(t,var)
  nc_close(t)
  return(v)
}
fdir.main = "~/Downloads/" #or other directory
meanage <- fn_getvar(var = 'fpc',fpath = paste0(fdir.main,"/DATA_analysis/meanAge_gridcell_prisec_plantonly_fpc_gtc_0d01.nc"))

meanage <- as.vector(meanage)
meanage <- meanage[!is.na(meanage)] 
hist(meanage)

#put into bins by 10 years
meanage.cut <- cut(meanage, breaks = c(seq(0, 100, by = 10),125,150))

tot.cells <- sum(table(meanage.cut))

plot(x=1:12, cumsum(table(meanage.cut))/sum(table(meanage.cut)),type='p',
     pch=21,bg='grey75',cex=2,
     xaxt='n',
     ylab='fraction of all grid-cells',
     xlab='Ageclass',
     main='Ecossytem Age: Cumulative Fraction of Vegetated Earth')
 axis(side = 1,at = 1:12,labels = names(table(meanage.cut)))
 abline(h=0.5)
 text(x=1,y=0.55,adj=0,labels='50 %')
