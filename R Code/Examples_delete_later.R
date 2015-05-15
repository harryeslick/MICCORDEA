library(RColorBrewer)
library(plotKML)
DivColorBreaks <- c(-6,-3,-0.1,0.1,3,6)#
NDivBreaks <- length(DivColorBreaks)-1 

data(eberg_grid)
gridded(eberg_grid) <- ~x+y
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
eberg_grid$r <- eberg_grid$TWISRT6-mean(eberg_grid$TWISRT6)
DivColPalette <- colorRampPalette((brewer.pal(NDivBreaks, "RdBu")), space="Lab")
DivColorBreaks <- c(min(eberg_grid$r),-1,-0.1,0.1,1,max(eberg_grid$r))
eberg_grid$rc <- cut(eberg_grid$r, breaks=DivColorBreaks,
                     include.lowest=TRUE)
summary(eberg_grid$rc)
plotKML(eberg_grid["rc"],
        colour_scale=DivColPalette(length(levels(eberg_grid$rc)))) 

