library("rioja")
library("readxl")
library("vegan")
# import data
van_counts <- read_excel("Emaotul_all_data.xlsx", sheet="pollen_counts")
depth<-van_counts[c(1)]
van_counts<-van_counts[-c(1)]
van_per <- van_counts / rowSums(van_counts) * 100
sp_max <- apply(van_per, 1, max)
#or 
sp_max <- sapply(van_per, max)
# Then use the max values as a condition to subset columns
tmp <- van_per[, sp_max > 1]

tmp<-cbind(depth, tmp)

# Import chronology
chron <- read_excel("Emaotul_all_data.xlsx", sheet="chron")
# Remove last column
chron <- chron[1:2]

tmp<-merge(tmp, chron, by= "depth")

# Cluster analysis
ma.dist <- vegdist(van_per, method="bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE) 
ma.chclust <- chclust(ma.dist, method="coniss")

pol.plot<-strat.plot(tmp[-grep("depth|age", colnames(tmp))], 
                     yvar=tmp$age, 
                     scale.percent=T, 
                     y.rev=TRUE, 
                     cex.xlabel=0.8, 
                     yTop=0.7, 
                     plot.line=FALSE,
                     plot.bar=TRUE,
                     plot.poly=FALSE,
                     lwd.bar=3,
                     srt.xlabel=45,
                     title="Lake Emaotul pollen % diagram displaying types >1%",
                     cex.title=1,
                     ylabel="Cal. years BP"
)

# Add cluster zones
addClustZone(pol.plot, ma.chclust, nZone=3, lwd=1.5, lty=2, col="grey25")

