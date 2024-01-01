library(itraxR)
library(readxl)
library(vegan)

# pollen####

magsus <-read.csv("magsus.csv")
names(magsus)[1]<-"depth"
names(magsus)[2]<-"Magnetic susc."
Ti<-read.csv("Ti_10cm.csv")
names(Ti)[2]<-"Ti/inc"
micro<-read_excel("Emaotul_all_data.xlsx", sheet="microcharcoal")
names(micro)[2]<-"Microcharcoal"
macro<-read_excel("Emaotul_all_data.xlsx", sheet="macrocharcoal")
names(macro)[2]<-"Macrocharcoal"
pollen_counts<-read_excel("Emaotul_all_data.xlsx", sheet="pollen_counts")
precip<-read_excel("Emaotul_all_data.xlsx", sheet="Precipitation")
names(precip)[2]<-"Precipitation"
precip<-precip[-c(3)]
zones<-read_excel("Emaotul_all_data.xlsx", sheet="pollen_zones")

x <- precip$depth
y <- precip$Precipitation
interpolated_precip<-approx(x, y, method = "linear", n = 350, ties = mean)
interpolated_precip<-as.data.frame(interpolated_precip)
plot(interpolated_precip)
names(interpolated_precip)[1]<-"depth"
names(interpolated_precip)[2]<-"Precipitation"

#Ti<-itrax_reduce(dataframe = Ti, by = 1)
#Ti<-Ti[-c(1)]
#write.csv(Ti, "Ti_10cm.csv")

depth<-pollen_counts$depth
pollen_counts<-pollen_counts[c(2:110)]
van_per <- pollen_counts / rowSums(pollen_counts) * 100
van_per$depth<-depth


all<-merge(magsus, Ti, by="depth", all = TRUE)
all<-merge(all, micro, by="depth", all = TRUE)
all<-merge(all, macro, by="depth", all = TRUE)
all<-merge(all, interpolated_precip, by="depth", all = TRUE)
all<-merge(all, van_per, by="depth")
all<-merge(all, zones, by="depth")
all<-all[-c(1:2),]
env<-all[c(2:6)]
pollen<-all[c(6:115)]
zones<-all$zones

cca <- cca(pollen, env)
cca
plot(cca, scaling = 2)
#text(cca, display = "species", cex = 0.5, col = "darkcyan")

scl <- 2 ## scaling = 3
#colvec <- c("red2", "green4", "mediumblue", "gold")

plot(cca, type = "n", scaling = scl)

with(env, points(cca, display = c("sites"), col = c("dodgerblue","chartreuse", "chartreuse", "chartreuse", "chartreuse", "chartreuse", "chartreuse", "chartreuse", "gold","gold","gold","gold","gold","gold","gold","gold","gold","gold","gold","gold","gold","orangered", "orangered","orangered","orangered","orangered","orangered","orangered","orangered","orangered"),
                 scaling = scl, pch = c(18,17,17,17,17,17,17,17,16,16,16,16,16,16,16,16,16,16,16,16,16,15,15,15,15,15,15,15,15,15), bg= zones, cex=2))

ordihull(cca, zones, scaling = 2, draw = "polygon", col = c("orangered", "gold", "chartreuse"), alpha = 0.3, border = NULL)

#ordipointlabel(cca, display = "sites", scaling = scl, add = TRUE)
