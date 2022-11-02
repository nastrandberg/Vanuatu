library(dplyr)
library(ggplot2)
library(itraxR)
library(readxl)
library(tidypaleo)
library(tidyverse)
library(vegan)

magsus <-read_excel("Emaotul_all_data.xlsx", sheet="magnetic_susceptibility")
Ti<-read_excel("Emaotul_all_data.xlsx", sheet="itrax")
micro<-read_excel("Emaotul_all_data.xlsx", sheet="microcharcoal")
macro<-read_excel("Emaotul_all_data.xlsx", sheet="macrocharcoal")
AP_NAP<-read_excel("Emaotul_all_data.xlsx", sheet="AP_NAP")
cn<-read_excel("Emaotul_all_data.xlsx", sheet="CN")
chiro_per<-read_excel("Emaotul_all_data.xlsx", sheet="chironomids_%")
pollen_counts<-read_excel("Emaotul_all_data.xlsx", sheet="pollen_counts")
dino<-read_excel("Emaotul_all_data.xlsx", sheet="dinosterol")
precip<-read_excel("Emaotul_all_data.xlsx", sheet="Precipitation")
  
#DCA pollen####
pollen_counts<-pollen_counts[-c(1,111)]
van_per <- pollen_counts / rowSums(pollen_counts) * 100
#pollen_counts<-pollen_counts[-c(32),]
scale <-scores(decorana(van_per))[,"DCA1"]	
van_per$DCA1<- scale
van_per$depth <- c(1, 4, 22, 27, 43, 59, 75, 90, 106, 154, 156, 157, 158, 159, 167, 168, 169, 177, 192, 207, 227, 237, 251, 266, 281, 296, 311, 321, 335, 322, 332, 341)
#pollen_counts$Cal_yrs_BP <- c(-65, -62, -4, 25, 81, 143, 205, 260, 318, 470, 476, 479, 483, 486, 537, 546, 554, 617, 721, 817, 936, 996, 1078, 1166, 1254, 1342, 1430, 1491, 1498, 1571, 1593)
van_per<-van_per[c(110,111)]
names(van_per)[1]<-"var"

#DCA chiros####
#This is the % data
chiro_per<-chiro_per[-c(1,12)]
#chiro_per<-chiro_per[-c(10),]
scale <-scores(decorana(chiro_per))[,"DCA1"]	
chiro_per$DCA1<- scale
chiro_per$depth <- c(61, 111, 160, 180, 200, 243, 261, 280, 331, 357)
#chiro_per$Cal_yrs_BP <- c(151, 335, 489, 638, 776, 1032, 1137, 1248, 1564, )
chiro_per<-chiro_per[c(11,12)]
names(chiro_per)[1]<-"var"

# average every 1cm
Ti<-itrax_reduce(dataframe = Ti, by = 1)
Ti<-Ti[-c(1)]

names(AP_NAP)[2]<-"var"
names(cn)[2]<-"var"
names(dino)[2]<-"var"
names(macro)[2]<-"var"
names(micro)[2]<-"var"
names(precip)[2]<-"var"
names(Ti)[2]<-"var"

AP_NAP$proxy<-"AP/NAP"
chiro_per$proxy<-"Chironomid DCA1"
cn$proxy<-"C/N"
dino$proxy<-"Dinosterol"
macro$proxy<-"Macrocharcoal"
magsus$proxy<-"Magnetic susc."
micro$proxy<-"Microcharcoal"
van_per$proxy<-"Pollen DCA1"
precip$proxy<-"Precip. rate"
Ti$proxy<-"Ti/inc."

all<-bind_rows(magsus, Ti, micro, macro, AP_NAP, cn, chiro_per, van_per, precip, dino)
all <- all[!is.na(all$var), ] 

Kuwae <- tibble(ymin = 160, ymax = 165, xmin = -Inf, xmax = Inf)
MCA <- tibble(ymin = 238, ymax = 250, xmin = -Inf, xmax = Inf)

####
all %>% 
  mutate(proxy = fct_relevel(proxy, "Magnetic susc.", "Ti/inc.", "Precip. rate", "Dinosterol", "Microcharcoal", "Macrocharcoal", "AP/NAP", "C/N", "Chironomid DCA1", "Pollen DCA1"))  %>%
  ggplot(aes(x = var, y = depth)) +
  geom_lineh(size = 0.6, colour= "black") +
  scale_y_reverse() +
  facet_geochem_gridh(vars(proxy)) +
  labs(x = NULL, y = "Depth (cm)") +
  theme(strip.background = element_rect(fill = "lightblue")) +
  theme(strip.text.x = element_text(size = 8, colour = "black", angle = 0)) +
  geom_rect(data=Kuwae, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="blue", alpha=0.3, inherit.aes = FALSE) +
  geom_rect(data=MCA, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.3, inherit.aes = FALSE) +
  geom_hline(yintercept = c(24.5, 155, 258), col = "red", lty = 2, alpha = 0.7) +
  theme_bw() +
  geom_errorbarh(aes(xmin = var - stdev, xmax = var + stdev), height = 0.5)
