library(readxl)
library(ggplot2)

data <-read_excel("Emaotul_all_data.xlsx", sheet="tephra")

p<-ggplot(data, aes(x=SiO2, y=MgO,shape=Depth, color=Depth, size=2)) +
  geom_point() + theme_bw()
p<-p+scale_color_manual(values = c("160-166" = "darkorange",
                                 "167-169" = "cornflowerblue",
                                 "Kuwae proximal"="darkslategray",
                                "171-173"="chartreuse3",
                                "Efate Pumice Formation"="steelblue"))
p+scale_shape_manual(values = c(16,17,15,3,4))

