source("odmr_helper.R")
library(ggplot2)
theme_set(theme_bw())

do_calc=TRUE;

if(do_calc) apd1 <- average_csv("../data/osci/apd-calibration-new.csv")
plot_avg(apd1)
height1=max(apd1$ch1)#-min(apd1$ch1)

if(do_calc) apd2 <- average_csv("../data/osci/apd-calibration-2.csv")
plot_avg(apd2)
height2=max(apd2$ch1)#-min(apd2$ch1)