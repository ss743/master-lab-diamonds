source("odmr_helper.R")
library(ggplot2)
theme_set(theme_bw())

do_calc=FALSE;

if(do_calc) odmr_001 <- average_csv("../data/osci/odmr_001.csv")
range_001 <- c(0.125,0.18)
fit_001 <- gausfit(odmr_001,range_001)
label_001=data.frame(x=fit_001['mu','Estimate'],y=0.004,text=sprintf("t0=(%0.4f+-%0.4f) s",fit_001['mu','Estimate'],fit_001['mu','Std. Error']))
plot_avg(odmr_001) + plotgaus(fit_001,range_001) + plotgausline(fit_001,0.01,-0.005) + geom_label(data=label_001,aes(x=x,y=y,label=text),colour="red")

if(do_calc) odmr_003 <- average_csv("../data/osci/odmr_003.csv")
range_003 <- c(0.125,0.18)
fit_003 <- gausfit(odmr_003,range_003)
label_003=data.frame(x=fit_003['mu','Estimate'],y=0.004,text=sprintf("t0=(%0.4f+-%0.4f) s",fit_003['mu','Estimate'],fit_003['mu','Std. Error']))
plot_avg(odmr_003) + plotgaus(fit_003,range_003) + plotgausline(fit_003,0.01,-0.005) + geom_label(data=label_003,aes(x=x,y=y,label=text),colour="red")

if(do_calc) odmr_004 <- average_csv("../data/osci/odmr_004.csv")
range_004 <- c(0.175,0.23)
fit_004 <- gausfit(odmr_004,range_004)
label_004=data.frame(x=fit_004['mu','Estimate'],y=0.004,text=sprintf("t2=(%0.4f+-%0.4f) s",fit_004['mu','Estimate'],fit_004['mu','Std. Error']))
plot_avg(odmr_004) + plotgaus(fit_004,range_004) + plotgausline(fit_004,0.01,-0.005) + geom_label(data=label_004,aes(x=x,y=y,label=text),colour="red")

if(do_calc) odmr_005 <- average_csv("../data/osci/odmr_005.csv")
range_005 <- c(0.075,0.12)
fit_005 <- gausfit(odmr_005,range_005)
label_005=data.frame(x=fit_005['mu','Estimate'],y=0.004,text=sprintf("t1=(%0.4f+-%0.4f) s",fit_005['mu','Estimate'],fit_005['mu','Std. Error']))
plot_avg(odmr_005) + plotgaus(fit_005,range_005) + plotgausline(fit_005,0.01,-0.005) + geom_label(data=label_005,aes(x=x,y=y,label=text),colour="red")

spec_003 <- change_units(odmr_003);
range_003s <- c(2.835,2.885)
fit_003s <- fgausfit(spec_003,range_003s)
label_003s=data.frame(x=fit_003s['mu','Estimate'],y=0.004,text=sprintf("f0=(%0.4f+-%0.4f) s",fit_003s['mu','Estimate'],fit_003s['mu','Std. Error']))
plot_spec(spec_003) + plotgaus(fit_003s,range_003s) + plotgausline(fit_003s,0.01,-0.005) + geom_label(data=label_003s,aes(x=x,y=y,label=text),colour="red")

if(do_calc) odmr_013 <- average_csv("../data/osci/odmr_013.csv")
spec_013 <- change_units(odmr_013);
range_013s <- c(2.835,2.885)
fit_013s <- fgausfit(spec_013,range_013s)
label_013s=data.frame(x=fit_013s['mu','Estimate'],y=0.004,text=sprintf("f0=(%0.4f+-%0.4f) s",fit_013s['mu','Estimate'],fit_013s['mu','Std. Error']))
plot_spec(spec_013) + plotgaus(fit_013s,range_013s) + plotgausline(fit_013s,0.01,-0.005) + geom_label(data=label_013s,aes(x=x,y=y,label=text),colour="red")
