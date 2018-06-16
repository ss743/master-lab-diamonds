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
range_003s <- c(2.835,2.885)+0.02
fit_003s <- fgausfit(spec_003,range_003s)
label_003s=data.frame(x=fit_003s['mu','Estimate'],y=0.004,text=sprintf("f0=(%0.4f+-%0.4f) GHz",fit_003s['mu','Estimate'],fit_003s['mu','Std. Error']))
plot_spec(spec_003) + plotgaus(fit_003s,range_003s) + fplotgausline(fit_003s,0.01,-0.005) + geom_label(data=label_003s,aes(x=x,y=y,label=text),colour="red")

if(do_calc) odmr_013 <- average_csv("../data/osci/odmr_013.csv")
spec_013 <- change_units(odmr_013);
range_013s <- c(2.84,2.865)+0.02
range_013s2 <- c(2.87,2.885)+0.02
fit_013s <- fgausfit(spec_013,range_013s)
fit_013s2 <- fgausfit(spec_013,range_013s2)
label_013s=data.frame(x=c(fit_013s['mu','Estimate'],fit_013s2['mu','Estimate']),y=c(0.004,0.003),text=c(sprintf("f1=(%0.4f+-%0.4f) GHz",fit_013s['mu','Estimate'],fit_013s['mu','Std. Error']),sprintf("f2=(%0.4f+-%0.4f) GHz",fit_013s2['mu','Estimate'],fit_013s2['mu','Std. Error'])))
plot_spec(spec_013) + plotgaus(fit_013s,range_013s) + fplotgausline(fit_013s,0.01,-0.005) + plotgaus(fit_013s2,range_013s2) + fplotgausline(fit_013s2,0.01,-0.005) + 
    geom_label(data=label_013s,aes(x=x,y=y,label=text),colour="red")

if(do_calc) odmr_014 <- average_csv("../data/osci/odmr_014.csv")
spec_014 <- change_units(odmr_014);
range_014s <- c(2.78,2.82)+0.02
range_014s2 <- c(2.82,2.85)+0.02
range_014s3 <- c(2.885,2.91)+0.02
range_014s4 <- c(2.92,2.94)+0.02
fit_014s <- fgausfit(spec_014,range_014s)
fit_014s2 <- fgausfit(spec_014,range_014s2)
fit_014s3 <- fgausfit(spec_014,range_014s3)
fit_014s4 <- fgausfit(spec_014,range_014s4)
label_014s=data.frame(x=c(fit_014s['mu','Estimate'],fit_014s2['mu','Estimate'],fit_014s3['mu','Estimate'],fit_014s4['mu','Estimate']),y=c(0.004,0.003,-0.003,-0.004),text=c(sprintf("f1=(%0.4f+-%0.4f) GHz",fit_014s['mu','Estimate'],fit_014s['mu','Std. Error']),sprintf("f2=(%0.4f+-%0.4f) GHz",fit_014s2['mu','Estimate'],fit_014s2['mu','Std. Error']),sprintf("f3=(%0.4f+-%0.4f) GHz",fit_014s3['mu','Estimate'],fit_014s3['mu','Std. Error']),sprintf("f4=(%0.4f+-%0.4f) GHz",fit_014s4['mu','Estimate'],fit_014s4['mu','Std. Error'])))
plot_spec(spec_014) + plotgaus(fit_014s,range_014s) + fplotgausline(fit_014s,0.01,-0.005) + 
                      plotgaus(fit_014s2,range_014s2) + fplotgausline(fit_014s2,0.01,-0.005) + 
                      plotgaus(fit_014s3,range_014s3) + fplotgausline(fit_014s3,0.01,-0.005) + 
                      plotgaus(fit_014s4,range_014s4) + fplotgausline(fit_014s4,0.01,-0.005) + 
                      geom_label(data=label_014s,aes(x=x,y=y,label=text),colour="red")

if(do_calc) odmr_015 <- average_csv("../data/osci/odmr_015.csv")
spec_015 <- change_units(odmr_015);
range_015s <- c(2.74,2.76)+0.02
range_015s2 <- c(2.84,2.88)+0.02
#range_015s3 <- c(2.885,2.91)
range_015s3 <- c(2.92,2.94)+0.02
fit_015s <- fgausfit(spec_015,range_015s)
fit_015s2 <- fgausfit(spec_015,range_015s2)
fit_015s3 <- fgausfit(spec_015,range_015s3)
#fit_015s4 <- fgausfit(spec_015,range_015s4)
label_015s=data.frame(x=c(fit_015s['mu','Estimate'],fit_015s2['mu','Estimate'],fit_015s3['mu','Estimate']),y=c(0.0045,0.0035,0.0025),text=c(sprintf("f1=(%0.4f+-%0.4f) GHz",fit_015s['mu','Estimate'],fit_015s['mu','Std. Error']),sprintf("f2=(%0.4f+-%0.4f) GHz",fit_015s2['mu','Estimate'],fit_015s2['mu','Std. Error']),sprintf("f3=(%0.4f+-%0.4f) GHz",fit_015s3['mu','Estimate'],fit_015s3['mu','Std. Error'])))
plot_spec(spec_015) + plotgaus(fit_015s,range_015s) + fplotgausline(fit_015s,0.01,-0.005) + 
  plotgaus(fit_015s2,range_015s2) + fplotgausline(fit_015s2,0.01,-0.005) + 
  plotgaus(fit_015s3,range_015s3) + fplotgausline(fit_015s3,0.01,-0.005) + 
  #plotgaus(fit_015s4,range_015s4) + plotgausline(fit_015s4,0.01,-0.005) #+ 
  geom_label(data=label_015s,aes(x=x,y=y,label=text),colour="red")

