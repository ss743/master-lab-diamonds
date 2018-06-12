library(ggplot2)
source("gausfit.R")
theme_set(theme_bw())

#Calibration Measurement for Laser Power
P_s=c(seq(0,10),12,15,17,20,22,seq(25,30),seq(35,50,5),seq(52,68),seq(70,100,5)); #Power Set on the Laser's Software Interface (mW)
P_m=c(16.5,16.6,17.2,18.0,18.5,19.1,19.8,20.2,20.7,21.2,21.8,23.0,24.7,25.7,27.4,28.3,29.7,30.2,
      30.7,31.0,31.7,32.1,34.2,36.3,38.3,40.0,40.5,41.1,41.3,41.5,41.8,42.0,42.2,42.4,42.6,42.8,
      43.0,43.1,43.4,43.5,43.4,43.4,43.4,43.4,43.2,43.0,43.4,43.5,43.4,43.4); #Power Measured with the Powermeter (mW)
s_P=sqrt((P_m*0.05)^2+0.05^2)+0.01;

P_data=data.frame(x=P_s,y=P_m,sy=s_P);
data20mw=data.frame(x=c(0,20,20),y=c(27.4,27.4,0),sy=c(0,0,0))

P_fit=P_data[P_data$x<=30,,]
fit=lm(P_fit$y~P_fit$x,weights=1/(P_fit$sy)^2)
func <- function(x) fit[[1]][2]*x+fit[[1]][1];


ggplot(data=P_data,aes(x=x,y=y,sy=sy)) + stat_function(fun=func,colour="blue") + geom_errorbar(aes(ymin=y-sy,ymax=y+sy),colour="darkgrey") +
        geom_point(colour="black",pch=4) + geom_point(data=P_fit,colour="blue",pch=4) + geom_line(data=data20mw,aes(x=x,y=y),colour="red") +
        labs(x=expression(Ps / mW),y=expression(Pm / mW))

#Calibration of the optical spectrometer
daten=read.csv("../data/spectrom/sun spectra -003.csv",sep=";",skip=33,header=FALSE,col.names=c("lambda","I"),dec=".")
daten=daten[daten[[2]]>0,]
daten=daten[1:(length(daten[[2]])-1),]
os=data.frame(x=daten[[1]],y=daten[[2]])

pos=c()
spos=c()
sigpos=c()

range1=c(525,530);
fit1=gausfit(os,range1);
pos=append(pos,fit1["mu","Estimate"])
spos=append(spos,fit1["mu","Std. Error"])
sigpos=append(sigpos,fit1["sig","Estimate"]/2)
range2=c(585,591);
fit2=gausfit(os,range2);
pos=append(pos,fit2["mu","Estimate"])
spos=append(spos,fit2["mu","Std. Error"])
sigpos=append(sigpos,fit2["sig","Estimate"]/2)
# range3=c(594,596);
# fit3=gausfit(os,range3);
# pos=append(pos,fit3["mu","Estimate"])
# spos=append(spos,fit3["mu","Std. Error"])
range4=c(625,630);
fit4=gausfit(os,range4);
pos=append(pos,fit4["mu","Estimate"])
spos=append(spos,fit4["mu","Std. Error"])
sigpos=append(sigpos,fit4["sig","Estimate"]/2)
range5=c(655,660);
fit5=gausfit(os,range5);
pos=append(pos,fit5["mu","Estimate"])
spos=append(spos,fit5["mu","Std. Error"])
sigpos=append(sigpos,fit5["sig","Estimate"]/2)
range6=c(680,690);
fit6=gausfit(os,range6);
pos=append(pos,fit6["mu","Estimate"])
spos=append(spos,fit6["mu","Std. Error"])
sigpos=append(sigpos,fit6["sig","Estimate"]/2)
range7=c(755,766);
fit7=gausfit(os,range7);
pos=append(pos,fit7["mu","Estimate"])
spos=append(spos,fit7["mu","Std. Error"])
sigpos=append(sigpos,fit7["sig","Estimate"]/2)
range8=c(820,827);
fit8=gausfit(os,range8);
pos=append(pos,fit8["mu","Estimate"])
spos=append(spos,fit8["mu","Std. Error"])
sigpos=append(sigpos,fit8["sig","Estimate"]/2)

labeldata=data.frame(x=pos,y=rep(0.1,length(pos)),text=1:length(pos))

#ggplot(os,aes(x=x,y=y)) + geom_line(colour="black") + ylim(0,1)
ggplot(os,aes(x=x,y=y)) + geom_line(colour="black") + plotgaus(fit1,range1) + plotgausline(fit1) + 
                                                      plotgaus(fit2,range2) + plotgausline(fit2) + 
                                                      # plotgaus(fit3,range3) + plotgausline(fit3) + 
                                                      plotgaus(fit4,range4) + plotgausline(fit4) + 
                                                      plotgaus(fit5,range5) + plotgausline(fit5) + 
                                                      plotgaus(fit6,range6) + plotgausline(fit6) + 
                                                      plotgaus(fit7,range7) + plotgausline(fit7) + 
                                                      plotgaus(fit8,range8) + plotgausline(fit8) +
                                                      geom_label(data=labeldata,aes(x=x,y=y,label=text),colour="red") +
                                                      xlab(expression(lambda/nm)) + ylab(expression(I))
#ggplot(os,aes(x=x,y=y)) + geom_point(colour="black",pch=4) + xlim(580,600) + plotgaus(fit2,range2) + plotgausline(fit2)


#Wavelength of the laser
daten=read.csv("../data/osa/004.csv",sep=",",skip=33,header=FALSE,col.names=c("lambda","I"),dec=".")
os_laser=data.frame(x=daten[[1]],y=daten[[2]])

pos=c()
spos=c()
sigpos=c()

range1=c(516,519);
fit1=posgausfit(os_laser,range1,sig0=0.5,N0=0.3);
pos=append(pos,fit1["mu","Estimate"])
spos=append(spos,fit1["mu","Std. Error"])
sigpos=append(sigpos,fit1["sig","Estimate"]/2)
range2=c(1030,1040);
fit2=posgausfit(os_laser,range2,N0=1.0);
pos=append(pos,fit2["mu","Estimate"])
spos=append(spos,fit2["mu","Std. Error"])
sigpos=append(sigpos,fit2["sig","Estimate"]/2)

labeldata=data.frame(x=pos,y=rep(0.9,length(pos)),text=1:length(pos))

ggplot(os_laser,aes(x=x,y=y)) + geom_line(colour="black") + plotgaus(fit1,range1) + plotgausline(fit1) + 
                                                            plotgaus(fit2,range2) + plotgausline(fit2) +
                                                            geom_label(data=labeldata,aes(x=x,y=y,label=text),colour="red") +
                                                            xlab(expression(lambda/nm)) + ylab(expression(I))
