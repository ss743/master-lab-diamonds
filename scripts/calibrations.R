library(ggplot2)
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
