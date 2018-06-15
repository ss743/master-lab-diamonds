library(ggplot2)

average_csv <- function(filename){
  daten=read.csv(filename,sep=",",skip=0,header=TRUE,dec=".");
  L=length(daten$time);
  N=(length(daten)-1)/2;
  if(filename=="../data/osci/odmr_001.csv"){
    N=(length(daten)-1)/3;
  }
  avg=array(dim=c(L,3));
  for(i in 1:L){
    ch1=0;
    ch2=0;
    #ch3=0;
    t=daten$time[i];
    for(j in 1:N){
      ch1=ch1+1/N*daten[i,sprintf("ch1m%i",j)];
      ch2=ch2+1/N*daten[i,sprintf("ch2m%i",j)];
      #ch3=ch3+1/N*daten[i,sprintf("ch3m%i",j)];
    }
    avg[i,]=c(t,ch1,ch2)#,ch3)
  }
  daten=data.frame(t=avg[,1],ch1=avg[,2],ch2=avg[,3]);
  return(daten);
}

change_units <- function(avg){
  time=avg$t;
  freq=avg$t*1.003+2.708;
  err=sqrt(0.003^2*time^2+0.011^2)
  spectrum=data.frame(f=freq,sf=err,ch1=avg$ch1,ch2=avg$ch2)
  return(spectrum);
}

plot_avg <- function(avg){
  ggplot(avg,aes(x=t,y=ch1)) + geom_point(pch=4,colour="black") + ylab("U / V") + xlab("t / s") + xlim(-0.2,0.45)
}

plot_spec <- function(avg){
  ggplot(avg,aes(x=f,y=ch1)) + geom_point(pch=4,colour="black") + ylab("U / V") + xlab("f / GHz")
}

gausfit <- function(avg,bereich,sig0=0,N0=0) {
  
  thegaussian <- ch1 ~ C + N/(sqrt(2*pi)*sig)*exp(-(t-mu)^2/(2*sig^2))
  
  daten=subset(avg,t>=bereich[1] & t<= bereich[2])
  
  ymin=min(daten$ch1)
  if(N0==0){
    ymax=max(daten$ch1)
    N0=ymin-ymax
  }
  mu0 =daten$t[which.min(daten$ch1)]
  if(sig0==0)
  {
    #sig0=(daten$x[bereich[2]]-daten$x[bereich[1]])/3
    sig0=(bereich[2]-bereich[1])/6
  }
  
  fit = nls(thegaussian,daten,start=list(C=ymax,N=N0,mu=mu0,sig=sig0))
  
  return(summary(fit)$parameters)
} 

fgausfit <- function(avg,bereich,sig0=0,N0=0) {
  
  thegaussian <- ch1 ~ C + N/(sqrt(2*pi)*sig)*exp(-(f-mu)^2/(2*sig^2))
  
  daten=subset(avg,f>=bereich[1] & f<= bereich[2])
  
  ymin=min(daten$ch1)
  if(N0==0){
    ymax=max(daten$ch1)
    N0=ymin-ymax
  }
  mu0 =daten$f[which.min(daten$ch1)]
  if(sig0==0)
  {
    #sig0=(daten$x[bereich[2]]-daten$x[bereich[1]])/3
    sig0=(bereich[2]-bereich[1])/6
  }
  
  fit = nls(thegaussian,daten,start=list(C=ymax,N=N0,mu=mu0,sig=sig0))
  
  return(summary(fit)$parameters)
} 


plotgausline <- function(params,height=1,zero=0) {
  line=data.frame(ch1=c(zero,zero+height),f=c(params["mu","Estimate"],params["mu","Estimate"]))
  geom_line(data=line,colour="red",linetype=2)
}

plotgaus <- function(fitdata,bereich){ #--- Plotten der gefitteten Gaussfunktion in vorhandenen Graph
  
  N<-fitdata["N","Estimate"]
  C<-fitdata["C","Estimate"]
  mu<-fitdata["mu","Estimate"]
  sig<-fitdata["sig","Estimate"]
  
  stat_function (fun=function(x){C + N/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2))},xlim=c(bereich[1],bereich[2]),colour="blue")
  
}

