gausfit <- function(input,bereich,sig0=0,N0=0) {
  
  thegaussian <- y ~ C + N/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2))
  
  daten=subset(input,x>=bereich[1] & x<= bereich[2])
  
  ymin=min(daten$y)
  if(N0==0){
    ymax=max(daten$y)
    N0=ymin-ymax
  }
  mu0 =daten$x[which.min(daten$y)]
  if(sig0==0)
  {
    #sig0=(daten$x[bereich[2]]-daten$x[bereich[1]])/3
    sig0=(bereich[2]-bereich[1])/6
  }

  fit = nls(thegaussian,daten,start=list(C=ymax,N=N0,mu=mu0,sig=sig0))
  
  return(summary(fit)$parameters)
} 
posgausfit <- function(input,bereich,sig0=0,N0=0) {
  
  thegaussian <- y ~ C + N/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2))
  
  daten=subset(input,x>=bereich[1] & x<= bereich[2])
  
  ymin=min(daten$y)
  if(N0==0){
    ymax=max(daten$y)
    N0=ymax-ymin
  }
  mu0 =daten$x[which.max(daten$y)]
  if(sig0==0)
  {
    #sig0=(daten$x[bereich[2]]-daten$x[bereich[1]])/3
    sig0=(bereich[2]-bereich[1])/3
  }
  
  fit = nls(thegaussian,daten,start=list(C=ymin,N=N0,mu=mu0,sig=sig0))
  
  return(summary(fit)$parameters)
} 
dualgausfit <- function(input,bereich,sig0=0,N0=0) {
  
  thegaussian <- y ~ C + N/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2))+N2/(sqrt(2*pi)*sig2)*exp(-(x-mu2)^2/(2*sig2^2))
  
  daten=subset(input,x>=bereich[1] & x<= bereich[2])
  
  ymin=min(daten$y)
  if(N0==0){
    ymax=max(daten$y)
  } else {
    ymax=N0
  }
  mu0 =daten$x[which.min(daten$y)]
  if(sig0==0)
  {
    #sig0=(daten$x[bereich[2]]-daten$x[bereich[1]])/3
    sig0=(bereich[2]-bereich[1])/6
  }
  
  fit = nls(thegaussian,daten,start=list(C=ymax,N=ymin-ymax,N2=ymin-ymax,mu=mu0-0.1,mu2=mu0+0.1,sig=sig0, sig2=sig0))
  
  return(summary(fit)$parameters)
} 

plotgausline <- function(params) {
  line=data.frame(y=c(0,1),x=c(params["mu","Estimate"],params["mu","Estimate"]))
  geom_line(data=line,colour="red",linetype=2)
}
plotgaus <- function(fitdata,bereich){ #--- Plotten der gefitteten Gaussfunktion in vorhandenen Graph
  
  N<-fitdata["N","Estimate"]
  C<-fitdata["C","Estimate"]
  mu<-fitdata["mu","Estimate"]
  sig<-fitdata["sig","Estimate"]
  
  stat_function (fun=function(x){C + N/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2))},xlim=c(bereich[1],bereich[2]),colour="blue")
  
}

plotdualgausline <- function(params) {
  line=data.frame(y=c(0,1),x=c(params["mu","Estimate"],params["mu","Estimate"]))
  geom_line(data=line,colour="red",linetype=2)
  line2=data.frame(y=c(0,1),x=c(params["mu2","Estimate"],params["mu2","Estimate"]))
  geom_line(data=line2,colour="red",linetype=2)
}
plotdualgaus <- function(fitdata,bereich){ #--- Plotten der gefitteten Gaussfunktion in vorhandenen Graph
  
  N<-fitdata["N","Estimate"]
  N2<-fitdata["N2","Estimate"]
  C<-fitdata["C","Estimate"]
  mu<-fitdata["mu","Estimate"]
  mu2<-fitdata["mu2","Estimate"]
  sig<-fitdata["sig","Estimate"]
  sig2<-fitdata["sig2","Estimate"]
  
  stat_function (fun=function(x){C + N/(sqrt(2*pi)*sig)*exp(-(x-mu)^2/(2*sig^2))},xlim=c(bereich[1],bereich[2]),colour="blue")
  
}
