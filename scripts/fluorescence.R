library(ggplot2)
source("gausfit.R")

daten=read.csv(sprintf("../data/osa/012-001.csv",i),sep=",",skip=33,header=FALSE,col.names=c("lambda","I"),dec=".")
l=length(daten[[1]])

intensity=matrix(nrow=l,ncol=10)

for(i in 1:10)
{
daten=read.csv(sprintf("../data/osa/012-%03i.csv",i),sep=",",skip=33,header=FALSE,col.names=c("lambda","I"),dec=".")
x=daten[[1]]
intensity[,i]=daten[[2]]
}
int_avg=c()
for(i in 1:length(intensity[,1]))
{
  int_avg[i]=mean(intensity[i,])
}

fluorescence = data.frame(x=x,y=int_avg)
fluorescence1 = data.frame(x=x,y=intensity[,1])

pos=c()
spos=c()
sigpos=c()

range1=c(640,650);
fit1=posgausfit(fluorescence1,range1,sig0=1.5);
pos=append(pos,fit1["mu","Estimate"])
spos=append(spos,fit1["mu","Std. Error"])
sigpos=append(sigpos,fit1["sig","Estimate"]/2)


ggplot(fluorescence,aes(x=x,y=y)) + geom_line(colour="black") +  xlab(expression(lambda/nm)) + ylab(expression(I)) + plotgausline(fit1,0.5)
