f0<-2.8699
sf0<-0.0002
fx<-c(2.8803,2.8930)
sfx<-c(0.0004,0.0008)
fy<-c(2.8190,2.8520,2.9214,2.9518)
sfy<-c(0.0003,0.0003,0.0004,0.0004)
fz<-c(2.7721,2.8801,2.9483)
sfz<-c(0.0010,0.0006,0.0008)
fc<-mean(c(fx,fy));
sfc<-1/(length(c(fy,fy))-1)*var(c(fy,fy))

dfx <- fx-fc;
sdfx <- sqrt(sfc^2+sfx^2);
dfy <- fy-fc;
sdfy <- sqrt(sfc^2+sfy^2);
dfz <- fz-f0;
sdfz <- sqrt(sfc^2+sfz^2);

gamma <- 28.951 #GHz/T

Bx <- dfx/gamma *10^3 #mT
sBx <- sdfx/gamma *10^3 #mT
By <- dfy/gamma *10^3 #mT
sBy <- sdfy/gamma *10^3 #mT
Bz <- dfz/gamma *10^3 #mT
sBz <- sdfz/gamma *10^3 #mT

for(i in 1:length(Bx))  print(sprintf("Bx_%i=%.2f+-%.2f",i,Bx[i],sBx[i]))
for(i in 1:length(By))  print(sprintf("By_%i=%.3f+-%.3f",i,By[i],sBy[i]))
for(i in 1:length(Bz))  print(sprintf("Bz_%i=%.2f+-%.2f",i,Bz[i],sBz[i]))

dfx=abs(dfx)
dfy=abs(dfy)

dfX=(dfx[1]/sdfx[1]^2+dfx[2]/sdfx[2]^2)/(1/sdfx[1]^2+1/sdfx[2]^2)
sdfX=sqrt(1/(1/sdfx[1]^2+1/sdfx[2]^2))

dfY1=(dfy[1]/sdfy[1]^2+dfy[4]/sdfy[4]^2)/(1/sdfy[1]^2+1/sdfy[4]^2)
sdfY1=sqrt(1/(1/sdfy[1]^2+1/sdfy[4]^2))
dfY2=(dfy[3]/sdfy[3]^2+dfy[2]/sdfy[2]^2)/(1/sdfy[3]^2+1/sdfy[2]^2)
sdfY2=sqrt(1/(1/sdfy[3]^2+1/sdfy[2]^2))

BX <- dfX/gamma *10^3 #mT
sBX <- sdfX/gamma *10^3 #mT
BY1 <- dfY1/gamma *10^3 #mT
sBY1 <- sdfY1/gamma *10^3 #mT
BY2 <- dfY2/gamma *10^3 #mT
sBY2 <- sdfY2/gamma *10^3 #mT

print("")
print(sprintf("Bx=%.3f+-%.3f",BX,sBX))
print(sprintf("By_1=%.3f+-%.3f",BY1,sBY1))
print(sprintf("By_2=%.3f+-%.3f",BY2,sBY2))

B0x=5.9 #mT
B0y=6.0 #mT

print("")
print(sprintf("Bx/B0=%.3f+-%.3f",BX/B0x,sBX/B0x))
print(sprintf("By_1/B0=%.3f+-%.3f",BY1/B0x,sBY1/B0x))
print(sprintf("By_2/B0=%.3f+-%.3f",BY2/B0x,sBY2/B0x))