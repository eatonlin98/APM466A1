library(readxl)
sl=read.csv("/Users/86745/Desktop/APM466/APM466 Selected2.csv")
head(sl)
options
head(sl)
sl
attach(sl)
#install.packages("jrvFinance")
library(jrvFinance)
coupon=c(0.0175,0.0150,0.0150,0.0025,0.0025,0.0150,0.0225,0.0150,0.0125,0.0050)
date=c("2021-01-18",	"2021-01-19","2021-01-20",	"2021-01-21",	"2021-01-22",	"2021-01-25",	"2021-01-26",	"2021-01-27",	"2021-01-28",	"2021-01-29")
mature=c("2021-05-01","2021-08-01","2022-02-01","2022-08-01","2023-02-01","2023-06-01","2024-03-01","2024-09-01","2025-03-01","2025-09-01")

a=1
ytm1=vector()
bond1=sl[1,c(5:14)]
bond1
for(i in 1:10){ytm1[a]=bond.yield(settle=date[i],mature = "2021-05-01",coupon=0.0175,price=bond1[i],comp.freq=2,redemption_value = 100)
a=a+1}
ytm1

b=1
ytm2=vector()
bond2=sl[2,c(5:14)]
bond2
bond2=unlist(bond2)
for(i in 1:10){ytm2[b]=bond.yield(settle=date[i],mature = "2021-08-01",coupon=0.0150,price=bond2[i],comp.freq=2,redemption_value = 100) 
b=b+1}
ytm2

a=1
ytm3=vector()
bond3=sl[3,c(5:14)]
bond3
bond3=unlist(bond3)
for(i in 1:10){ytm3[a]=bond.yield(settle=date[i],mature = "2022-02-01",coupon=0.0150,price=bond3[i],comp.freq=2,redemption_value = 100)
a=a+1}
ytm3

a=1
ytm4=vector()
bond4=sl[4,c(5:14)]
bond4
bond4=unlist(bond4)
for(i in 1:10){ytm4[a]=bond.yield(settle=date[i],mature = "2022-08-01",coupon=0.0025,price=bond4[i],comp.freq=2,redemption_value = 100)
a=a+1}
ytm4


a=1
ytm5=vector()
bond5=sl[5,c(5:14)]
bond5
bond5=unlist(bond5)
for(i in 1:10){ytm5[a]=bond.yield(settle=date[i],mature = "2023-02-01",coupon=0.0025,price=bond5[i],comp.freq=2,redemption_value = 100)
a=a+1}
ytm5


a=1
ytm6=vector()
bond6=sl[6,c(5:14)]
bond6
bond6=unlist(bond6)
for(i in 1:10){ytm6[a]=bond.yield(settle=date[i],mature = "2023-06-01",coupon=0.0150,price=bond6[i],comp.freq=2,redemption_value = 100)
a=a+1}
ytm6


a=1
ytm7=vector()
bond7=sl[7,c(5:14)]
bond7
bond7=unlist(bond7)
for(i in 1:10){ytm7[a]=bond.yield(settle=date[i],mature = "2024-03-01",coupon=0.0225,price=bond7[i],comp.freq=2,redemption_value = 100)
a=a+1}
ytm7

a=1
ytm8=vector()
bond8=sl[8,c(5:14)]
bond8
bond8=unlist(bond8)
for(i in 1:10){ytm8[a]=bond.yield(settle=date[i],mature = "2024-09-01",coupon=0.0150,price=bond8[i],comp.freq=2,redemption_value = 100)
a=a+1}
ytm8

a=1
ytm9=vector()
bond9=sl[9,c(5:14)]
bond9
bond9=unlist(bond9)
for(i in 1:10){ytm9[a]=bond.yield(settle=date[i],mature = "2025-03-01",coupon=0.0125,price=bond9[i],comp.freq=2,redemption_value = 100)
a=a+1}
ytm9

a=1
ytm10=vector()
bond10=sl[10,c(5:14)]
bond10
bond10=unlist(bond10)
for(i in 1:10){ytm10[a]=bond.yield(settle=date[i],mature = "2025-09-01",coupon=0.0050,price=bond10[i],comp.freq=2,redemption_value = 100)
a=a+1}
ytm10

#install.packages("lattice")
library(lattice)
#install.packages("survival")
library(survival)
#installed.packages("Formula")
library(Formula)
#installed.packages("ggplot2")
library(ggplot2)


YTM<- matrix(c(ytm1,ytm2,ytm3,ytm4,ytm5,ytm6,ytm7,ytm8,ytm9,ytm10),nrow=10,byrow=FALSE)
YTM=t(YTM)
YTM
#install.packages("xlsx")

YTMrow<- c(ytm1,ytm2,ytm3,ytm4,ytm5,ytm6,ytm7,ytm8,ytm9,ytm10)
bond1ttm=vector()
for (i in 1:10){bond1ttm[i]=yearFraction(date[i],"2021-03-01")}
bond1ttm

ttm1=vector()
for(i in 1:10){
  sd=as.Date("2021-05-01")
  ed=as.Date(date[i])
  ttm1[i]=(sd-ed)/365
}
ttm1
as.data.frame(ttm1)

ttm2=vector()
for(i in 1:10){
  sd=as.Date("2021-08-01")
  ed=as.Date(date[i])
  ttm2[i]=(sd-ed)/365
}
ttm2

ttm3=vector()
for(i in 1:10){
  sd=as.Date("2022-02-01")
  ed=as.Date(date[i])
  ttm3[i]=(sd-ed)/365
}
ttm3

ttm4=vector()
for(i in 1:10){
  sd=as.Date("2022-08-01")
  ed=as.Date(date[i])
  ttm4[i]=(sd-ed)/365
}
ttm4

ttm5=vector()
for(i in 1:10){
  sd=as.Date("2023-02-01")
  ed=as.Date(date[i])
  ttm5[i]=(sd-ed)/365
}
ttm5
datel=c("2021-05-01",
        "2021-08-01",
        "2022-03-01",
        "2022-06-01",
        "2023-03-01",
        "2023-06-01",
        "2024-03-01",
        "2024-09-01",
        "2025-03-01",
        "2025-09-01"
)
ttm6=vector()
for(i in 1:10){
  sd=as.Date("2023-06-01")
  ed=as.Date(date[i])
  ttm6[i]=(sd-ed)/365
}
ttm6
ttm7=vector()
for(i in 1:10){
  sd=as.Date("2024-03-01")
  ed=as.Date(date[i])
  ttm7[i]=(sd-ed)/365
}
ttm7
ttm8=vector()
for(i in 1:10){
  sd=as.Date("2024-09-01")
  ed=as.Date(date[i])
  ttm8[i]=(sd-ed)/365
}
ttm8
ttm9=vector()
for(i in 1:10){
  sd=as.Date("2025-03-01")
  ed=as.Date(date[i])
  ttm9[i]=(sd-ed)/365
}
ttm9
ttm9=vector()
for(i in 1:10){
  sd=as.Date("2025-03-01")
  ed=as.Date(date[i])
  ttm9[i]=(sd-ed)/365
}
ttm9
ttm10=vector()
for(i in 1:10){
  sd=as.Date("2025-09-01")
  ed=as.Date(date[i])
  ttm10[i]=(sd-ed)/365
}
ttm10
library(ggplot2)
plotdata=read.csv("/Users/86745/Desktop/APM466/APM466 plot.csv",header = TRUE)
plotdata=data.frame(plotdata)

pttm=plotdata$time_to_mat
pttm=data.frame(pttm,stringsAsFactors = F) 
#class(pttm)

pytm=plotdata$ytm
pytm=data.frame(pytm,stringsAsFactors = F)
#class(pytm)

pdate=as.data.frame(plotdata$date)

#class(pdate)

ggplot(plotdata,aes(x=plotdata$time_to_mat,y=plotdata$ytm,group=plotdata$date, color=plotdata$date))+
         geom_line()+xlab("Time to mat")+ylab("Yield to mat")+labs(colours="date")

#4b

p1=vector()
bond1=sl[1,c(5:14)]
for(i in 1:10){p1[i]=bond1[i]+bond.TCF(settle=date[i],mature = "2021-05-01",coupon=0.0175,freq = 2)$accrued
}
p1=t(p1)

p2=vector()
bond2=sl[2,c(5:14)]
for(i in 1:10){p2[i]=bond2[i]+bond.TCF(settle=date[i],mature = "2021-08-01",coupon=0.0150,freq = 2)$accrued
}
p2=t(p2)
p2

p3=vector()
bond3=sl[3,c(5:14)]
for(i in 1:10){p3[i]=bond3[i]+bond.TCF(settle=date[i],mature = "2022-02-01",coupon=0.0150,freq = 2)$accrued
}
p3=t(p3)

p4=vector()
bond4=sl[4,c(5:14)]
for(i in 1:10){p4[i]=bond4[i]+bond.TCF(settle=date[i],mature = "2022-08-01",coupon=0.0025,freq = 2)$accrued
}
p4=t(p4)

p5=vector()
bond5=sl[5,c(5:14)]
for(i in 1:10){p5[i]=bond5[i]+bond.TCF(settle=date[i],mature = "2023-02-01",coupon=0.0025,freq = 2)$accrued
}
p5=t(p5)

p6=vector()
bond6=sl[6,c(5:14)]
for(i in 1:10){p6[i]=bond6[i]+bond.TCF(settle=date[i],mature = "2023-06-01",coupon=0.0150,freq = 2)$accrued
}
p6=t(p6)

p7=vector()
bond7=sl[7,c(5:14)]
for(i in 1:10){p7[i]=bond7[i]+bond.TCF(settle=date[i],mature = "2024-03-01",coupon=0.0225,freq = 2)$accrued
}
p7=t(p7)

p8=vector()
bond8=sl[8,c(5:14)]
for(i in 1:10){p8[i]=bond8[i]+bond.TCF(settle=date[i],mature = "2024-09-01",coupon=0.0150,freq = 2)$accrued
}
p8
p8=t(p8)
p9=vector()
bond9=sl[9,c(5:14)]
for(i in 1:10){p9[i]=bond9[i]+bond.TCF(settle=date[i],mature = "2025-03-01",coupon=0.0125,freq = 2)$accrued
}
p9=t(p9)

p10=vector()
bond10=sl[10,c(5:14)]
for(i in 1:10){p10[i]=bond10[i]+bond.TCF(settle=date[i],mature = "2025-09-01",coupon=0.0050,freq = 2)$accrued
}
p10=t(p10)

m1=matrix()
m1=matrix(c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10),nrow=10,ncol=10)
m1=t(m1)
m1
m1=data.frame(m1)


m12=c(101.4558,   101.45 ,101.4342 ,101.4283, 101.4325 , 101.435 ,101.4392, 101.4433 ,101.4375 ,101.4417)



pa1=m1[2,1]
pa1=as.numeric(pa1)
coupona=0.015*100/2
coupona=as.numeric(coupona)
fa=function(x)(coupona+(coupona+100)*(1+x/2)^(-0.5)-pa1)
ra1=uniroot(fa,lower=-1,upper=1)$root
pa2=m1[3,1]
pa2=as.numeric(pa2)
fa2=function(x)(coupona+coupona*(1+ra1/2)^(-0.5)+(coupona+100)*(1+x/2)^(-2*0.5)-pa2)
ra2=uniroot(fa2,lower=0,upper=1)$root
pa3=m1[4,1]
pa3=as.numeric(pa3)
couponb=0.0025*100/2
couponb=as.numeric(couponb)
fa3=function(x)(couponb+couponb*(1+ra1/2)^(-0.5)+couponb*(1+ra2/2)^(-2*0.5)+(couponb+100)*(1+x/2)^(-3*0.5)-pa3)
ra3=uniroot(fa3,lower=0,upper=1)$root
pa4=m1[5,1]
pa4=as.numeric(pa4)
fa4=function(x)(couponb+couponb*(1+ra1/2)^(-0.5)+couponb*(1+ra2/2)^(-2*0.5)+couponb*(1+ra3/2)^(-3*0.5)+(couponb+100)*(1+x/2)^(-4*0.5)-pa4)
ra4=uniroot(fa4,lower=0,upper=1)$root
jan18=vector()
jan18=c(ra1,ra2,ra3,ra4)
jan18
x=c(0.5,1,1.5,2)
func=splinefun(x=x,y=jan18,method="fmm",ties = mean)
l1=func(seq(0.5,5,0.5))
l1=t(l1)
l1
#forward rate Jan18
func=splinefun(x=x,y=jan18,method="fmm",ties = mean)
l1a=func(seq(0.5,5,1))
l1a=t(l1a)

f12_18=((1+l1a[2])^2/(1+l1a[1]))-1
f13_18=(((1+l1a[3])^3)/(1+l1a[1]))^(1/2)-1
f14_18=((1+l1a[4])^4/(1+l1a[1]))^(1/3)-1
f15_18=((1+l1a[5])^5/(1+l1a[1]))^(1/4)-1
c(f12_18,f13_18,f14_18,f15_18)

#jan19
pb1=m1[2,2]
pb1=as.numeric(pb1)
coupona=0.015*100/2
coupona=as.numeric(coupona)
fb=function(x)(coupona+(coupona+100)*(1+x/2)^(-0.5)-pb1)
rb1=uniroot(fb,lower=-1,upper=1)$root
pb2=m1[3,2]
pb2=as.numeric(pb2)
fb2=function(x)(coupona+coupona*(1+rb1/2)^(-0.5)+(coupona+100)*(1+x/2)^(-2*0.5)-pb2)
rb2=uniroot(fb2,lower=0,upper=1)$root
pb3=m1[4,2]
pb3=as.numeric(pb3)
couponb=0.0025*100/2
couponb=as.numeric(couponb)
fb3=function(x)(couponb+couponb*(1+rb1/2)^(-0.5)+couponb*(1+rb2/2)^(-2*0.5)+(couponb+100)*(1+x/2)^(-3*0.5)-pb3)
rb3=uniroot(fb3,lower=0,upper=1)$root
pb4=m1[5,2]
pb4=as.numeric(pb4)
fb4=function(x)(couponb+couponb*(1+rb1/2)^(-0.5)+couponb*(1+rb2/2)^(-2*0.5)+couponb*(1+rb3/2)^(-3*0.5)+(couponb+100)*(1+x/2)^(-4*0.5)-pb4)
rb4=uniroot(fb4,lower=0,upper=1)$root
jan19=vector()
jan19=c(rb1,rb2,rb3,rb4)
jan19
x=c(0.5,1,1.5,2)
func=splinefun(x=x,y=jan19,method="fmm",ties = mean)
l2=func(seq(0.5,5,0.5))
l2=t(l2)
l2
#forward rate Jan19
func=splinefun(x=x,y=jan19,method="fmm",ties = mean)
l2a=func(seq(0.5,5,1))
l2a=t(l2a)

f12_19=((1+l2a[2])^2/(1+l2a[1]))-1
f13_19=(((1+l2a[3])^3)/(1+l2a[1]))^(1/2)-1
f14_19=((1+l2a[4])^4/(1+l2a[1]))^(1/3)-1
f15_19=((1+l2a[5])^5/(1+l2a[1]))^(1/4)-1
c(f12_19,f13_19,f14_19,f15_19)


pc1=m1[2,3]
pc1=as.numeric(pc1)
coupona=0.015*100/2
coupona=as.numeric(coupona)
fc=function(x)(coupona+(coupona+100)*(1+x/2)^(-0.5)-pc1)
rc1=uniroot(fc,lower=-1,upper=1)$root
pc2=m1[3,3]
pc2=as.numeric(pc2)
fc2=function(x)(coupona+coupona*(1+rc1/2)^(-0.5)+(coupona+100)*(1+x/2)^(-2*0.5)-pc2)
rc2=uniroot(fc2,lower=0,upper=1)$root
pc3=m1[4,3]
pc3=as.numeric(pc3)
couponb=0.0025*100/2
couponb=as.numeric(couponb)
fc3=function(x)(couponb+couponb*(1+rc1/2)^(-0.5)+couponb*(1+rc2/2)^(-2*0.5)+(couponb+100)*(1+x/2)^(-3*0.5)-pc3)
rc3=uniroot(fc3,lower=0,upper=1)$root
pc4=m1[5,3]
pc4=as.numeric(pc4)
fc4=function(x)(couponb+couponb*(1+rc1/2)^(-0.5)+couponb*(1+rc2/2)^(-2*0.5)+couponb*(1+rc3/2)^(-3*0.5)+(couponb+100)*(1+x/2)^(-4*0.5)-pc4)
rc4=uniroot(fc4,lower=0,upper=1)$root
jan20=vector()
jan20=c(rc1,rc2,rc3,rc4)
jan20
x=c(0.5,1,1.5,2)
func=splinefun(x=x,y=jan20,method="fmm",ties = mean)
l3=func(seq(0.5,5,0.5))
l3=t(l3)
l3
#forward rate Jan20
func=splinefun(x=x,y=jan20,method="fmm",ties = mean)
l3a=func(seq(0.5,5,1))
l3a=t(l3a)

f12_20=((1+l3a[2])^2/(1+l3a[1]))-1
f13_20=(((1+l3a[3])^3)/(1+l3a[1]))^(1/2)-1
f14_20=((1+l3a[4])^4/(1+l3a[1]))^(1/3)-1
f15_20=((1+l3a[5])^5/(1+l3a[1]))^(1/4)-1
c(f12_20,f13_20,f14_20,f15_20)

pd1=m1[2,4]
pd1=as.numeric(pd1)
coupona=0.015*100/2
coupona=as.numeric(coupona)
fd=function(x)(coupona+(coupona+100)*(1+x/2)^(-0.5)-pd1)
rd1=uniroot(fd,lower=-1,upper=1)$root
pd2=m1[3,4]
pd2=as.numeric(pd2)
fd2=function(x)(coupona+coupona*(1+rd1/2)^(-0.5)+(coupona+100)*(1+x/2)^(-2*0.5)-pd2)
rd2=uniroot(fd2,lower=0,upper=1)$root
pd3=m1[4,4]
pd3=as.numeric(pd3)
couponb=0.0025*100/2
couponb=as.numeric(couponb)
fd3=function(x)(couponb+couponb*(1+rd1/2)^(-0.5)+couponb*(1+rd2/2)^(-2*0.5)+(couponb+100)*(1+x/2)^(-3*0.5)-pd3)
rd3=uniroot(fd3,lower=0,upper=1)$root
pd4=m1[5,4]
pd4=as.numeric(pd4)
fd4=function(x)(couponb+couponb*(1+rd1/2)^(-0.5)+couponb*(1+rd2/2)^(-2*0.5)+couponb*(1+rd3/2)^(-3*0.5)+(couponb+100)*(1+x/2)^(-4*0.5)-pd4)
rd4=uniroot(fd4,lower=0,upper=1)$root
jan21=vector()
jan21=c(rd1,rd2,rd3,rd4)
x=c(0.5,1,1.5,2)
func=splinefun(x=x,y=jan21,method="fmm",ties = mean)
l4=func(seq(0.5,5,0.5))
l4=t(l4)
l4
#forward rate Jan21
func=splinefun(x=x,y=jan21,method="fmm",ties = mean)
l4a=func(seq(0.5,5,1))
l4a=t(l4a)

f12_21=((1+l4a[2])^2/(1+l4a[1]))-1
f13_21=(((1+l4a[3])^3)/(1+l4a[1]))^(1/2)-1
f14_21=((1+l4a[4])^4/(1+l4a[1]))^(1/3)-1
f15_21=((1+l4a[5])^5/(1+l4a[1]))^(1/4)-1
c(f12_21,f13_21,f14_21,f15_21)


pe1=m1[2,5]
pe1=as.numeric(pe1)
coupona=0.015*100/2
coupona=as.numeric(coupona)
fe=function(x)(coupona+(coupona+100)*(1+x/2)^(-0.5)-pe1)
re1=uniroot(fe,lower=-1,upper=1)$root
pe2=m1[3,5]
pe2=as.numeric(pe2)
fe2=function(x)(coupona+coupona*(1+re1/2)^(-0.5)+(coupona+100)*(1+x/2)^(-2*0.5)-pe2)
re2=uniroot(fe2,lower=0,upper=1)$root
pe3=m1[4,5]
pe3=as.numeric(pe3)
couponb=0.0025*100/2
couponb=as.numeric(couponb)
fe3=function(x)(couponb+couponb*(1+re1/2)^(-0.5)+couponb*(1+re2/2)^(-2*0.5)+(couponb+100)*(1+x/2)^(-3*0.5)-pe3)
re3=uniroot(fe3,lower=0,upper=1)$root
pe4=m1[5,5]
pe4=as.numeric(pe4)
fe4=function(x)(couponb+couponb*(1+re1/2)^(-0.5)+couponb*(1+re2/2)^(-2*0.5)+couponb*(1+re3/2)^(-3*0.5)+(couponb+100)*(1+x/2)^(-4*0.5)-pe4)
re4=uniroot(fe4,lower=0,upper=1)$root
jan22=vector()
jan22=c(re1,re2,re3,re4)
x=c(0.5,1,1.5,2)
func=splinefun(x=x,y=jan22,method="fmm",ties = mean)
l5=func(seq(0.5,5,0.5))
l5=t(l5)
l5
#forward rate Jan22
func=splinefun(x=x,y=jan22,method="fmm",ties = mean)
l5a=func(seq(0.5,5,1))
l5a=t(l5a)

f12_22=((1+l5a[2])^2/(1+l5a[1]))-1
f13_22=(((1+l5a[3])^3)/(1+l5a[1]))^(1/2)-1
f14_22=((1+l5a[4])^4/(1+l5a[1]))^(1/3)-1
f15_22=((1+l5a[5])^5/(1+l5a[1]))^(1/4)-1
c(f12_22,f13_22,f14_22,f15_22)


pf1=m1[2,6]
pf1=as.numeric(pf1)
coupona=0.015*100/2
coupona=as.numeric(coupona)
ff=function(x)(coupona+(coupona+100)*(1+x/2)^(-0.5)-pf1)
rf1=uniroot(ff,lower=-1,upper=1)$root
pf2=m1[3,6]
pf2=as.numeric(pf2)
ff2=function(x)(coupona+coupona*(1+rf1/2)^(-0.5)+(coupona+100)*(1+x/2)^(-2*0.5)-pf2)
rf2=uniroot(ff2,lower=0,upper=1)$root
pf3=m1[4,6]
pf3=as.numeric(pf3)
couponb=0.0025*100/2
couponb=as.numeric(couponb)
ff3=function(x)(couponb+couponb*(1+rf1/2)^(-0.5)+couponb*(1+rf2/2)^(-2*0.5)+(couponb+100)*(1+x/2)^(-3*0.5)-pf3)
rf3=uniroot(ff3,lower=0,upper=1)$root
pf4=m1[5,6]
pf4=as.numeric(pf4)
ff4=function(x)(couponb+couponb*(1+rf1/2)^(-0.5)+couponb*(1+rf2/2)^(-2*0.5)+couponb*(1+rf3/2)^(-3*0.5)+(couponb+100)*(1+x/2)^(-4*0.5)-pf4)
rf4=uniroot(ff4,lower=0,upper=1)$root
jan25=vector()
jan25=c(rf1,rf2,rf3,rf4)
x=c(0.5,1,1.5,2)
func=splinefun(x=x,y=jan25,method="fmm",ties = mean)
l6=func(seq(0.5,5,0.5))
l6=t(l6)
l6
#forward rate Jan25
func=splinefun(x=x,y=jan25,method="fmm",ties = mean)
l6a=func(seq(0.5,5,1))
l6a=t(l6a)

f12_25=((1+l6a[2])^2/(1+l6a[1]))-1
f13_25=(((1+l6a[3])^3)/(1+l6a[1]))^(1/2)-1
f14_25=((1+l6a[4])^4/(1+l6a[1]))^(1/3)-1
f15_25=((1+l6a[5])^5/(1+l6a[1]))^(1/4)-1
c(f12_25,f13_25,f14_25,f15_25)


pg1=m1[2,7]
pg1=as.numeric(pg1)
coupona=0.015*100/2
coupona=as.numeric(coupona)
fg=function(x)(coupona+(coupona+100)*(1+x/2)^(-0.5)-pg1)
rg1=uniroot(fg,lower=-1,upper=1)$root
pg2=m1[3,7]
pg2=as.numeric(pg2)
fg2=function(x)(coupona+coupona*(1+rg1/2)^(-0.5)+(coupona+100)*(1+x/2)^(-2*0.5)-pg2)
rg2=uniroot(fg2,lower=0,upper=1)$root
pg3=m1[4,7]
pg3=as.numeric(pg3)
couponb=0.0025*100/2
couponb=as.numeric(couponb)
fg3=function(x)(couponb+couponb*(1+rg1/2)^(-0.5)+couponb*(1+rg2/2)^(-2*0.5)+(couponb+100)*(1+x/2)^(-3*0.5)-pg3)
rg3=uniroot(fg3,lower=0,upper=1)$root
pg4=m1[5,7]
pg4=as.numeric(pg4)
fg4=function(x)(couponb+couponb*(1+rg1/2)^(-0.5)+couponb*(1+rg2/2)^(-2*0.5)+couponb*(1+rg3/2)^(-3*0.5)+(couponb+100)*(1+x/2)^(-4*0.5)-pg4)
rg4=uniroot(fg4,lower=0,upper=1)$root
jan26=vector()
jan26=c(rg1,rg2,rg3,rg4)
x=c(0.5,1,1.5,2)
func=splinefun(x=x,y=jan26,method="fmm",ties = mean)
l7=func(seq(0.5,5,0.5))
l7=t(l7)
l7
#forward rate Jan26
func=splinefun(x=x,y=jan26,method="fmm",ties = mean)
l7a=func(seq(0.5,5,1))
l7a=t(l7a)

f12_26=((1+l7a[2])^2/(1+l7a[1]))-1
f13_26=(((1+l7a[3])^3)/(1+l7a[1]))^(1/2)-1
f14_26=((1+l7a[4])^4/(1+l7a[1]))^(1/3)-1
f15_26=((1+l7a[5])^5/(1+l7a[1]))^(1/4)-1
c(f12_26,f13_26,f14_26,f15_26)


ph1=m1[2,8]
ph1=as.numeric(ph1)
coupona=0.015*100/2
coupona=as.numeric(coupona)
fh=function(x)(coupona+(coupona+100)*(1+x/2)^(-0.5)-ph1)
rh1=uniroot(fh,lower=-1,upper=1)$root
ph2=m1[3,8]
ph2=as.numeric(ph2)
fh2=function(x)(coupona+coupona*(1+rh1/2)^(-0.5)+(coupona+100)*(1+x/2)^(-2*0.5)-ph2)
rh2=uniroot(fh2,lower=0,upper=1)$root
ph3=m1[4,8]
ph3=as.numeric(ph3)
couponb=0.0025*100/2
couponb=as.numeric(couponb)
fh3=function(x)(couponb+couponb*(1+rh1/2)^(-0.5)+couponb*(1+rh2/2)^(-2*0.5)+(couponb+100)*(1+x/2)^(-3*0.5)-ph3)
rh3=uniroot(fh3,lower=0,upper=1)$root
ph4=m1[5,8]
ph4=as.numeric(ph4)
fh4=function(x)(couponb+couponb*(1+rh1/2)^(-0.5)+couponb*(1+rh2/2)^(-2*0.5)+couponb*(1+rh3/2)^(-3*0.5)+(couponb+100)*(1+x/2)^(-4*0.5)-ph4)
rh4=uniroot(fh4,lower=0,upper=1)$root
jan27=vector()
jan27=c(rh1,rh2,rh3,rh4)
x=c(0.5,1,1.5,2)
func=splinefun(x=x,y=jan27,method="fmm",ties = mean)
l8=func(seq(0.5,5,0.5))
l8=t(l8)
l8
#forward rate Jan27
func=splinefun(x=x,y=jan27,method="fmm",ties = mean)
l8a=func(seq(0.5,5,1))
l8a=t(l8a)

f12_27=((1+l8a[2])^2/(1+l8a[1]))-1
f13_27=(((1+l8a[3])^3)/(1+l8a[1]))^(1/2)-1
f14_27=((1+l8a[4])^4/(1+l8a[1]))^(1/3)-1
f15_27=((1+l8a[5])^5/(1+l8a[1]))^(1/4)-1
c(f12_27,f13_27,f14_27,f15_27)



pi1=m1[2,9]
pi1=as.numeric(pi1)
coupona=0.015*100/2
coupona=as.numeric(coupona)
fi=function(x)(coupona+(coupona+100)*(1+x/2)^(-0.5)-pi1)
ri1=uniroot(fi,lower=-1,upper=1)$root
pi2=m1[3,9]
pi2=as.numeric(pi2)
fi2=function(x)(coupona+coupona*(1+ri1/2)^(-0.5)+(coupona+100)*(1+x/2)^(-2*0.5)-pi2)
ri2=uniroot(fi2,lower=0,upper=1)$root
pi3=m1[4,9]
pi3=as.numeric(pi3)
couponb=0.0025*100/2
couponb=as.numeric(couponb)
fi3=function(x)(couponb+couponb*(1+ri1/2)^(-0.5)+couponb*(1+ri2/2)^(-2*0.5)+(couponb+100)*(1+x/2)^(-3*0.5)-pi3)
ri3=uniroot(fi3,lower=0,upper=1)$root
pi4=m1[5,9]
pi4=as.numeric(pi4)
fi4=function(x)(couponb+couponb*(1+ri1/2)^(-0.5)+couponb*(1+ri2/2)^(-2*0.5)+couponb*(1+ri3/2)^(-3*0.5)+(couponb+100)*(1+x/2)^(-4*0.5)-pi4)
ri4=uniroot(fi4,lower=0,upper=1)$root
jan28=vector()
jan28=c(ri1,ri2,ri3,ri4)
x=c(0.5,1,1.5,2)
func=splinefun(x=x,y=jan28,method="fmm",ties = mean)
l9=func(seq(0.5,5,0.5))
l9=t(l9)
l9
#forward rate Jan28
func=splinefun(x=x,y=jan28,method="fmm",ties = mean)
l9a=func(seq(0.5,5,1))
l9a=t(l9a)

f12_28=((1+l9a[2])^2/(1+l9a[1]))-1
f13_28=(((1+l9a[3])^3)/(1+l9a[1]))^(1/2)-1
f14_28=((1+l9a[4])^4/(1+l9a[1]))^(1/3)-1
f15_28=((1+l9a[5])^5/(1+l9a[1]))^(1/4)-1
c(f12_28,f13_28,f14_28,f15_28)


pj1=m1[2,10]
pj1=as.numeric(pj1)
coupona=0.015*100/2
coupona=as.numeric(coupona)
fj=function(x)(coupona+(coupona+100)*(1+x/2)^(-0.5)-pj1)
rj1=uniroot(fj,lower=-1,upper=1)$root
pj2=m1[3,10]
pj2=as.numeric(pj2)
fj2=function(x)(coupona+coupona*(1+rj1/2)^(-0.5)+(coupona+100)*(1+x/2)^(-2*0.5)-pj2)
rj2=uniroot(fj2,lower=0,upper=1)$root
pj3=m1[4,10]
pj3=as.numeric(pj3)
couponb=0.0025*100/2
couponb=as.numeric(couponb)
fj3=function(x)(couponb+couponb*(1+rj1/2)^(-0.5)+couponb*(1+rj2/2)^(-2*0.5)+(couponb+100)*(1+x/2)^(-3*0.5)-pj3)
rj3=uniroot(fj3,lower=0,upper=1)$root
pj4=m1[5,10]
pj4=as.numeric(pj4)
fj4=function(x)(couponb+couponb*(1+rj1/2)^(-0.5)+couponb*(1+rj2/2)^(-2*0.5)+couponb*(1+rj3/2)^(-3*0.5)+(couponb+100)*(1+x/2)^(-4*0.5)-pj4)
rj4=uniroot(fj4,lower=0,upper=1)$root
jan29=vector()
jan29=c(rj1,rj2,rj3,rj4)
x=c(0.5,1,1.5,2)
func=splinefun(x=x,y=jan29,method="fmm",ties = mean)
l10=func(seq(0.5,5,0.5))
l10=t(l10)
l10
#forward rate Jan29
func=splinefun(x=x,y=jan29,method="fmm",ties = mean)
l10a=func(seq(0.5,5,1))
l10a=t(l10a)

f12_29=((1+l10a[2])^2/(1+l10a[1]))-1
f13_29=(((1+l10a[3])^3)/(1+l10a[1]))^(1/2)-1
f14_29=((1+l10a[4])^4/(1+l10a[1]))^(1/3)-1
f15_29=((1+l10a[5])^5/(1+l10a[1]))^(1/4)-1
c(f12_29,f13_29,f14_29,f15_29)


#plot spot rate
library(ggplot2)
plotdata2=read.csv("/Users/86745/Desktop/APM466/APM466 plot2.csv",header = TRUE)
plotdata2=data.frame(plotdata2)

pttm2=plotdata2$time_to_mat
pttm2=data.frame(pttm,stringsAsFactors = F) 
#class(pttm)

pytm2=plotdata2$spot_rate
pytm2=data.frame(pytm2,stringsAsFactors = F)
#class(pytm)

pdate2=as.data.frame(plotdata2$date)

#class(pdate)

ggplot(plotdata2,aes(x=plotdata2$time_to_mat,y=plotdata2$spot_rate,group=plotdata2$date, color=plotdata2$date))+
  geom_line()+xlab("Time to mat")+ylab("Spot rate")+labs(colours="date")

#plot forward rate
plotdata3=read.csv("/Users/86745/Desktop/APM466/APM466 fr.csv",header = TRUE)
plotdata3=data.frame(plotdata3)

pttm3=plotdata3$time_to_mat2
pttm3=data.frame(pttm3,stringsAsFactors = F) 
#class(pttm)

fr=plotdata3$fr
fr=data.frame(fr,stringsAsFactors = F)
#class(pytm)

pdate3=as.data.frame(plotdata3$date)

#class(pdate)

ggplot(plotdata3,aes(x=plotdata3$time_to_mat2,y=plotdata3$fr,group=plotdata3$date, color=plotdata3$date))+
  geom_line()+xlab("Time to mat")+ylab("Forward rate")+labs(colours="date")


#------------------------------------------------------------------------------------------------------



#5
a=vector()
ytm1=unlist(ytm1)
class(ytm1[1])
ytm1

for (j in 1:9){
  a[j] <- log(ytm2[j+1]/ytm2[j])
}
a

b=vector()
ytm2=unlist(ytm2)
for (j in 1:9){
  b[j] <- log(ytm3[j+1]/ytm3[j])
}
b

c=vector()
for (j in 1:9){
  c[j] <- log(ytm4[j+1]/ytm4[j])
}
d=vector()
for (j in 1:9){
  d[j] <- log(ytm5[j+1]/ytm5[j])
}
e=vector()
for (j in 1:9){
  e[j] <- log(ytm6[j+1]/ytm6[j])
}
e
f=vector()
for (j in 1:9){
  f[j] <- log(ytm9[j+1]/ytm9[j])
}
e
#convert Xi's into matrix form
covmatrix<- matrix(c(a,b,c,d,e),nrow=9,ncol=5,byrow = T)
covmatrix
covmatrixfinal <-cov(covmatrix)
covmatrixfinal
ytmeigen <- eigen(covmatrixfinal)
ytmeigen


#fr cov matrix
fr_matrix=data.frame(c(f12_18,f13_18,f14_18,f15_18),c(f12_19,f13_19,f14_19,f15_19),c(f12_20,f13_20,f14_20,f15_20),c(f12_21,f13_21,f14_21,f15_21),
                     c(f12_22,f13_22,f14_22,f15_22),c(f12_25,f13_25,f14_25,f15_25),c(f12_26,f13_26,f14_26,f15_26),
                     c(f12_27,f13_27,f14_27,f15_27),c(f12_28,f13_28,f14_28,f15_28),c(f12_29,f13_29,f14_29,f15_29))
fr_matrix
c(f12_18,f13_18,f14_18,f15_18)
c(f12_19,f13_19,f14_19,f15_19)
a2=vector()
row1=c( 0.003143245 ,0.003160247 ,0.003313878 , 0.00306270,0.003210197,0.003789962 ,0.003669354 , 0.003281103,0.002762055,  0.002909530)
for (j in 1:9){
  a2[j] <- log(row1[j+1]/row1[j])
}
a2

b2=vector()
row2=c( 0.010779742,0.009249813, 0.008699726 , 0.00897069,0.008848799 , 0.008871071, 0.009351317, 0.008627334,0.010106928 , 0.009378267)
for (j in 1:9){
  b2[j] <- log(row2[j+1]/row2[j])
}
b2

c2=vector()
row3=c(0.038789190 ,0.026528841 , 0.024164237, 0.02692554,0.025593367 , 0.024558439  ,0.028608326 , 0.025699521,0.036558332 , 0.032512257)
for (j in 1:9){
  c2[j] <- log(row3[j+1]/row3[j])
}
c2
d2=vector()
row4=c(0.099326869  ,0.060735900 , 0.054826736 ,0.06317646, 0.059182908 ,0.056658855 , 0.069246559 , 0.060988421,0.093144973 ,  0.081954689)
for (j in 1:9){
  d2[j] <- log(row4[j+1]/row4[j])
}
d2

covmatrix_fr<- matrix(c(a2,b2,c2,d2),nrow=9,ncol=4,byrow=T)
covmatrix_fr
covmatrixfinal_fr <-cov(covmatrix_fr)
covmatrixfinal_fr
freigen <- eigen(covmatrixfinal_fr)
freigen
