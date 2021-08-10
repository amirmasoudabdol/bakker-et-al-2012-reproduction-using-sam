#packages
library(meta)
library(metafor)

#functions
FisherR=function(r){.5*log((1+r)/(1-r))}
power=function(A,s,alpha,df){1 - pt (qt (1-alpha/2, df)-A/s, df) + pt (-qt(1-alpha/2, df)-A/s, df)}
rTOd=function(r){(2*r)/sqrt(1-r^2)}
zTOr=function(z){(exp(2*z)-1)/(exp(2*z)+1)}
p0=function(x){ifelse(x<.001,"p<.001",paste("p=",round(x,3),sep=""))}

#read data from studies and create list
S1=read.csv2("Alfieri et al 2011.csv",header=T)
S2=read.csv2("Benish et al 2011.csv",header=T)
S3=read.csv2("Berry et al 2011.csv",header=T)
S4=read.csv2("Card et al 2011.csv",header=T)
S5=read.csv2("Farber and Doolin 2011.csv",header=T)
S6=read.csv2("Green and Rosenfeld 2011.csv",header=T)
S7=read.csv2("Hallion and Ruscio 2011.csv",header=T)
S8=read.csv2("Lucassen et al 2011.csv",header=T)
S9=read.csv2("Mol and Bus 2011.csv",header=T)
S10=read.csv2("Woodin 2011.csv",header=T)
S11=read.csv2("Woodley 2011.csv",header=T)
S12=read.csv2("Greenwald et al 2009.csv",header=T)
S13=read.csv2("McCall and Carriger 1993.csv",header=T)

allData=list(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13)

#save allData
#save(allData,file="allData.dat")
#load("allData.dat")

#vector with information on datatype where c=correlations and m = mean difference in d
met=c("m","m","c","c","c","m","m","c","c","m","c","c","c")	

#create Table
Table1=data.frame(matrix(NA,13,17))
colnames(Table1)=c("Reference","Subgroup","ES","ESd","SE","k","N","medN","Q","Qp","I",
	"Exp","Obs","Chi2","Chi2p","Regtest","RegP")


#create list with all meta-analyses
metas=list(NA)

for(i in 1:13)
{
	if(met[i]=="c")
	{
		d=allData[[i]]
		pVals=2*(1-pnorm(abs(d$Fisher.s.Z/d$SE)))
		obsSig=length(which(pVals<.05))
		m2rma=rma.uni(ri=d$r,ni=d$n,measure="ZCOR")
		m2meta=metacor(d$r,d$n,sm="ZCOR")
		metas[[i]]=m2meta		
		ES=m2meta$TE.fixed
		ESd=rTOd(zTOr(ES))
		SE=m2meta$seTE.fixed
		k=m2meta$k
		N=sum(d$n)
		medN=median(d$n)
		Q=m2meta$Q
		Qp=1-pchisq(m2meta$Q,m2meta$k-1)
		I=ifelse(m2meta$Q-(m2meta$k-1)<0,0,(m2meta$Q-(m2meta$k-1))/m2meta$Q)*100
		Exp=sum(power(ES,m2meta$seTE,.05,Inf))
		if(i==4)
		{
			Obs=length(which(pVals<.05&d$d>0))
		}else{
			Obs=length(which(pVals<.05))
		}
		Chi2=(Obs-Exp)^2/Exp + (Obs-Exp)^2/(k-Exp)
		Chi2p=1-pchisq(Chi2,1)
		Regtest=regtest(m2rma)$zval
		RegP=regtest(m2rma)$pval

		Table1[i,3:17]=c(ES,ESd,SE,k,N,medN,Q,Qp,I,Exp,Obs,Chi2,Chi2p,Regtest,RegP)
	}
	
	if(met[i]=="m")
	{
		d=allData[[i]]
		pVals=2*(1-pnorm(abs(d$d/d$SEd)))
		obsSig=length(which(pVals<.05))
		m2meta=metagen(d$d,d$SEd,sm="SMD")
		m2rma=rma.uni(yi=d$d,sei=d$SEd,method="FE")
		metas[[i]]=m2meta	
		ES=m2meta$TE.fixed
		ESd=ES
		SE=m2meta$seTE.fixed
		k=m2meta$k
		if(i==7|i==10)
		{
			N=sum(d$n)
			medN=median(d$n)	
		}else{
			N=sum(c(d$n1,d$n2))
			medN=median(d$n1+d$n2)
		}
		Q=m2meta$Q
		Qp=1-pchisq(m2meta$Q,m2meta$k-1)
		I=ifelse(m2meta$Q-(m2meta$k-1)<0,0,(m2meta$Q-(m2meta$k-1))/m2meta$Q)*100
		Exp=sum(power(ES,m2meta$seTE,.05,Inf))
		Obs=length(which(pVals<.05))
		Chi2=(Obs-Exp)^2/Exp + (Obs-Exp)^2/(k-Exp)
		Chi2p=1-pchisq(Chi2,1)
		Regtest=regtest(m2rma)$zval
		RegP=regtest(m2rma)$pval

		Table1[i,3:17]=c(ES,ESd,SE,k,N,medN,Q,Qp,I,Exp,Obs,Chi2,Chi2p,Regtest,RegP)
	}
}
Table1$Reference=c("Alfieri et al. 2011",
"Benish et al. 2011",
"Berry et al. 2011",
"Card et al. 2011",
"Farber & Doolin 2011",
"Green & Rosenfeld 2011",
"Hallion & Ruscio 2011",
"Lucassen et al. 2011",
"Mol & Bus 2011",
"Woodin 2011",
"Woodley 2011",
"Greenwald et al. 2009",
"McCall & Carriger 1993")
Table1$Subgroup=c("Enhanced Discovery: Children",
"All",
"Self-other",
"Externalizing",
"All",
"Average SIRS simulators versus nonclinical",
"Posttest",
"All",
"Grades 1-12 Basics",
"Satis.-Hosti.",
"All",
"Race IAT",
"Habituation")

#round values of Table 1
TableR=Table1
TableR[,c(3,4,9,12,14,16)]=round(Table1[,c(3,4,9,12,14,16)],2)
TableR[,c(5,10,15,17)]=round(Table1[,c(5,10,15,17)],3)
TableR[,8]=round(Table1[,8],0)
TableR[,11]=round(Table1[,11],1)

write.csv2(TableR,"TableR.csv")


#make Figure 3

pdf("Studies Annette final.pdf",10.5,14)

layout(matrix(1:12,4,3,T))


funnel(metas[[1]],yaxis="invse",contour=.95,col.contour="light grey",xlim=c(-2,2),ylim=c(1.5,12),main=TableR$Reference[1],xlab="mean difference")
	legend(-2.25,12,c(paste("Est ES=",TableR$ES[1],sep=""),paste("Q(",TableR$k[1]-1,")=",TableR$Q[1],", ",p0(TableR$Qp[1]),sep=""),
	paste("I^2=",TableR$I[1],sep=""),paste("I-Chi(1)=",TableR$Chi2[1],", ",p0(TableR$Chi2p[1]),sep=""),paste("Bias Z=",TableR$Regtest[1],", ",p0(TableR$RegP[1]),sep="")),cex=.9,bty="n")

funnel(metas[[2]],yaxis="invse",contour=.95,col.contour="light grey",xlim=c(-2,2),ylim=c(1.5,5.5),main=TableR$Reference[2],xlab="mean difference")
	legend(-2.25,5.5,c(paste("Est ES=",TableR$ES[2],sep=""),paste("Q(",TableR$k[2]-1,")=",TableR$Q[2],", ",p0(TableR$Qp[2]),sep=""),
	paste("I^2=",TableR$I[2],sep=""),paste("I-Chi(1)=",TableR$Chi2[2],", ",p0(TableR$Chi2p[2]),sep=""),paste("Bias Z=",TableR$Regtest[2],", ",p0(TableR$RegP[2]),sep="")),cex=.9,bty="n")

funnel(metas[[3]],yaxis="invse",contour=.95,col.contour="light grey",xlim=c(-1,1),ylim=c(5,25),main=TableR$Reference[3],xlab="Fisher transformed correlations")
	legend(-1.125,25,c(paste("Est ES=",TableR$ES[3],sep=""),paste("Q(",TableR$k[3]-1,")=",TableR$Q[3],", ",p0(TableR$Qp[3]),sep=""),
	paste("I^2=",TableR$I[3],sep=""),paste("I-Chi(1)=",TableR$Chi2[3],", ",p0(TableR$Chi2p[3]),sep=""),paste("Bias Z=",TableR$Regtest[3],", ",p0(TableR$RegP[3]),sep="")),cex=.9,bty="n")

funnel(metas[[4]],yaxis="invse",contour=.95,col.contour="light grey",xlim=c(-1,1),ylim=c(5,40),main=TableR$Reference[4],xlab="Fisher transformed correlations")
	legend(-1.125,40,c(paste("Est ES=",TableR$ES[4],sep=""),paste("Q(",TableR$k[4]-1,")=",TableR$Q[4],", ",p0(TableR$Qp[4]),sep=""),
	paste("I^2=",TableR$I[4],sep=""),paste("I-Chi(1)=",TableR$Chi2[4],", ",p0(TableR$Chi2p[4]),sep=""),paste("Bias Z=",TableR$Regtest[4],", ",p0(TableR$RegP[4]),sep="")),cex=.9,bty="n")

funnel(metas[[5]],yaxis="invse",contour=.95,col.contour="light grey",xlim=c(-1,1),ylim=c(2,14),main=TableR$Reference[5],xlab="Fisher transformed correlations")
	legend(-1.125,14,c(paste("Est ES=",TableR$ES[5],sep=""),paste("Q(",TableR$k[5]-1,")=",TableR$Q[5],", ",p0(TableR$Qp[5]),sep=""),
	paste("I^2=",TableR$I[5],sep=""),paste("I-Chi(1)=",TableR$Chi2[5],", ",p0(TableR$Chi2p[5]),sep=""),paste("Bias Z=",TableR$Regtest[5],", ",p0(TableR$RegP[5]),sep="")),cex=.9,bty="n")

funnel(metas[[6]],yaxis="invse",contour=.95,col.contour="light grey",xlim=c(-2,3),ylim=c(2.2,4.2),main=TableR$Reference[6],xlab="mean difference")
	legend(-2.3,4.2,c(paste("Est ES=",TableR$ES[6],sep=""),paste("Q(",TableR$k[6]-1,")=",TableR$Q[6],", ",p0(TableR$Qp[6]),sep=""),
	paste("I^2=",TableR$I[6],sep=""),paste("I-Chi(1)=",TableR$Chi2[6],", ",p0(TableR$Chi2p[6]),sep=""),paste("Bias Z=",TableR$Regtest[6],", ",p0(TableR$RegP[6]),sep="")),cex=.9,bty="n")

funnel(metas[[7]],yaxis="invse",contour=.95,col.contour="light grey",xlim=c(-2,2),ylim=c(2,6.1),main=TableR$Reference[7],xlab="mean difference")
	legend(-2.25,6.1,c(paste("Est ES=",TableR$ES[7],sep=""),paste("Q(",TableR$k[7]-1,")=",TableR$Q[7],", ",p0(TableR$Qp[7]),sep=""),
	paste("I^2=",TableR$I[7],sep=""),paste("I-Chi(1)=",TableR$Chi2[7],", ",p0(TableR$Chi2p[7]),sep=""),paste("Bias Z=",TableR$Regtest[7],", ",p0(TableR$RegP[7]),sep="")),cex=.9,bty="n")

funnel(metas[[8]],yaxis="invse",contour=.95,col.contour="light grey",xlim=c(-1,1),ylim=c(5,15),main=TableR$Reference[8],xlab="Fisher transformed correlations")
	legend(-1.125,15,c(paste("Est ES=",TableR$ES[8],sep=""),paste("Q(",TableR$k[8]-1,")=",TableR$Q[8],", ",p0(TableR$Qp[8]),sep=""),
	paste("I^2=",TableR$I[8],sep=""),paste("I-Chi(1)=",TableR$Chi2[8],", ",p0(TableR$Chi2p[8]),sep=""),paste("Bias Z=",TableR$Regtest[8],", ",p0(TableR$RegP[8]),sep="")),cex=.9,bty="n")

funnel(metas[[9]],yaxis="invse",contour=.95,col.contour="light grey",xlim=c(-1,1),ylim=c(3.8,13),main=TableR$Reference[9],xlab="Fisher transformed correlations")
	legend(-1.125,13,c(paste("Est ES=",TableR$ES[9],sep=""),paste("Q(",TableR$k[9]-1,")=",TableR$Q[9],", ",p0(TableR$Qp[9]),sep=""),
	paste("I^2=",TableR$I[9],sep=""),paste("I-Chi(1)=",TableR$Chi2[9],", ",p0(TableR$Chi2p[9]),sep=""),paste("Bias Z=",TableR$Regtest[9],", ",p0(TableR$RegP[9]),sep="")),cex=.9,bty="n")

funnel(metas[[10]],yaxis="invse",contour=.95,col.contour="light grey",xlim=c(-2,2),ylim=c(1,8),main=TableR$Reference[10],xlab="mean difference")
	legend(0,8.1,c(paste("Est ES=",TableR$ES[10],sep=""),paste("Q(",TableR$k[10]-1,")=",TableR$Q[10],", ",p0(TableR$Qp[10]),sep=""),
	paste("I^2=",TableR$I[10],sep=""),paste("I-Chi(1)=",TableR$Chi2[10],", ",p0(TableR$Chi2p[10]),sep=""),paste("Bias Z=",TableR$Regtest[10],", ",p0(TableR$RegP[10]),sep="")),cex=.9,bty="n")

funnel(metas[[11]],yaxis="invse",contour=.95,col.contour="light grey",xlim=c(-1,1),ylim=c(9.5,15.5),main=TableR$Reference[11],xlab="Fisher transformed correlations")
	legend(-1.125,15.5,c(paste("Est ES=",TableR$ES[11],sep=""),paste("Q(",TableR$k[11]-1,")=",TableR$Q[11],", ",p0(TableR$Qp[11]),sep=""),
	paste("I^2=",TableR$I[11],sep=""),paste("I-Chi(1)=",TableR$Chi2[11],", ",p0(TableR$Chi2p[11]),sep=""),paste("Bias Z=",TableR$Regtest[11],", ",p0(TableR$RegP[11]),sep="")),cex=.9,bty="n")

funnel(metas[[12]],yaxis="invse",contour=.95,col.contour="light grey",xlim=c(-1,1),ylim=c(3,14.5),main=TableR$Reference[12],xlab="Fisher transformed correlations")
	legend(-1.125,14.5,c(paste("Est ES=",TableR$ES[12],sep=""),paste("Q(",TableR$k[12]-1,")=",TableR$Q[12],", ",p0(TableR$Qp[12]),sep=""),
	paste("I^2=",TableR$I[12],sep=""),paste("I-Chi(1)=",TableR$Chi2[12],", ",p0(TableR$Chi2p[12]),sep=""),paste("Bias Z=",TableR$Regtest[12],", ",p0(TableR$RegP[12]),sep="")),cex=.9,bty="n")

dev.off()

#Create Figure 1
pdf("McCall.pdf")
funnel(metas[[13]],yaxis="invse",contour=.95,col.contour="light grey",xlim=c(-1,1),main="McCall and Carriger 1993",xlab="Fisher transformed correlations")
dev.off()





