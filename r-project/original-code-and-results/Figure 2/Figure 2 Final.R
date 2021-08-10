#This R-code is used to generate Figure 2. For each point in the es vector, the 4
#strategies as descibed in the paper are applied 10000 times. The estimated effect
#sizes, calculated p values, the number of studies needed, and which QRPs are applied
#are collected. 

#required packages
library(MASS)

#variables

nsim=10000			#number of simulations
es=seq(0,1,by=.05)	#vector with effectsizes	
nS=c(5,10,20)		#cell size of small study, total sample size will be 2 times nS: 5, 10 and 20
mf=5				#multiplication factor
nL=mf*nS			#cell size of large study
alpha=.05			#used alpha level		
cbdv=.5			#correlation between dependent variables
nb=10				#number of subjects added (per cell)

#simulation

#create arrays with p values and effect sizes
psAr=esAr=selAr=array(NA,c(length(nS),length(es),nsim,4))
#create array for the category of QRPs and number of studies. ORPs of large study = [,,,1], QRPs of multiple small studies = [,,,2].
#Coded: 1=No QRPs, 2=Second dependent variable, 3=Adding subjects, 4=Removing outliers, 5=Used all QRPs but did not find significant result.
#In the same array the number of studies performed without QRPs = [,,,3], and the number of studies with QRPs = [,,,4]
catQRPAr=array(NA,c(length(nS),length(es),nsim,4))

for(i in 1:length(nS))
{
	for(j in 1:length(es))
	{
		for(k in 1:nsim)
		{
			#one large study
			g1L=mvrnorm(nL[i],rep(es[j],2),matrix(c(1,cbdv,cbdv,1),2,2))
			g2L=mvrnorm(nL[i],rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2))

			ttL=t.test(g1L[,1],g2L[,1],var.equal=T)
			pL=ttL$p.value
			esL=(mean(g1L[,1])-mean(g2L[,1]))/sqrt(.5*(var(g1L[,1])+var(g2L[,1])))
			psAr[i,j,k,1]=pL
			esAr[i,j,k,1]=esL

			if(pL>alpha|esL<0)
			{
				#test second dependent variable
				ttL_2=t.test(g1L[,2],g2L[,2],var.equal=T)
				pL_2=ttL_2$p.value
				esL_2=(mean(g1L[,2])-mean(g2L[,2]))/sqrt(.5*(var(g1L[,2])+var(g2L[,2])))

				if(pL_2>alpha|esL_2<0)
				{
					#add nb extra subjects per cell for both dependent variables
					g1L_set2=mvrnorm(nb,rep(es[j],2),matrix(c(1,cbdv,cbdv,1),2,2))
					g2L_set2=mvrnorm(nb,rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2))
					g1L_3=rbind(g1L,g1L_set2)
					g2L_3=rbind(g2L,g2L_set2)

					ttL_3_v1=t.test(g1L_3[,1],g2L_3[,1],var.equal=T)
					pL_3_v1=ttL_3_v1$p.value
					esL_3_v1=(mean(g1L_3[,1])-mean(g2L_3[,1]))/sqrt(.5*(var(g1L_3[,1])+var(g2L_3[,1])))

					ttL_3_v2=t.test(g1L_3[,2],g2L_3[,2],var.equal=T)
					pL_3_v2=ttL_3_v2$p.value
					esL_3_v2=(mean(g1L_3[,2])-mean(g2L_3[,2]))/sqrt(.5*(var(g1L_3[,2])+var(g2L_3[,2])))
					
					if((pL_3_v1>alpha|esL_3_v1<0)&(pL_3_v2>alpha|esL_3_v2<0))
					{
						#remove outliers
						g1L_4=g1L_3
						g2L_4=g2L_3
						g1L_4[which(abs((g1L_4[,1]-mean(g1L_4[,1]))/sd(g1L_4[,1]))>2)]=NA
						g2L_4[which(abs((g2L_4[,1]-mean(g2L_4[,1]))/sd(g2L_4[,1]))>2)]=NA
						g1L_4[which(abs((g1L_4[,2]-mean(g1L_4[,2]))/sd(g1L_4[,2]))>2)]=NA
						g2L_4[which(abs((g2L_4[,2]-mean(g2L_4[,2]))/sd(g2L_4[,2]))>2)]=NA

						ttL_4_v1=t.test(g1L_4[,1],g2L_4[,1],var.equal=T,na.rm=T)
						pL_4_v1=ttL_4_v1$p.value
						esL_4_v1=(mean(g1L_4[,1],na.rm=T)-mean(g2L_4[,1],na.rm=T))/sqrt(.5*(var(g1L_4[,1],na.rm=T)+var(g2L_4[,1],na.rm=T)))
						
						ttL_4_v2=t.test(g1L_4[,2],g2L_4[,2],var.equal=T,na.rm=T)
						pL_4_v2=ttL_4_v2$p.value
						esL_4_v2=(mean(g1L_4[,2],na.rm=T)-mean(g2L_4[,2],na.rm=T))/sqrt(.5*(var(g1L_4[,2],na.rm=T)+var(g2L_4[,2],na.rm=T)))

						if((pL_4_v1>alpha|esL_4_v1<0)&(pL_4_v2>alpha|esL_4_v2<0))
						{
							#if non is significant select the best result
							all_psL=c(pL,pL_2,pL_3_v1,pL_3_v2,pL_4_v1,pL_4_v2)
							all_esL=c(esL,esL_2,esL_3_v1,esL_3_v2,esL_4_v1,esL_4_v2)
							if(length(which(all_esL>0))>0)
							{
								pL_5=min(all_psL[all_esL>0])
							}else{
								pL_5=max(all_psL[all_esL<0])
							}
							esL_5=all_esL[which(all_psL==pL_5)][1]
							
							psAr[i,j,k,2]=pL_5
							esAr[i,j,k,2]=esL_5
							catQRPAr[i,j,k,1]=5
						}else{
							pL_4=min(c(pL_4_v1,pL_4_v2)[which(c(esL_4_v1,esL_4_v2)>0)])
							esL_4=c(esL_4_v1,esL_4_v2)[which(c(pL_4_v1,pL_4_v2)==pL_4)]

							psAr[i,j,k,2]=pL_4
							esAr[i,j,k,2]=esL_4
							catQRPAr[i,j,k,1]=4
						}
					}else{
						pL_3=min(c(pL_3_v1,pL_3_v2)[which(c(esL_3_v1,esL_3_v2)>0)])
						esL_3=c(esL_3_v1,esL_3_v2)[which(c(pL_3_v1,pL_3_v2)==pL_3)]

						psAr[i,j,k,2]=pL_3
						esAr[i,j,k,2]=esL_3
						catQRPAr[i,j,k,1]=3
					}
				}else{
					psAr[i,j,k,2]=pL_2
					esAr[i,j,k,2]=esL_2
					catQRPAr[i,j,k,1]=2
				}
			}else{
				psAr[i,j,k,2]=pL
				esAr[i,j,k,2]=esL
				catQRPAr[i,j,k,1]=1
			}
		

			#multiple small studies

			#temperory vectors to collect the results of the small studies
			pS=esSV=catS=pSQRP=esSQRP=catSQRP=rep(NA,mf)
			for(l in 1:mf)
			{
				g1=mvrnorm(nS[i],rep(es[j],2),matrix(c(1,cbdv,cbdv,1),2,2))
				g2=mvrnorm(nS[i],rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2))

				tt=t.test(g1[,1],g2[,1],var.equal=T)
				p=tt$p.value
				esS=(mean(g1[,1])-mean(g2[,1]))/sqrt(.5*(var(g1[,1])+var(g2[,1])))

				pS[l]=p
				esSV[l]=esS	

				if(p>alpha|esS<0)
				{
					#test second dependent variable
					tt_2=t.test(g1[,2],g2[,2],var.equal=T)
					p_2=ttL_2$p.value
					es_2=(mean(g1[,2])-mean(g2[,2]))/sqrt(.5*(var(g1[,2])+var(g2[,2])))

					if(p_2>alpha|es_2<0)
					{
						#add nb extra subjects per cell for both dependent variables 
						g1_set2=mvrnorm(nb,rep(es[j],2),matrix(c(1,cbdv,cbdv,1),2,2))
						g2_set2=mvrnorm(nb,rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2))
						g1_3=rbind(g1,g1_set2)
						g2_3=rbind(g2,g2_set2)
	
						tt_3_v1=t.test(g1_3[,1],g2_3[,1],var.equal=T)
						p_3_v1=tt_3_v1$p.value
						es_3_v1=(mean(g1_3[,1])-mean(g2_3[,1]))/sqrt(.5*(var(g1_3[,1])+var(g2_3[,1])))

						tt_3_v2=t.test(g1_3[,2],g2_3[,2],var.equal=T)
						p_3_v2=tt_3_v2$p.value
						es_3_v2=(mean(g1_3[,2])-mean(g2_3[,2]))/sqrt(.5*(var(g1_3[,2])+var(g2_3[,2])))

						if((p_3_v1>alpha|es_3_v1<0)&(p_3_v2>alpha|es_3_v2<0))
						{
							#remove outliers
							g1_4=g1_3
							g2_4=g2_3
							g1_4[which(abs((g1_4[,1]-mean(g1_4[,1]))/sd(g1_4[,1]))>2)]=NA
							g2_4[which(abs((g2_4[,1]-mean(g2_4[,1]))/sd(g2_4[,1]))>2)]=NA
							g1_4[which(abs((g1_4[,2]-mean(g1_4[,2]))/sd(g1_4[,2]))>2)]=NA
							g2_4[which(abs((g2_4[,2]-mean(g2_4[,2]))/sd(g2_4[,2]))>2)]=NA

							tt_4_v1=t.test(g1_4[,1],g2_4[,1],var.equal=T,na.rm=T)
							p_4_v1=tt_4_v1$p.value
							es_4_v1=(mean(g1_4[,1],na.rm=T)-mean(g2_4[,1],na.rm=T))/sqrt(.5*(var(g1_4[,1],na.rm=T)+var(g2_4[,1],na.rm=T)))
						
							tt_4_v2=t.test(g1_4[,2],g2_4[,2],var.equal=T,na.rm=T)
							p_4_v2=tt_4_v2$p.value
							es_4_v2=(mean(g1_4[,2],na.rm=T)-mean(g2_4[,2],na.rm=T))/sqrt(.5*(var(g1_4[,2],na.rm=T)+var(g2_4[,2],na.rm=T)))

							if((p_4_v1>alpha|es_4_v1<0)&(p_4_v2>alpha|es_4_v2<0))
							{
								#if non is significant select the best result
								all_ps=c(p,p_2,p_3_v1,p_3_v2,p_4_v1,p_4_v2)
								all_es=c(esS,es_2,es_3_v1,es_3_v2,es_4_v1,es_4_v2)
								if(length(which(all_es>0))>0)
								{
									p_5=min(all_ps[all_es>0])
								}else{
									p_5=max(all_ps[all_es<0])
								}
								es_5=all_es[which(all_ps==p_5)][1]
							
								pSQRP[l]=p_5
								esSQRP[l]=es_5
								catSQRP[l]=5
							}else{
								p_4=min(c(p_4_v1,p_4_v2)[which(c(es_4_v1,es_4_v2)>0)])
								es_4=c(es_4_v1,es_4_v2)[which(c(p_4_v1,p_4_v2)==p_4)]

								pSQRP[l]=p_4
								esSQRP[l]=es_4
								catSQRP[l]=4
							}
						}else{
							p_3=min(c(p_3_v1,p_3_v2)[which(c(es_3_v1,es_3_v2)>0)])
							es_3=c(es_3_v1,es_3_v2)[which(c(p_3_v1,p_3_v2)==p_3)]

							pSQRP[l]=p_3
							esSQRP[l]=es_3
							catSQRP[l]=3
						}
					}else{
						pSQRP[l]=p_2
						esSQRP[l]=es_2
						catSQRP[l]=2
					}
				}else{
					pSQRP[l]=p
					esSQRP[l]=esS
					catSQRP[l]=1
				}			

			}

			#select first significant study without QRPs with a positive ES if non is significant, select best study.
			if(length(which(esSV>0))>0)
			{
				if(length(which(pS<alpha&esSV>0))>0)
				{
					selP=pS[which(pS<alpha&esSV>0)][1]
					catQRPAr[i,j,k,3]=which(pS<alpha&esSV>0)[1]
				}else{
					selP=min(pS[which(esSV>0)])
					catQRPAr[i,j,k,3]=5
				}
			}else{
				selP=max(pS[which(esSV<0)])
				catQRPAr[i,j,k,3]=5
			}
			psAr[i,j,k,3]=selP
			esAr[i,j,k,3]=esSV[which(pS==selP)][1]

			#select first significant study with QRPs with a positive ES if non is significant, select best study.
			if(length(which(esSQRP>0))>0)
			{
				if(length(which(pSQRP<alpha&esSQRP>0))>0)
				{
					selPQRP=pSQRP[which(pSQRP<alpha&esSQRP>0)][1]
					catQRPAr[i,j,k,4]=which(pSQRP<alpha&esSQRP>0)[1]
				}else{
					selPQRP=min(pSQRP[which(esSQRP>0)])
					catQRPAr[i,j,k,4]=5
				}
			}else{
				selPQRP=max(pSQRP[which(esSQRP<0)])
				catQRPAr[i,j,k,4]=5
			}
			psAr[i,j,k,4]=selPQRP
			esAr[i,j,k,4]=esSQRP[which(pSQRP==selPQRP)][1]
			catQRPAr[i,j,k,2]=catSQRP[which(pSQRP==selPQRP)][1]
		}
		print(j)
		flush.console()	
	}
	print(i)
	flush.console()
}


#calculate proportions of significant p values
pProp=matrix(NA,length(es),12)
for(i in 1:length(es))
{
	for(j in 1:length(nS))
	{
		for(k in 1:4)
		{
			pProp[i,(j-1)*4+k]=length(which(psAr[j,i,,k][which(esAr[j,i,,k]>0)]<alpha))/length(psAr[j,i,,k])
		}
	}
}


#save and load (earlier) created data
#save(esAr,file="esAr.dat")
#write.csv2(pProp,"pProp.csv")
#load("esAr.dat")
#pProp=read.csv2("pProp.csv")[,-1]


#create pdf of figure 2
#pdf("proportions and bias final.pdf",7,10)
postscript("figure 2.ps",paper="special",width=7,height=10)
lab=c("1 large study","1 large study with QRPs","5 small studies","5 small studies with QRPs")
layout(matrix(1:6,3,2))

plot(es,pProp[,1],type="l",col=1,lty=1,ylim=c(0,1),main="Chance of getting at least 1 significant result \n Small N = 10",xlab="ES",ylab="proportion")
	lines(es,pProp[,2],type="l",col=2,lty=1)
	lines(es,pProp[,3],type="l",col=1,lty=2)
	lines(es,pProp[,4],type="l",col=2,lty=2)
	legend(.5,.3,lab,col=1:2,lty=c(1,1,2,2),bty="n",cex=.8)

plot(es,pProp[,5],type="l",col=1,lty=1,ylim=c(0,1),main="Chance of getting at least 1 significant result \n Small N = 20",xlab="ES",ylab="proportion")
	lines(es,pProp[,6],type="l",col=2,lty=1)
	lines(es,pProp[,7],type="l",col=1,lty=2)
	lines(es,pProp[,8],type="l",col=2,lty=2)
	legend(.5,.3,lab,col=1:2,lty=c(1,1,2,2),bty="n",cex=.8)

plot(es,pProp[,9],type="l",col=1,lty=1,ylim=c(0,1),main="Chance of getting at least 1 significant result \n Small N = 40",xlab="ES",ylab="proportion")
	lines(es,pProp[,10],type="l",col=2,lty=1)
	lines(es,pProp[,11],type="l",col=1,lty=2)
	lines(es,pProp[,12],type="l",col=2,lty=2)
	legend(.5,.3,lab,col=1:2,lty=c(1,1,2,2),bty="n",cex=.8)

plot(es,apply(esAr[1,,,1],1,mean)-es,type="l",col=1,lty=1,ylim=c(0,1.2),main="ES bias; Small N = 10",xlab="ES",ylab="bias")
	lines(es,apply(esAr[1,,,2],1,mean)-es,type="l",col=2,lty=1)
	lines(es,apply(esAr[1,,,3],1,mean)-es,type="l",col=1,lty=2)
	lines(es,apply(esAr[1,,,4],1,mean)-es,type="l",col=2,lty=2)
	legend(0,1.2,lab,col=1:2,lty=c(1,1,2,2),bty="n",cex=.8)

plot(es,apply(esAr[2,,,1],1,mean)-es,type="l",col=1,lty=1,ylim=c(0,1.2),main="ES bias; Small N = 20",xlab="ES",ylab="bias")
	lines(es,apply(esAr[2,,,2],1,mean)-es,type="l",col=2,lty=1)
	lines(es,apply(esAr[2,,,3],1,mean)-es,type="l",col=1,lty=2)
	lines(es,apply(esAr[2,,,4],1,mean)-es,type="l",col=2,lty=2)
	legend(0,1.2,lab,col=1:2,lty=c(1,1,2,2),bty="n",cex=.8)

plot(es,apply(esAr[3,,,1],1,mean)-es,type="l",col=1,lty=1,ylim=c(0,1.2),main="ES bias; Small N = 40",xlab="ES",ylab="bias")
	lines(es,apply(esAr[3,,,2],1,mean)-es,type="l",col=2,lty=1)
	lines(es,apply(esAr[3,,,3],1,mean)-es,type="l",col=1,lty=2)
	lines(es,apply(esAr[3,,,4],1,mean)-es,type="l",col=2,lty=2)
	legend(0,1.2,lab,col=1:2,lty=c(1,1,2,2),bty="n",cex=.8)

dev.off()


