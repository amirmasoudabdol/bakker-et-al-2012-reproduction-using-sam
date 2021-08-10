#This R-code is used to generate Figure 4 and the expected values of the meta-
#analyses performed for this figure. If you only want the Figure, set nSim on 1 
#(more efficient).

#required packages
library(MASS)
library(meta)
library(metafor)
library(gplots)

#variables
es=c(0,.2,.5,.8)			#selected values of underlying ES
nSamp=100				#number of studies included in each meta analysis
cbdv=.5				#correlation between the two dependent variables
nStudies=5				#maximum number of small studies performed	
nSim=10				#number of simulations to create expected values of these meta analyses
nb=10					#number of subjects added (per cell)


#functions
power=function(A,s,alpha,df){1 - pt (qt (1-alpha/2, df)-A/s, df) + pt (-qt(1-alpha/2, df)-A/s, df)}
p0=function(x){ifelse(x<.001,"p<.001",paste("p=",round(x,3),sep=""))}

#simulation

#create arrays for the expected values of the metaanalyses
RExVal=PRExVal=ESExVal=QExVal=PQExVal=IExVal=PIExVal=array(NA,c(length(es),4,nSim))

for(l in 1:nSim)
{
	#create arrays for the study characteristics
	resL=resLQRP=resS=resSQRP=array(NA,c(length(es),nSamp,10)) ##

	for(i in 1:length(es))
	{
		for(j in 1:nSamp)
		{
			#draw total sample size of small study
			tS=rnbinom(1,mu=30,size=2)+10
			tC=round(tS/2) 				#cell size of small study
			tL=nStudies*tC				#cell size of large study
			resk=reskQRP=matrix(NA,5,10)		#create matrix for results 5 small studies


			#perform 'nStudies' studies
			for(k in 1:nStudies)
			{
				g1=mvrnorm(tC,rep(es[i],2),matrix(c(1,cbdv,cbdv,1),2,2))
				g2=mvrnorm(tC,rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2))
				m1=mean(g1[,1])
				m2=mean(g2[,1])
				sd1=sd(g1[,1])
				sd2=sd(g2[,1])
				obsES=(m1-m2)/sqrt(.5*(var(g1[,1])+var(g2[,1])))
				vi=(tC+tC)/(tC*tC)+obsES^2/(tC+tC)
				sei=sqrt(vi)
				pv=t.test(g1[,1],g2[,1],var.equal=T)$p.value

				resk[k,]=c(tC,m1,m2,sd1,sd2,obsES,vi,sei,pv,tC)

				if(pv>.05|obsES<0)
				{
					#test second dependent variable
					m1_2=mean(g1[,2])
					m2_2=mean(g2[,2])
					sd1_2=sd(g1[,2])
					sd2_2=sd(g2[,2])
					obsES_2=(m1_2-m2_2)/sqrt(.5*(var(g1[,2])+var(g2[,2])))
					vi_2=(tC+tC)/(tC*tC)+obsES_2^2/(tC+tC)
					sei_2=sqrt(vi_2)
					pv_2=t.test(g1[,2],g2[,2],var.equal=T)$p.value

					if(pv_2>.05|obsES_2<0)
					{
						#add nb extra subjects per cell for both dependent variables
						g1_set2=mvrnorm(nb,rep(es[1],2),matrix(c(1,cbdv,cbdv,1),2,2))
						g2_set2=mvrnorm(nb,rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2))
						g1_3=rbind(g1,g1_set2)
						g2_3=rbind(g2,g2_set2)
						pv_3_v1=t.test(g1_3[,1],g2_3[,1],var.equal=T)$p.value
						pv_3_v2=t.test(g1_3[,2],g2_3[,2],var.equal=T)$p.value
						obsES_3_v1=(mean(g1_3[,1])-mean(g2_3[,1]))/sqrt(.5*(var(g1_3[,1])+var(g2_3[,1])))
						obsES_3_v2=(mean(g1_3[,2])-mean(g2_3[,2]))/sqrt(.5*(var(g1_3[,2])+var(g2_3[,2])))

						m1_3_v1=mean(g1_3[,1])
						m2_3_v1=mean(g2_3[,1])
						sd1_3_v1=sd(g1_3[,1])
						sd2_3_v1=sd(g2_3[,1])
						vi_3_v1=(tC+tC+2*nb)/((tC+nb)*(tC+nb))+obsES_3_v1^2/(tC+tC+2*nb)
						sei_3_v1=sqrt(vi_3_v1)

						m1_3_v2=mean(g1_3[,2])
						m2_3_v2=mean(g2_3[,2])
						sd1_3_v2=sd(g1_3[,2])
						sd2_3_v2=sd(g2_3[,2])
						vi_3_v2=(tC+tC+2*nb)/((tC+nb)*(tC+nb))+obsES_3_v2^2/(tC+tC+2*nb)
						sei_3_v2=sqrt(vi_3_v2)			

						if((pv_3_v1>.05|obsES_3_v1<0)&(pv_3_v2>.05|obsES_3_v2))
						{
							#remove outliers
							g1_4=g1_3
							g2_4=g2_3
							g1_4[which(abs((g1_4[,1]-mean(g1_4[,1]))/sd(g1_4[,1]))>2)]=NA
							g2_4[which(abs((g2_4[,1]-mean(g2_4[,1]))/sd(g2_4[,1]))>2)]=NA
							g1_4[which(abs((g1_4[,2]-mean(g1_4[,2]))/sd(g1_4[,2]))>2)]=NA
							g2_4[which(abs((g2_4[,2]-mean(g2_4[,2]))/sd(g2_4[,2]))>2)]=NA
				
							pv_4_v1=t.test(g1_4[,1],g2_4[,1],var.equal=T,na.rm=T)$p.value
							pv_4_v2=t.test(g1_4[,2],g2_4[,2],var.equal=T,na.rm=T)$p.value
							obsES_4_v1=(mean(g1_4[,1],na.rm=T)-mean(g2_4[,1],na.rm=T))/sqrt(.5*(var(g1_4[,1],na.rm=T)+var(g2_4[,1],na.rm=T)))
							obsES_4_v2=(mean(g1_4[,2],na.rm=T)-mean(g2_4[,2],na.rm=T))/sqrt(.5*(var(g1_4[,2],na.rm=T)+var(g2_4[,2],na.rm=T)))
							TC4_1_v1=length(na.omit(g1_4[,1]))
							TC4_2_v1=length(na.omit(g2_4[,1]))
							TC4_1_v2=length(na.omit(g1_4[,2]))
							TC4_2_v2=length(na.omit(g2_4[,2]))
						
							m1_4_v1=mean(g1_4[,1],na.rm=T)
							m2_4_v1=mean(g2_4[,1],na.rm=T)
							sd1_4_v1=sd(g1_4[,1],na.rm=T)
							sd2_4_v1=sd(g2_4[,1],na.rm=T)
							vi_4_v1=(length(na.omit(g1_4[,1]))+length(na.omit(g2_4[,1])))/((length(na.omit(g1_4[,1])))*(length(na.omit(g2_4[,1]))))+obsES_4_v1^2/(length(na.omit(g1_4[,1]))+length(na.omit(g2_4[,1])))
							sei_4_v1=sqrt(vi_4_v1)
	
							m1_4_v2=mean(g1_4[,2],na.rm=T)
							m2_4_v2=mean(g2_4[,2],na.rm=T)
							sd1_4_v2=sd(g1_4[,2],na.rm=T)
							sd2_4_v2=sd(g2_4[,2],na.rm=T)
							vi_4_v2=(length(na.omit(g1_4[,2]))+length(na.omit(g2_4[,2])))/((length(na.omit(g1_4[,2])))*(length(na.omit(g2_4[,2]))))+obsES_4_v2^2/(length(na.omit(g1_4[,2]))+length(na.omit(g2_4[,2])))
							sei_4_v2=sqrt(vi_4_v2)			

							if((pv_4_v1>.05|obsES_4_v1<0)&(pv_4_v2>.05|obsES_4_v2))
							{
								#if non is significant select the best result
								all_ps=c(pv,pv_2,pv_3_v1,pv_3_v2,pv_4_v1,pv_4_v2)
								all_es=c(obsES,obsES_2,obsES_3_v1,obsES_3_v2,obsES_4_v1,obsES_4_v2)
								if(length(which(all_es>0))>0)
								{
									p_5=min(all_ps[all_es>0])
								}else{
									p_5=max(all_ps[all_es<0])
								}
								obsES_5=all_es[which(all_ps==p_5)][1]
								w=which(all_ps==p_5)[1]
								if(w==1)
								{
									reskQRP[k,]=c(tC,m1,m2,sd1,sd2,obsES,vi,sei,pv,tC)
								}
								if(w==2)
								{
									reskQRP[k,]=c(tC,m1_2,m2_2,sd1_2,sd2_2,obsES_2,vi_2,sei_2,pv_2,tC)
								}
								if(w==3)
								{
									reskQRP[k,]=c(tC+10,m1_3_v1,m2_3_v1,sd1_3_v1,sd2_3_v1,obsES_3_v1,vi_3_v1,sei_3_v1,pv_3_v1,tC+10)
								}
								if(w==4)
								{
									reskQRP[k,]=c(tC+10,m1_3_v2,m2_3_v2,sd1_3_v2,sd2_3_v2,obsES_3_v2,vi_3_v2,sei_3_v2,pv_3_v2,tC+10)
								}
								if(w==5)
								{
									reskQRP[k,]=c(TC4_1_v1,m1_4_v1,m2_4_v1,sd1_4_v1,sd2_4_v1,obsES_4_v1,vi_4_v1,sei_4_v1,pv_4_v1,TC4_2_v1)
								}
								if(w==6)
								{
									reskQRP[k,]=c(TC4_1_v2,m1_4_v2,m2_4_v2,sd1_4_v2,sd2_4_v2,obsES_4_v2,vi_4_v2,sei_4_v2,pv_4_v2,TC4_2_v2)
								}
							}else{
								w2=which(c(pv_4_v1,pv_4_v2)==min(c(pv_4_v1,pv_4_v2)[which(c(obsES_4_v1,obsES_4_v2)>0)]))
								if(w2==1)
								{
									reskQRP[k,]=c(TC4_1_v1,m1_4_v1,m2_4_v1,sd1_4_v1,sd2_4_v1,obsES_4_v1,vi_4_v1,sei_4_v1,pv_4_v1,TC4_2_v1)
								}
								if(w2==2)
								{
									reskQRP[k,]=c(TC4_1_v2,m1_4_v2,m2_4_v2,sd1_4_v2,sd2_4_v2,obsES_4_v2,vi_4_v2,sei_4_v2,pv_4_v2,TC4_2_v2)
								}
							}

						}else{
							w3=which(c(pv_3_v1,pv_3_v2)==min(c(pv_3_v1,pv_3_v2)[which(c(obsES_3_v1,obsES_3_v2)>0)]))
							if(w3==1)
							{
								reskQRP[k,]=c(tC+10,m1_3_v1,m2_3_v1,sd1_3_v1,sd2_3_v1,obsES_3_v1,vi_3_v1,sei_3_v1,pv_3_v1,tC+10)
							}
							if(w3==2)
							{
								reskQRP[k,]=c(tC+10,m1_3_v2,m2_3_v2,sd1_3_v2,sd2_3_v2,obsES_3_v2,vi_3_v2,sei_3_v2,pv_3_v2,tC+10)
							}
						}
					}else{
						reskQRP[k,]=c(tC,m1_2,m2_2,sd1_2,sd2_2,obsES_2,vi_2,sei_2,pv_2,tC)
					}

				}else{
					reskQRP[k,]=c(tC,m1,m2,sd1,sd2,obsES,vi,sei,pv,tC)
				}
			}
			#select first significant small study with positive ES
			if(length(which(resk[,6]>0&resk[,9]<.05))>0)
			{
				resS[i,j,]=resk[which(resk[,6]>0&resk[,9]<.05)[1],]
			}else{
				if(length(which(resk[,6]>0))>0)
				{
					minp=min(resk[which(resk[,6]>0),9])
					resS[i,j,]=resk[which(resk[,9]==minp)[1],] 
				}else{
					resS[i,j,]=resk[which(resk[,9]==max(resk[,9])),]
				}
			}
			#select first significant small study with QRP with positive ES
			if(length(which(reskQRP[,6]>0&reskQRP[,9]<.05))>0)
			{
				resSQRP[i,j,]=reskQRP[which(reskQRP[,6]>0&reskQRP[,9]<.05)[1],]
			}else{
				if(length(which(reskQRP[,6]>0))>0)
				{
					minpQRP=min(reskQRP[which(reskQRP[,6]>0),9])
					resSQRP[i,j,]=reskQRP[which(reskQRP[,9]==minpQRP)[1],] 
				}else{
					resSQRP[i,j,]=reskQRP[which(reskQRP[,9]==max(reskQRP[,9])),]
				}
			}
			
			#large study
			reskL=rep(NA,9)
			g1L=mvrnorm(tL,rep(es[i],2),matrix(c(1,cbdv,cbdv,1),2,2))
			g2L=mvrnorm(tL,rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2))
			m1L=mean(g1L[,1])
			m2L=mean(g2L[,1])
			sd1L=sd(g1L[,1])
			sd2L=sd(g2L[,1])
			obsESL=(m1L-m2L)/sqrt(.5*(var(g1L[,1])+var(g2L[,1])))
			viL=(tL+tL)/(tL*tL)+obsESL^2/(tL+tL)
			seiL=sqrt(viL)
			pvL=t.test(g1L[,1],g2L[,1],var.equal=T)$p.value
			resL[i,j,1:10]=c(tL,m1L,m2L,sd1L,sd2L,obsESL,viL,seiL,pvL,tL)
		
			if(pvL>.05|obsESL<0)
			{
				#test second dependent variable
				m1L_2=mean(g1L[,2])
				m2L_2=mean(g2L[,2])
				sd1L_2=sd(g1L[,2])
				sd2L_2=sd(g2L[,2])
				obsESL_2=(m1L_2-m2L_2)/sqrt(.5*(var(g1L[,2])+var(g2L[,2])))
				viL_2=(tL+tL)/(tL*tL)+obsESL_2^2/(tL+tL)
				seiL_2=sqrt(viL_2)
				pvL_2=t.test(g1L[,2],g2L[,2],var.equal=T)$p.value

				if(pvL_2>.05|obsESL_2<0)
				{
					#add nb extra subjects per cell for both dependent variables
					g1L_set2=mvrnorm(nb,rep(es[1],2),matrix(c(1,cbdv,cbdv,1),2,2))
					g2L_set2=mvrnorm(nb,rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2))
					g1L_3=rbind(g1L,g1L_set2)
					g2L_3=rbind(g2L,g2L_set2)
					pvL_3_v1=t.test(g1L_3[,1],g2L_3[,1],var.equal=T)$p.value
					pvL_3_v2=t.test(g1L_3[,2],g2L_3[,2],var.equal=T)$p.value
					obsESL_3_v1=(mean(g1L_3[,1])-mean(g2L_3[,1]))/sqrt(.5*(var(g1L_3[,1])+var(g2L_3[,1])))
					obsESL_3_v2=(mean(g1L_3[,2])-mean(g2L_3[,2]))/sqrt(.5*(var(g1L_3[,2])+var(g2L_3[,2])))

					m1L_3_v1=mean(g1L_3[,1])
					m2L_3_v1=mean(g2L_3[,1])
					sd1L_3_v1=sd(g1L_3[,1])
					sd2L_3_v1=sd(g2L_3[,1])
					viL_3_v1=(tL+tL+2*nb)/((tL+nb)*(tL+nb))+obsESL_3_v1^2/(tL+tL+2*nb)
					seiL_3_v1=sqrt(viL_3_v1)

					m1L_3_v2=mean(g1L_3[,2])
					m2L_3_v2=mean(g2L_3[,2])
					sd1L_3_v2=sd(g1L_3[,2])
					sd2L_3_v2=sd(g2L_3[,2])
					viL_3_v2=(tL+tL+2*nb)/((tL+nb)*(tL+nb))+obsESL_3_v2^2/(tL+tL+2*nb)
					seiL_3_v2=sqrt(viL_3_v2)			
	
					if((pvL_3_v1>.05|obsESL_3_v1<0)&(pvL_3_v2>.05|obsESL_3_v2))
					{
						#remove outliers
						g1L_4=g1L_3
						g2L_4=g2L_3
						g1L_4[which(abs((g1L_4[,1]-mean(g1L_4[,1]))/sd(g1L_4[,1]))>2)]=NA
						g2L_4[which(abs((g2L_4[,1]-mean(g2L_4[,1]))/sd(g2L_4[,1]))>2)]=NA
						g1L_4[which(abs((g1L_4[,2]-mean(g1L_4[,2]))/sd(g1L_4[,2]))>2)]=NA
						g2L_4[which(abs((g2L_4[,2]-mean(g2L_4[,2]))/sd(g2L_4[,2]))>2)]=NA
				
						pvL_4_v1=t.test(g1L_4[,1],g2L_4[,1],var.equal=T,na.rm=T)$p.value
						pvL_4_v2=t.test(g1L_4[,2],g2L_4[,2],var.equal=T,na.rm=T)$p.value
						obsESL_4_v1=(mean(g1L_4[,1],na.rm=T)-mean(g2L_4[,1],na.rm=T))/sqrt(.5*(var(g1L_4[,1],na.rm=T)+var(g2L_4[,1],na.rm=T)))
						obsESL_4_v2=(mean(g1L_4[,2],na.rm=T)-mean(g2L_4[,2],na.rm=T))/sqrt(.5*(var(g1L_4[,2],na.rm=T)+var(g2L_4[,2],na.rm=T)))
						TCL_1_v1=length(na.omit(g1L_4[,1]))
						TCL_2_v1=length(na.omit(g2L_4[,1]))
						TCL_1_v2=length(na.omit(g1L_4[,2]))
						TCL_2_v2=length(na.omit(g2L_4[,2]))

						m1L_4_v1=mean(g1L_4[,1],na.rm=T)
						m2L_4_v1=mean(g2L_4[,1],na.rm=T)
						sd1L_4_v1=sd(g1L_4[,1],na.rm=T)
						sd2L_4_v1=sd(g2L_4[,1],na.rm=T)
						viL_4_v1=(length(na.omit(g1L_4[,1]))+length(na.omit(g2L_4[,1])))/((length(na.omit(g1L_4[,1])))*(length(na.omit(g2L_4[,1]))))+obsES_4_v1^2/(length(na.omit(g1L_4[,1]))+length(na.omit(g2L_4[,1])))
						seiL_4_v1=sqrt(viL_4_v1)
	
						m1L_4_v2=mean(g1L_4[,2],na.rm=T)
						m2L_4_v2=mean(g2L_4[,2],na.rm=T)
						sd1L_4_v2=sd(g1L_4[,2],na.rm=T)
						sd2L_4_v2=sd(g2L_4[,2],na.rm=T)
						viL_4_v2=(length(na.omit(g1L_4[,2]))+length(na.omit(g2L_4[,2])))/((length(na.omit(g1L_4[,2])))*(length(na.omit(g2L_4[,2]))))+obsES_4_v2^2/(length(na.omit(g1L_4[,2]))+length(na.omit(g2L_4[,2])))
						seiL_4_v2=sqrt(viL_4_v2)			

						if((pvL_4_v1>.05|obsESL_4_v1<0)&(pvL_4_v2>.05|obsESL_4_v2))
						{
							#if non is significant select the best result
							allL_ps=c(pvL,pvL_2,pvL_3_v1,pvL_3_v2,pvL_4_v1,pvL_4_v2)
							allL_es=c(obsESL,obsESL_2,obsESL_3_v1,obsESL_3_v2,obsESL_4_v1,obsESL_4_v2)
							if(length(which(allL_es>0))>0)
							{
								pL_5=min(allL_ps[allL_es>0])
							}else{
								pL_5=max(allL_ps[allL_es<0])
							}
							obsESL_5=allL_es[which(allL_ps==p_5)][1]
							wL=which(allL_ps==pL_5)[1]
							if(wL==1)
							{
								reskL=c(tL,m1L,m2L,sd1L,sd2L,obsESL,viL,seiL,pvL,tL)
							}
							if(wL==2)
							{
								reskL=c(tL,m1L_2,m2L_2,sd1L_2,sd2L_2,obsESL_2,viL_2,seiL_2,pvL_2,tL)
							}
							if(wL==3)
							{
								reskL=c(tL+10,m1L_3_v1,m2L_3_v1,sd1L_3_v1,sd2L_3_v1,obsESL_3_v1,viL_3_v1,seiL_3_v1,pvL_3_v1,tL+10)
							}
							if(wL==4)
							{
								reskL=c(tL+10,m1L_3_v2,m2L_3_v2,sd1L_3_v2,sd2L_3_v2,obsESL_3_v2,viL_3_v2,seiL_3_v2,pvL_3_v2,tL+10)
							}
							if(wL==5)
							{
								reskL=c(TCL_1_v1,m1L_4_v1,m2L_4_v1,sd1L_4_v1,sd2L_4_v1,obsESL_4_v1,viL_4_v1,seiL_4_v1,pvL_4_v1,TCL_2_v1)
							}
							if(wL==6)
							{
								reskL=c(TCL_1_v2,m1L_4_v2,m2L_4_v2,sd1L_4_v2,sd2L_4_v2,obsESL_4_v2,viL_4_v2,seiL_4_v2,pvL_4_v2,TCL_2_v2)
							}
						}else{
							wL2=which(c(pvL_4_v1,pvL_4_v2)==min(c(pvL_4_v1,pvL_4_v2)[which(c(obsESL_4_v1,obsESL_4_v2)>0)]))
							if(wL2==1)
							{
								reskL=c(TCL_1_v1,m1L_4_v1,m2L_4_v1,sd1L_4_v1,sd2L_4_v1,obsESL_4_v1,viL_4_v1,seiL_4_v1,pvL_4_v1,TCL_2_v1)
							}
							if(wL2==2)
							{
								reskL=c(TCL_1_v2,m1L_4_v2,m2L_4_v2,sd1L_4_v2,sd2L_4_v2,obsESL_4_v2,viL_4_v2,seiL_4_v2,pvL_4_v2,TCL_2_v2)
							}
						}

					}else{
						wL3=which(c(pvL_3_v1,pvL_3_v2)==min(c(pvL_3_v1,pvL_3_v2)[which(c(obsESL_3_v1,obsESL_3_v2)>0)]))
						if(wL3==1)
						{
							reskL=c(tL+10,m1L_3_v1,m2L_3_v1,sd1L_3_v1,sd2L_3_v1,obsESL_3_v1,viL_3_v1,seiL_3_v1,pvL_3_v1,tL+10)
						}
						if(wL3==2)
						{
							reskL=c(tL+10,m1L_3_v2,m2L_3_v2,sd1L_3_v2,sd2L_3_v2,obsESL_3_v2,viL_3_v2,seiL_3_v2,pvL_3_v2,tL+10)
						}
					}
				}else{
					reskL=c(tL,m1L_2,m2L_2,sd1L_2,sd2L_2,obsESL_2,viL_2,seiL_2,pvL_2,tL)
				}

			}else{
				reskL=c(tL,m1L,m2L,sd1L,sd2L,obsESL,viL,seiL,pvL,tL)
			}
			resLQRP[i,j,1:10]=reskL
		}
		
		#perform meta-analyses
		m1=metagen(resL[i,,6],resL[i,,8],sm="SMD")
		m2=metagen(resLQRP[i,,6],resLQRP[i,,8],sm="SMD")
		m3=metagen(resS[i,,6],resS[i,,8],sm="SMD")
		m4=metagen(resSQRP[i,,6],resSQRP[i,,8],sm="SMD")

		m1b=rma.uni(yi=resL[i,,6],sei=resL[i,,8],method="FE")
		m2b=rma.uni(yi=resLQRP[i,,6],sei=resLQRP[i,,8],method="FE")
		m3b=rma.uni(yi=resS[i,,6],sei=resS[i,,8],method="FE")
		m4b=rma.uni(yi=resSQRP[i,,6],sei=resSQRP[i,,8],method="FE")		

		#collect estimated ES
		ESExVal[i,1,l]=m1$TE.fixed
		ESExVal[i,2,l]=m2$TE.fixed
		ESExVal[i,3,l]=m3$TE.fixed
		ESExVal[i,4,l]=m4$TE.fixed

		#collect Q values and accompanying p values
		QExVal[i,1,l]=m1$Q
		QExVal[i,2,l]=m2$Q
		QExVal[i,3,l]=m3$Q
		QExVal[i,4,l]=m4$Q
		PQExVal[i,1,l]=1-pchisq(m1$Q,99)
		PQExVal[i,2,l]=1-pchisq(m2$Q,99)
		PQExVal[i,3,l]=1-pchisq(m3$Q,99)
		PQExVal[i,4,l]=1-pchisq(m4$Q,99)

		#collect Bias Z and p values
		RExVal[i,1,l]=regtest(m1b)$zval
		RExVal[i,2,l]=regtest(m2b)$zval
		RExVal[i,3,l]=regtest(m3b)$zval
		RExVal[i,4,l]=regtest(m4b)$zval
		PRExVal[i,1,l]=regtest(m1b)$pval
		PRExVal[i,2,l]=regtest(m2b)$pval
		PRExVal[i,3,l]=regtest(m3b)$pval
		PRExVal[i,4,l]=regtest(m4b)$pval	

		#calculate expected values and collect observed values and perform Ioannidis test
		Exp1=sum(power(m1$TE.fixed,sqrt(1/resL[i,,1]+1/resL[i,,10]),.05,Inf))
		Obs1=length(which(resL[i,,9]<.05))
		Exp2=sum(power(m2$TE.fixed,sqrt(1/resLQRP[i,,1]+1/resLQRP[i,,10]),.05,Inf))
		Obs2=length(which(resLQRP[i,,9]<.05))
		Exp3=sum(power(m3$TE.fixed,sqrt(1/resS[i,,1]+1/resS[i,,10]),.05,Inf))
		Obs3=length(which(resS[i,,9]<.05))
		Exp4=sum(power(m4$TE.fixed,sqrt(1/resSQRP[i,,1]+1/resSQRP[i,,10]),.05,Inf))
		Obs4=length(which(resSQRP[i,,9]<.05))
		IExVal[i,1,l]=(Obs1-Exp1)^2/Exp1 + (Obs1-Exp1)^2/(nSamp-Exp1)
		IExVal[i,2,l]=(Obs2-Exp2)^2/Exp2 + (Obs2-Exp2)^2/(nSamp-Exp2)
		IExVal[i,3,l]=(Obs3-Exp3)^2/Exp3 + (Obs3-Exp3)^2/(nSamp-Exp3)
		IExVal[i,4,l]=(Obs4-Exp4)^2/Exp4 + (Obs4-Exp4)^2/(nSamp-Exp4)
		PIExVal[i,1,l]=1-pchisq(IExVal[i,1,l],1)
		PIExVal[i,2,l]=1-pchisq(IExVal[i,2,l],1)
		PIExVal[i,3,l]=1-pchisq(IExVal[i,3,l],1)
		PIExVal[i,4,l]=1-pchisq(IExVal[i,4,l],1)
	}

	if(l==1)
	{
		#create Figure 4 from first simulated meta-analysis
		pdf("funnel plots final_test.pdf",16,16)
		layout(matrix(c(25,17,17,17,18,18,18,19,19,19,20,20,20,
			21,1,1,1,2,2,2,3,3,3,4,4,4,
			21,1,1,1,2,2,2,3,3,3,4,4,4,
			21,1,1,1,2,2,2,3,3,3,4,4,4,
			22,5,5,5,6,6,6,7,7,7,8,8,8,
			22,5,5,5,6,6,6,7,7,7,8,8,8,
			22,5,5,5,6,6,6,7,7,7,8,8,8,
			23,9,9,9,10,10,10,11,11,11,12,12,12,
			23,9,9,9,10,10,10,11,11,11,12,12,12,
			23,9,9,9,10,10,10,11,11,11,12,12,12,
			24,13,13,13,14,14,14,15,15,15,16,16,16,
			24,13,13,13,14,14,14,15,15,15,16,16,16,
			24,13,13,13,14,14,14,15,15,15,16,16,16
			),13,13,T))

		for(i in 1:4)
		{
			m1=metagen(resL[i,,6],resL[i,,8],sm="SMD")
			funnel(m1,yaxis="invse",contour.levels=.95,xlim=c(-1.5,1.5),ylim=c(2,12),col.contour="light grey",main=paste("1 large study; True ES = ",es[i],sep=""),cex=1.1,cex.lab=1.2,cex.main=1.4)
				legend(-1.7,12,c(paste("Est ES =",round(ESExVal[i,1,l],3)),
					paste("I-Chi(1)=",round(IExVal[i,1,l],3),", ",p0(PIExVal[i,1,l]),sep=""),
					paste("Z=",round(RExVal[i,1,l],3),", ",p0(PRExVal[i,1,l]),sep="")),bty="n",cex=1)
			m2=metagen(resLQRP[i,,6],resLQRP[i,,8],sm="SMD")
			funnel(m2,yaxis="invse",contour.levels=.95,xlim=c(-1.5,1.5),ylim=c(2,12),col.contour="light grey",main=paste("1 large study with QRPs; True ES = ",es[i],sep=""),cex=1.1,cex.lab=1.2,cex.main=1.4)
				legend(-1.7,12,c(paste("Est ES =",round(ESExVal[i,2,l],3)),
					paste("I-Chi(1)=",round(IExVal[i,2,l],3),", ",p0(PIExVal[i,2,l]),sep=""),
					paste("Z=",round(RExVal[i,2,l],3),", ",p0(PRExVal[i,2,l]),sep="")),bty="n",cex=1)
			m3=metagen(resS[i,,6],resS[i,,8],sm="SMD")
			funnel(m3,yaxis="invse",contour.levels=.95,xlim=c(-2,2),ylim=c(1,7.5),col.contour="light grey",main=paste("5 small studies; True ES = ",es[i],sep=""),cex=1.1,cex.lab=1.2,cex.main=1.4)
				legend(-2.27,7.5,c(paste("Est ES =",round(ESExVal[i,3,l],3)),
					paste("I-Chi(1)=",round(IExVal[i,3,l],3),", ",p0(PIExVal[i,3,l]),sep=""),
					paste("Z=",round(RExVal[i,3,l],3),", ",p0(PRExVal[i,3,l]),sep="")),bty="n",cex=1)
			m4=metagen(resSQRP[i,,6],resSQRP[i,,8],sm="SMD")
			funnel(m4,yaxis="invse",contour.levels=.95,xlim=c(-2,2),ylim=c(1,7.5),col.contour="light grey",main=paste("5 small studies with QRPs; True ES = ",es[i],sep=""),cex=1.1,cex.lab=1.2,cex.main=1.4)
				legend(-2.27,7.5,c(paste("Est ES =",round(ESExVal[i,4,l],3)),
					paste("I-Chi(1)=",round(IExVal[i,4,l],3),", ",p0(PIExVal[i,4,l]),sep=""),
					paste("Z=",round(RExVal[i,4,l],3),", ",p0(PRExVal[i,4,l]),sep="")),bty="n",cex=1)
		}
		textplot("1",cex=2.5)
		textplot("2",cex=2.5)
		textplot("3",cex=2.5)
		textplot("4",cex=2.5)
		textplot("A",cex=2.5)
		textplot("B",cex=2.5)
		textplot("C",cex=2.5)
		textplot("D",cex=2.5)
		dev.off()
	}
	print(l)
	flush.console()
}

#save values
#save(ESExVal,file="ESExVal.dat")
#save(QExVal,file="QExVal.dat")
#save(PQExVal,file="PQExVal.dat")
#save(RExVal,file="RExVal.dat")
#save(PRExVal,file="PRExVal.dat")
#save(IExVal,file="IExVal.dat")
#save(PIExVal,file="PIExVal.dat")

#load values
#load("ESExVal.dat")
#load("QExVal.dat")
#load("PQExVal.dat")
#load("RExVal.dat")
#load("PRExVal.dat")
#load("IExVal.dat")
#load("PIExVal.dat")

#calculate expected values for tabel in appendix
ExpValAll=array(NA,c(4,4,7))
for(i in 1:4)
{
	for(j in 1:4)
	{
		ExpValAll[i,j,1]=mean(ESExVal[i,j,])
		ExpValAll[i,j,2]=mean(QExVal[i,j,])
		ExpValAll[i,j,3]=length(which(PQExVal[i,j,]<.05))/nSim
		ExpValAll[i,j,4]=mean(RExVal[i,j,])
		ExpValAll[i,j,5]=length(which(PRExVal[i,j,]<.05))/nSim
		ExpValAll[i,j,6]=mean(IExVal[i,j,])
		ExpValAll[i,j,7]=length(which(PIExVal[i,j,]<.05))/nSim
	}

}
round(ExpValAll[,,1],3)
round(ExpValAll[,,2],3)
round(ExpValAll[,,3],3)
round(ExpValAll[,,4],3)
round(ExpValAll[,,5],3)
round(ExpValAll[,,6],3)
round(ExpValAll[,,7],3)

#save expected values
#save(ExpValAll,file="ExpValAll.dat")






