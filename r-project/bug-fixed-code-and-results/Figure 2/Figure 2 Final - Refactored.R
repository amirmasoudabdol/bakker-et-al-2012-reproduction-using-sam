#This R-code is used to generate Figure 2. For each point in the es vector, the 4
#strategies as descibed in the paper are applied 10000 times. The estimated effect
#sizes, calculated p values, the number of studies needed, and which QRPs are applied
#are collected. 

#required packages
library(MASS)

#variables

source("Figure 2/helperFunctions.R")

nsim <- 2500			#number of simulations
es <- seq(0.5, 1, by= .05)	#vector with effectsizes	
nS <- c(5, 10, 20)		#cell size of small study, total sample size will be 2 times nS: 5, 10 and 20
# nS <- c(10, 20, 40)
mf <- 5				#multiplication factor
nreps <- 5
nL <- mf*nS			#cell size of large study
alpha <- .05			#used alpha level		
cbdv <- .5			#correlation between dependent variables
nb <- 100				#number of subjects added (per cell)

Sigma <- matrix(c(1.0,   0.5,   0.0,   0.0,
                  0.5,   1.0,   0.0,   0.0,
                  0.0,   0.0,   1.0,   0.5,
                  0.0,   0.0,   0.5,   1.0), 4, 4)

#simulation

#create arrays with p values and effect sizes
psAr <- esAr <- selAr <- array(NA,c(length(nS),length(es),nsim,4))
# Create array for the category of QRPs and number of studies. 
# QRPs of large study = [,,,1], QRPs of multiple small studies = [,,,2].
# Coded: 
#   1 = No QRPs,
#   2 = Second dependent variable,
#   3 = Adding subjects,
#   4 = Removing outliers,
#   5 = Used all QRPs but did not find significant result.
# In the same array the number of studies performed without QRPs = [,,,3], 
# and the number of studies with QRPs = [,,,4]
catQRPAr <- array(NA,c(length(nS),length(es),nsim,4))

for(i in 1:length(nS))
{
  for(j in 1:length(es))
  {
    for(k in 1:nsim)
    {
      #one large study

      g12L <- mvrnorm(nL[i], c(rep(es[j], 2), 0, 0), Sigma)
      g1L <- g12L[, c(1, 2)]
      g2L <- g12L[, c(3, 4)]
      
      pL <- calculatePvalue(g1L[,1], g2L[,1])
      esL <- calculateEffect(g1L[,1], g2L[,1])
      
      #> Storing Large with noQRP right away
      psAr[i,j,k,1] <- pL
      esAr[i,j,k,1] <- esL
      
      if(pL>alpha|esL<0)
      {
        #test second dependent variable
        pL_2 <- calculatePvalue(g1L[,2], g2L[,2])
        esL_2 <- calculateEffect(g1L[,2], g2L[,2])
        
        # Add Extra Subjects
        #> If NIETHER of the results where sig with postive effect
        if(pL_2>alpha|esL_2<0)
        {
          #add nb extra subjects per cell for both dependent variables

          os.res <- optionalStopping(g1L, g2L, es[j], Sigma, nb)
           g1L_3 <- os.res$os.g1
           g2L_3 <- os.res$os.g2
          
          pL_3_v1 <- calculatePvalue(g1L_3[,1], g2L_3[,1])
          esL_3_v1 <- calculateEffect(g1L_3[,1], g2L_3[,1])
          
          pL_3_v2 <- calculatePvalue(g1L_3[,2], g2L_3[,2])
          esL_3_v2 <- calculateEffect(g1L_3[,2], g2L_3[,2])
          
          # Remove Outliers
          #> If EITHER of the results where sig with postive effect
          #> If NIETHER of them are satisfactory
          if((pL_3_v1>alpha|esL_3_v1<0)&(pL_3_v2>alpha|esL_3_v2<0))
          {
            #remove outliers
            g1L_4 <- g1L_3
            g2L_4 <- g2L_3
            g1L_4[which(abs((g1L_4[,1]-mean(g1L_4[,1]))/sd(g1L_4[,1]))>2)] <- NA
            g2L_4[which(abs((g2L_4[,1]-mean(g2L_4[,1]))/sd(g2L_4[,1]))>2)] <- NA
            g1L_4[which(abs((g1L_4[,2]-mean(g1L_4[,2]))/sd(g1L_4[,2]))>2)] <- NA
            g2L_4[which(abs((g2L_4[,2]-mean(g2L_4[,2]))/sd(g2L_4[,2]))>2)] <- NA
            
            pL_4_v1 <- calculatePvalue(g1L_4[,1], g2L_4[,1])
            esL_4_v1 <- calculateEffect(g1L_4[,1], g2L_4[,1])
            
            pL_4_v2 <- calculatePvalue(g1L_4[,2], g2L_4[,2])
            esL_4_v2 <- calculateEffect(g1L_4[,2], g2L_4[,2])
            
            #> If nothing helped and our hacking were ineffective 
            if((pL_4_v1>alpha|esL_4_v1<0)&(pL_4_v2>alpha|esL_4_v2<0))
            {
              #if non is significant select the best result
              all_psL=c(pL,pL_2,pL_3_v1,pL_3_v2,pL_4_v1,pL_4_v2)
              all_esL=c(esL,esL_2,esL_3_v1,esL_3_v2,esL_4_v1,esL_4_v2)
              
              pL_5 = chooseBetweenOutcomes(all_esL, all_psL)
              esL_5=all_esL[which(all_psL==pL_5)][1]
              
              #> Storing Large with QRP
              psAr[i,j,k,2] <- pL_5
              esAr[i,j,k,2] <- esL_5
              catQRPAr[i,j,k,1] <- 5
            }else{
              min.pos <- selectMinPos(c(esL_4_v1,esL_4_v2), c(pL_4_v1,pL_4_v2))
              psAr[i,j,k,2] <- min.pos[1]
              esAr[i,j,k,2] <- min.pos[2]
            }
          }else{
            min.pos <- selectMinPos(c(esL_3_v1,esL_3_v2), c(pL_3_v1,pL_3_v2))
            psAr[i,j,k,2] <- min.pos[1]
            esAr[i,j,k,2] <- min.pos[2]
          }
        }else{
          #> Storing Large with QRP
          psAr[i,j,k,2] <- pL_2
          esAr[i,j,k,2] <- esL_2
          catQRPAr[i,j,k,1] <- 2
        }
      }else{
        #> Storing Large with QRP
        psAr[i,j,k,2] <- pL
        esAr[i,j,k,2] <- esL
        catQRPAr[i,j,k,1] <- 1
      }
      
      
      #multiple small studies
      
      #temperory vectors to collect the results of the small studies
      pS=esSV=catS=pSQRP=esSQRP=catSQRP=rep(NA, nreps)
      for(l in 1:nreps)
      {

        g12 <- mvrnorm(nS[i], c(rep(es[j], 2), 0, 0), Sigma)
        g1 <- g12[, c(1, 2)]
        g2 <- g12[, c(3, 4)]
        
        p <- calculatePvalue(g1[,1], g2[,1])
        esS <- calculateEffect(g1[,1], g2[,1])
        
        #> Storing Small without QRP
        pS[l] <- p
        esSV[l] <- esS	
        
        if(p>alpha|esS<0)
        {
          #test second dependent variable
          p_2 <- calculatePvalue(g1[,2], g2[,2])
          es_2 <- calculateEffect(g1[,2], g2[,2])
          
          # Add new observations
          #> If NIETHER of the results where sig with postive effect
          if(p_2>alpha|es_2<0)
          {

            os.res <- optionalStopping(g1, g2, es[j], Sigma, nb)
             g1_3 <- os.res$os.g1
             g2_3 <- os.res$os.g2
            
            p_3_v1 <- calculatePvalue(g1_3[,1], g2_3[,1])
            es_3_v1 <- calculateEffect(g1_3[,1], g2_3[,1])
            
            p_3_v2 <- calculatePvalue(g1_3[,2], g2_3[,2])
            es_3_v2 <- calculateEffect(g1_3[,2], g2_3[,2])
            
            # Remove outliers
            #> If NIETHER of the results where sig with postive effect
            if((p_3_v1>alpha|es_3_v1<0)&(p_3_v2>alpha|es_3_v2<0))
            {
              
              g1_4 <- g1_3
              g2_4 <- g2_3
              g1_4[which(abs((g1_4[,1]-mean(g1_4[,1]))/sd(g1_4[,1]))>2)] <- NA
              g2_4[which(abs((g2_4[,1]-mean(g2_4[,1]))/sd(g2_4[,1]))>2)] <- NA
              g1_4[which(abs((g1_4[,2]-mean(g1_4[,2]))/sd(g1_4[,2]))>2)] <- NA
              g2_4[which(abs((g2_4[,2]-mean(g2_4[,2]))/sd(g2_4[,2]))>2)] <- NA
              
              p_4_v1 <- calculatePvalue(g1_4[,1], g2_4[,1])
              es_4_v1 <- calculateEffect(g1_4[,1], g2_4[,1])
              
              p_4_v2 <- calculatePvalue(g1_4[,2], g2_4[,2])
              es_4_v2 <- calculateEffect(g1_4[,2], g2_4[,2])
              
              #> If nothing helped and our hacking were ineffective 
              if((p_4_v1>alpha|es_4_v1<0)&(p_4_v2>alpha|es_4_v2<0))
              {
                #if non is significant select the best result
                all_ps=c(p,p_2,p_3_v1,p_3_v2,p_4_v1,p_4_v2)
                all_es=c(esS,es_2,es_3_v1,es_3_v2,es_4_v1,es_4_v2)
                
                p_5 = chooseBetweenOutcomes(all_es, all_ps)
                es_5=all_es[which(all_ps==p_5)][1]
                
                print(es[j])
                print(p_5)
                
                pSQRP[l] <- p_5
                esSQRP[l] <- es_5
                catSQRP[l] <- 5
              }else{
                min.pos <- selectMinPos(c(es_4_v1,es_4_v2), c(p_4_v1,p_4_v2))
                pSQRP[l] <- min.pos[1]
                esSQRP[l] <- min.pos[2]
              }
            }else{
              min.pos <- selectMinPos(c(es_3_v1,es_3_v2), c(p_3_v1,p_3_v2))
              pSQRP[l] <- min.pos[1]
              esSQRP[l] <- min.pos[2]
            }
          }else{
            pSQRP[l] <- p_2
            esSQRP[l] <- es_2
            catSQRP[l] <- 2
          }
        }else{
          pSQRP[l] <- p
          esSQRP[l] <- esS
          catSQRP[l] <- 1
        }
      }
      
      # Select first significant study without QRPs with a positive ES if non is significant, select best study. The best study is the study with max p-value between those with min negative effect
      #> Selecting between small studies witnout QRP, we have 5 of them in pS and esSV
      selP <- chooseBetweenReplications(esSV, pS)
      
      # Adding the selected result without QRP to collection
      psAr[i,j,k,3] <- selP
      esAr[i,j,k,3] <- esSV[which(pS==selP)][1]
      
      # Select first significant study with QRPs with a positive ES if non is significant, select best study.
      #> Selecting between small studies with QRP, there are in pSQRP, and esSQRP
      selPQRP <- chooseBetweenReplications(esSQRP, pSQRP)
      
      # Adding the selected result with QRP to collection
      psAr[i,j,k,4] <- selPQRP
      esAr[i,j,k,4] <- esSQRP[which(pSQRP==selPQRP)][1]
      catQRPAr[i,j,k,2] <- catSQRP[which(pSQRP==selPQRP)][1]
      
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
      pProp[i,(j-1)*4+k] <- length(which(psAr[j,i,,k][which(esAr[j,i,,k]>0)]<alpha))/length(psAr[j,i,,k])
    }
  }
}


#save and load (earlier) created data
save(esAr,file="Figure 2/esAr.dat")
write.csv2(pProp,"Figure 2/pProp.csv")
#load("esAr.dat")
#pProp=read.csv2("pProp.csv")[,-1]


#create pdf of figure 2
pdf("proportions and bias final.pdf",7,10)
# postscript("figure 2.ps",paper="special",width=7,height=10)
lab=c("1 large study","1 large study with QRPs","5 small studies","5 small studies with QRPs")
layout(matrix(1:6,3,2))

plot(es,pProp[,1],type="l",col=1,lty=1,ylim=c(0,1),main=paste("Chance of getting at least 1 significant result \n Small N = ", nS[1], sep=""),xlab="ES",ylab="proportion")
lines(es,pProp[,2],type="l",col=2,lty=1)
lines(es,pProp[,3],type="l",col=1,lty=2)
lines(es,pProp[,4],type="l",col=2,lty=2)
legend(.5,.3,lab,col=1:2,lty=c(1,1,2,2),bty="n",cex=.8)

plot(es,pProp[,5],type="l",col=1,lty=1,ylim=c(0,1),main=paste("Chance of getting at least 1 significant result \n Small N = ", nS[2], sep=""),xlab="ES",ylab="proportion")
lines(es,pProp[,6],type="l",col=2,lty=1)
lines(es,pProp[,7],type="l",col=1,lty=2)
lines(es,pProp[,8],type="l",col=2,lty=2)
legend(.5,.3,lab,col=1:2,lty=c(1,1,2,2),bty="n",cex=.8)

plot(es,pProp[,9],type="l",col=1,lty=1,ylim=c(0,1),main=paste("Chance of getting at least 1 significant result \n Small N = ", nS[3], sep=""),xlab="ES",ylab="proportion")
lines(es,pProp[,10],type="l",col=2,lty=1)
lines(es,pProp[,11],type="l",col=1,lty=2)
lines(es,pProp[,12],type="l",col=2,lty=2)
legend(.5,.3,lab,col=1:2,lty=c(1,1,2,2),bty="n",cex=.8)

plot(es,apply(esAr[1,,,1],1,mean)-es,type="l",col=1,lty=1,ylim=c(0,1.2),main=paste("ES bias; Small N = ", nS[1], sep=""),xlab="ES",ylab="bias")
lines(es,apply(esAr[1,,,2],1,mean)-es,type="l",col=2,lty=1)
lines(es,apply(esAr[1,,,3],1,mean)-es,type="l",col=1,lty=2)
lines(es,apply(esAr[1,,,4],1,mean)-es,type="l",col=2,lty=2)
legend(0,1.2,lab,col=1:2,lty=c(1,1,2,2),bty="n",cex=.8)

plot(es,apply(esAr[2,,,1],1,mean)-es,type="l",col=1,lty=1,ylim=c(0,1.2),main=paste("ES bias; Small N = ", nS[2], sep=""),xlab="ES",ylab="bias")
lines(es,apply(esAr[2,,,2],1,mean)-es,type="l",col=2,lty=1)
lines(es,apply(esAr[2,,,3],1,mean)-es,type="l",col=1,lty=2)
lines(es,apply(esAr[2,,,4],1,mean)-es,type="l",col=2,lty=2)
legend(0,1.2,lab,col=1:2,lty=c(1,1,2,2),bty="n",cex=.8)

plot(es,apply(esAr[3,,,1],1,mean)-es,type="l",col=1,lty=1,ylim=c(0,1.2),main=paste("ES bias; Small N = ", nS[3], sep=""),xlab="ES",ylab="bias")
lines(es,apply(esAr[3,,,2],1,mean)-es,type="l",col=2,lty=1)
lines(es,apply(esAr[3,,,3],1,mean)-es,type="l",col=1,lty=2)
lines(es,apply(esAr[3,,,4],1,mean)-es,type="l",col=2,lty=2)
legend(0,1.2,lab,col=1:2,lty=c(1,1,2,2),bty="n",cex=.8)

dev.off()


