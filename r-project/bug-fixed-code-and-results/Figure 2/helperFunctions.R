chooseBetweenReplications <- function(eS, pS) {
  alpha <- 0.05
  
  # if (length(which((pS < alpha) & (eS > 0))) > 0) {
  #   selP <- pS[which((pS < alpha) & (eS > 0))][1]
  #   return(selP)
  # }
  # 
  # if (length(which(eS > 0)) > 0) {
  #   selP <- min(pS[which(eS > 0)])
  #   return(selP)
  # }
  # 
  # selP <- max(pS[which(eS < 0)])
  
  # selP <- min(pS)
  selP <- pS[length(pS)]
  
  return(selP)
}

chooseBetweenOutcomes <- function(eS, pS) {

  if (length(which(eS > 0)) > 0){
    selP <- min(pS[eS > 0])
  }else{
    selP <- max(pS[eS < 0])
  }
  
  return(selP)
}

selectMinPos <- function(eS, pS) {
  # selP <- min(pS[which(eS>0)])
  selP <- pS[length(pS)]
  selE <- eS[which(pS==selP)]
  return(c(selP, selE))
}

calculateEffect <- function(g1, g2) {
  return((mean(g1, na.rm = T) - mean(g2, na.rm = T)) / sqrt(.5 * (var(g1, na.rm = T) + var(g2, na.rm = T))))
}

calculatePvalue <- function(g1, g2) {
  return(t.test(g1, g2, var.equal = T)$p.value)
}

optionalStopping <- function(g1, g2, es, Sigma, n) {
  new.obs <- mvrnorm(n, c(rep(es, 2), 0, 0), Sigma)

  new.g1 <- rbind(g1, new.obs[, c(1, 2)])
  new.g2 <- rbind(g2, new.obs[, c(3, 4)])

  return(list(os.g1 = new.g1, os.g2 = new.g2))
}
