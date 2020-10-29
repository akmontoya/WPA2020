nsims <- 10 #number of simulations to run
boots <- 1000 #number of bootstrap samples
MCsamples <- 1000 #number of monte carlo samples
corm <- .22 #correlation among repeated measurements of mediator
cory <- 0.32 #correlation among repeated measurements of outcome
aest <- -0.44 #a-path estimates standardized by SD(M)
best <- -0.34 #b-path estimate standardized by SD(M) and SD(Y)
cest <- 0.39 #c-path estimate standardized by SD(Y)
dest <- 0 #d-path estimate standardized by SD(M) and SD(Y)

N <- 30
alpha <- .05

sigm1y1 <- best + dest
sigm2y2 <- best - dest
sigm1y2 <- corm*(best - dest)
sigm2y1 <- corm*(best + dest)

cormat <- matrix(c(1,       corm,    sigm1y1, sigm1y2,
                   corm,    1,       sigm2y1, sigm2y2,
                   sigm1y1, sigm2y1, 1,       cory, 
                   sigm1y2, sigm2y2, cory,    1),
                 nrow = 4, ncol = 4)
dets <- rep(NA, times = 3)
for (i in 2:4){
  dets[i-1] <- det(cormat[1:i, 1:i]) 
}

if(all(dets > 0)){
  Results <- data.frame(matrix(data = NA, nrow = nsims, ncol = 4))
  names(Results) <- c("CausalSteps", "JointSig", "Bootstrap", "MonteCarlo")
  for(i in 1:nsims){
    M1M2Y1Y2 <- data.frame(matrix(rnorm(n = N*4), ncol = 4)%*%chol(cormat))
    names(M1M2Y1Y2) <- c("M1", "M2", "Y1", "Y2")
    M1M2Y1Y2$M2 <- M1M2Y1Y2$M2-aest #is apath on the right scale here?
    M1M2Y1Y2$Y2 <- M1M2Y1Y2$Y2-cest #is cpath on the right scale here?
    contrast <- matrix(c( 1,  1,  0, 
                         -1,  1,  0,
                          0,  0,  1,
                          0,  0, -1), nrow = 4, byrow = TRUE) #need to change to average instead of sum
    MDiffMSumYDiff <- data.frame(as.matrix(M1M2Y1Y2)%*%contrast)
    names(MDiffMSumYDiff) <- c("Mdiff", "Msum", "Ydiff")
    MSumUncentered <- MDiffMSumYDiff$Msum #do I use this?
    MDiffMSumYDiff$Msum <- MDiffMSumYDiff$Msum -mean(MDiffMSumYDiff$Msum)
    
    #Causal Steps
    cpath <- mean(MDiffMSumYDiff[,3])
    secpath <- sd(MDiffMSumYDiff[,3])/sqrt(N)
    cpathsig <- 2*pt(abs(cpath/secpath), df = N-1, lower.tail = FALSE)
    apath <- mean(MDiffMSumYDiff[,1])
    seapath <- sd(MDiffMSumYDiff[,1])/sqrt(N)
    apathsig <- 2*pt(abs(apath/seapath), df = N-1, lower.tail = FALSE)
    bmodel <- lm(Ydiff~Mdiff+Msum, data = MDiffMSumYDiff)
    betas <- coef(bmodel)
    bpath <- betas[2]
    sebpath <- summary(bmodel)$coef[2,2]
    bpathsig <- summary(bmodel)$coef[2,4]
    cppath <- betas[1]
    secppath <- summary(bmodel)$coef[1,2]
    cppathsig <- summary(bmodel)$coef[1,4]
    CausalStepsReject <- ((apathsig < alpha)&(bpathsig < alpha)&(cpathsig<alpha))
    Results[i,1] <- CausalStepsReject
    
    #Joint Significance
    JointSigReject <- ((apathsig < alpha)&(bpathsig < alpha))
    Results[i,2] <- JointSigReject
    
    #Bootstrap CI
    confidence <- 1- alpha
    bootResults <- vector(length = boots)
    for (j in 1:boots){
      rows <- sample(1:N, size = N, replace = TRUE)
      bootdat <- MDiffMSumYDiff[rows, ]
      bootapath <- mean(bootdat$Mdiff)
      bootmodel <- lm(Ydiff~Mdiff+Msum, data = bootdat)
      bootbpath <- coef(bootmodel)[2]
      bootResults[j] <- bootapath*bootbpath 
    }
    bootResults <- sort(bootResults)
    LowerCI = (1-confidence)/2
    UpperCI = 1 - LowerCI
    BootLCI = bootResults[ceiling(boots*LowerCI)] #something weird going on with ceiling function 25 --> 26
    BootUCI = bootResults[ceiling(boots*UpperCI)]
    BootReject = ((BootLCI > 0)|(BootUCI < 0))
    Results[i,3] <- BootReject
    
    #Monte Carlo CI
    asamp <- rnorm(n = MCsamples, mean = apath, sd = seapath)
    bsamp <- rnorm(n = MCsamples, mean = bpath, sd = sebpath)
    absamp <- asamp*bsamp
    absamp <- sort(absamp)
    MCLCI <- absamp[ceiling(MCsamples*LowerCI)]
    MCUCI <- absamp[ceiling(MCsamples*UpperCI)]
    MCReject <- ((MCLCI >0)|(MCUCI <0))
    Results[i,4] <- MCReject
  }
  
testpower <- colMeans(Results)
print(testpower)
  
}
if(all(dets <= 0)){
  print("Impossible Parameter Combinations")
}



