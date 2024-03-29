forecastx <-
  function(fit,
           fscale = NULL,
           catchval = NULL,
           catchval.exact = NULL,
           fval = NULL,
           nextssb = NULL,
           landval = NULL,
           cwF = NULL,
           nosim = 1000,
           year.base = max(fit$data$years),
           ave.years = max(fit$data$years) + (-4:0),
           rec.years = max(fit$data$years) + (-9:0),
           label = NULL,
           overwriteSelYears = NULL,
           deterministic = FALSE,
           processNoiseF = TRUE,
           customWeights = NULL,
           customSel = NULL,
           lagR = FALSE,
           splitLD = FALSE,
           addTSB = FALSE) {
    
  resample <- function(x, ...){
    if(deterministic){
      ret <- mean(x)
    }else{
      ret <- x[sample.int(length(x), ...)]
    }
    return(ret)
  }
  ns<-max(length(fscale), length(catchval), length(catchval.exact), length(fval), length(nextssb), length(cwF))    
  if(missing(fscale)&missing(fval)&missing(catchval)&missing(catchval.exact)&missing(nextssb)&missing(cwF))stop("No scenario is specified")    
  if(missing(fscale)) fscale <- rep(NA,ns)
  if(missing(fval)) fval <- rep(NA,ns)
  if(missing(catchval)) catchval <- rep(NA,ns)
  if(missing(catchval.exact)) catchval.exact <- rep(NA,ns)    
  if(missing(nextssb)) nextssb <-rep(NA,ns)
  if(missing(landval)) landval <-rep(NA,ns)  
  if(missing(cwF)) cwF <-rep(NA,ns)  
  
  if(!all(rowSums(!is.na(cbind(fscale, catchval, catchval.exact, fval, nextssb, landval, cwF)))==1)){
    stop("For each forecast year exactly one of fscale, catchval or fval must be specified (all others must be set to NA)")
  }
  
  if(!is.null(overwriteSelYears)){
    fromto <- fit$conf$fbarRange-(fit$conf$minAge-1)  
    Ftab <- faytable(fit)
    fixedsel <- colMeans(Ftab[as.integer(rownames(Ftab))%in%overwriteSelYears,,drop=FALSE])
    fixedsel <- fixedsel/mean(fixedsel[fromto[1]:fromto[2]])
  }
  
  if(!is.null(customSel)){
    fromto <- fit$conf$fbarRange-(fit$conf$minAge-1)  
    customSel <- customSel/mean(customSel[fromto[1]:fromto[2]])
  }
  
  getF <- function(x, allowSelOverwrite=FALSE){
    idx <- fit$conf$keyLogFsta[1,]+1
    nsize <- length(idx)
    ret <- exp(x[nsize+idx])
    ret[idx==0] <- 0
    if(allowSelOverwrite){
      if(!is.null(overwriteSelYears)){
        fromto <- fit$conf$fbarRange-(fit$conf$minAge-1)    
        thisfbar<-mean(ret[fromto[1]:fromto[2]])
        ret<-fixedsel*thisfbar
      }
      if(!is.null(customSel)){
        fromto <- fit$conf$fbarRange-(fit$conf$minAge-1)    
        thisfbar<-mean(ret[fromto[1]:fromto[2]])
        ret<-customSel*thisfbar
      }
    }
    ret
  }
  
  fbar<-function(x){
    fromto <- fit$conf$fbarRange-(fit$conf$minAge-1)  
    mean(getF(x)[fromto[1]:fromto[2]])
  }
  
  fbarFrac<-function(x, lf){
    fromto <- fit$conf$fbarRange-(fit$conf$minAge-1)  
    mean((lf*getF(x))[fromto[1]:fromto[2]])
  }    
  
  getCWF<-function(x,w){
    sum(getF(x)*w)
  }    
  
  getN <- function(x){
    idx <- fit$conf$keyLogFsta[1,]+1
    nsize <- length(idx)
    ret <- exp(x[1:nsize])
    ret
  }
  
  getState <- function(N,F){
    k <- fit$conf$keyLogFsta[1,]
    F <- F[k>=0]
    k <- unique(k[k>=0]+1)
    x <- log(c(N,F[k]))
    x
  }    
  
  getProcessVar <- function(fit){
    cof <- coef(fit)
    sdN <- exp(fit$pl$logSdLogN[fit$conf$keyVarLogN+1])
    sdN[1]<-0
    nN <- length(sdN)
    sdF <- exp(cof[names(cof)=="logSdLogFsta"][fit$conf$keyVarF+1])
    if(processNoiseF==FALSE){
      sdF <- sdF*0
    }
    k<-fit$conf$keyLogFsta[1,]
    sdF <- sdF[k>=0]
    k <- unique(k[k >= 0] + 1)
    sdF <-sdF[k]
    nF <- length(sdF)
    if(fit$conf$corFlag==0){
      corr <- diag(nF)    
    }
    if(fit$conf$corFlag==1){
      y <- cof[names(cof)=="itrans_rho"]
      rho <- 2/(1+exp(-2*y))-1
      corr <- matrix(rho, nrow=nF, ncol=nF)
      diag(corr) <- 1
    }
    if(fit$conf$corFlag==2){
      y <- cof[names(cof)=="itrans_rho"]
      rho <- 2/(1+exp(-2*y))-1
      corr <- diag(sdF)
      corr <- rho^abs(row(corr)-col(corr))
    }
    cov <- matrix(0,nrow=nN+nF,ncol=nN+nF)
    cov[1:nN,1:nN] <- diag(sdN^2)
    
    if(fit$conf$corFlag <3){
      cov[nN+1:nF,nN+1:nF] <- (sdF%*%t(sdF))*corr
    }else{
      if(fit$conf$corFlag ==3){
        sdU <- exp(cof[names(cof)=="sepFlogSd"][1])
        sdV <- exp(cof[names(cof)=="sepFlogSd"][2])
        
        diag(cov[nN+1:(nF-1),nN+1:(nF-1)]) <- sdU^2
        cov[nN+nF,nN+nF] <- sdV^2
      }
    }
    cov
  }
  
  step <- function(x, nm, recpool, scale, inyear=FALSE){
    F <- getF(x, allowSelOverwrite=!inyear)
    N <- getN(x)
    if(!inyear){
      Z <- F+nm
      n <- length(N)
      N <- c(resample(recpool,1),N[-n]*exp(-Z[-n])+c(rep(0,n-2),N[n]*exp(-Z[n])))
    }
    F <- F*scale
    xx <- getState(N,F)
    return(xx)
  }
  
  scaleF <- function(x, scale){
    F <- getF(x)*scale
    N <- getN(x)
    xx <- getState(N,F)
    return(xx)
  }
  
  sel<-function(x){
    getF(x)/fbar(x)
  }
  
  catch <- function(x, nm, cw){
    F <- getF(x)
    Z <- F+nm
    N <- getN(x)
    C <- F/Z*(1-exp(-Z))*N
    return(sum(cw*C))
  }
  
  catchFrac <- function(x, nm, w, frac){
    F <- getF(x)
    Z <- F+nm
    N <- getN(x)
    C <- F/Z*(1-exp(-Z))*N
    return(sum(frac*w*C))
  }
  
  catchatage <- function(x, nm){
    F <- getF(x)
    Z <- F+nm
    N <- getN(x)
    C <- F/Z*(1-exp(-Z))*N
    return(C)
  }
  
  ssb <- function(x, nm, sw, mo, pm, pf){
    F <- getF(x)
    N <- getN(x)*exp(-pm*nm-pf*F)
    return(sum(N*mo*sw))
  }
  
  tsb <- function(x, sw){
    F <- getF(x)
    N <- getN(x)
    return(sum(N*sw))
  }
  
  rectab<-rectable(fit)
  recpool<-rectab[rownames(rectab)%in%rec.years,1]
  
  # Get final state
  if(year.base==max(fit$data$years)){
    est <- fit$sdrep$estY
    cov <- fit$sdrep$covY
  }
  if(year.base==(max(fit$data$years)-1)){  
    est <- fit$sdrep$estYm1
    cov <- fit$sdrep$covYm1
  }
  if(year.base<(max(fit$data$years)-1)){
    stop("State not saved, so cannot proceed from this year")
  }
  if(deterministic)cov<-cov*0
  sim<-rmvnorm(nosim, mu=est, Sigma=cov)
  
  if(is.null(overwriteSelYears) & is.null(customSel)){  
    if(!all.equal(est,getState(getN(est),getF(est))))stop("Sorry somthing is wrong here (check code for getN, getF, and getState)")  
  }
  
  doAve <- function(x,y)colMeans(x[rownames(x)%in%ave.years,,drop=FALSE]) 
  ave.sw <- doAve(fit$data$stockMeanWeight)
  ave.cw <- doAve(fit$data$catchMeanWeight)
  ave.mo <- doAve(fit$data$propMat)
  ave.nm <- doAve(fit$data$natMor)
  ave.lf <- doAve(fit$data$landFrac)
  ave.lw <- doAve(fit$data$landMeanWeight)
  ave.pm <- doAve(fit$data$propM)
  ave.pf <- doAve(fit$data$propF)
  getThisOrAve <- function(x,y, ave){
    if(y %in% rownames(x)){
      ret <- x[which(rownames(x)==y),]
    }else{
      ret <- ave
    }
    ret
  }
  procVar<-getProcessVar(fit)  
  simlist<-list()
  for(i in 0:(length(fscale)-1)){
    y<-year.base+i  
    
    sw<-getThisOrAve(fit$data$stockMeanWeight, y, ave.sw)
    cw<-getThisOrAve(fit$data$catchMeanWeight, y, ave.cw)
    mo<-getThisOrAve(fit$data$propMat, y, ave.mo)
    nm<-getThisOrAve(fit$data$natMor, y, ave.nm)
    lf<-getThisOrAve(fit$data$landFrac, y, ave.lf)    
    lw<-getThisOrAve(fit$data$landMeanWeight, y, ave.lw)
    pm<-getThisOrAve(fit$data$propM, y, ave.pm)
    pf<-getThisOrAve(fit$data$propF, y, ave.pf)
    
    sim <- t(apply(sim, 1, function(s)step(s, nm=nm, recpool=recpool, scale=1, inyear=(i==0))))
    if(i!=0){
      cof <- coef(fit)
      if(deterministic){procVar<-procVar*0}
      if(!is.null(overwriteSelYears)){nn<-length(fit$conf$keyLogFsta[1,]); procVar[-c(1:nn),-c(1:nn)] <- 0}
      if(!is.null(customSel)){nn<-length(fit$conf$keyLogFsta[1,]); procVar[-c(1:nn),-c(1:nn)] <- 0}
      
      if(fit$conf$corFlag <3){
        sim <- sim + rmvnorm(nosim, mu=rep(0,nrow(procVar)), Sigma=procVar)
      }else if(fit$conf$corFlag ==3){
        k<-fit$conf$keyLogFsta[1,]
        k <- unique(k[k >= 0] + 1)
        nF <- length(k)
        simTmp <- rmvnorm(nosim, mu=rep(0,nrow(procVar)), Sigma=procVar)
        rhoF = 2/(1 + exp(-2*cof[names(cof)=="sepFlogitRho"])) -1
        sepFalpha = cof[names(cof)=="sepFalpha"]
        for(jj in 1:nosim){
          #Calculates previous U and V.
          V = mean(sim[jj,(dim(sim)[2]-nF+1):(dim(sim)[2])]) 
          U = sim[jj,(dim(sim)[2]-nF+1):(dim(sim)[2]-1)] -V   
          
          #Extract new contributions to U and V
          Uepsilon = simTmp[jj,(dim(simTmp)[2]-nF+1):(dim(simTmp)[2]-1)]
          Vepsilon = simTmp[jj,dim(simTmp)[2]]
          
          #Calculates updated U and V
          Unew = rhoF[1]*U + Uepsilon+ sepFalpha[1:(length(sepFalpha)-1)] 
          Vnew = rhoF[2]*V + Vepsilon+ sepFalpha[length(sepFalpha)]
          
          #Calculate F based on U and V
          sim[jj,(dim(simTmp)[2]-nF+1):(dim(simTmp)[2]-1)] = Unew + Vnew
          sim[jj,dim(simTmp)[2]] =  -sum(Unew) + Vnew #subtract sum(Unew) since U sum to 0 over all ages
          
          #Update process for N
          sim[jj,1:(dim(simTmp)[2]-nF)] = sim[jj,1:(dim(simTmp)[2]-nF)]   + simTmp[jj,1:(dim(simTmp)[2]-nF)] #Simulated N is not affected by the separable structure of F
        }
      }else{
        stop("forcast not implemented for the given corflag")
      }
    }
    
    
    if(!is.na(fscale[i+1])){
      sim<-t(apply(sim, 1, scaleF, scale=fscale[i+1]))    
    }
    
    if(!is.na(fval[i+1])){
      curfbar<-median(apply(sim, 1, fbar))
      adj<-fval[i+1]/curfbar
      sim<-t(apply(sim, 1, scaleF, scale=adj))    
    }
    
    if(!is.na(cwF[i+1])){
      if(missing(customWeights))stop("customWeights must be supplied when using the cwF option")  
      curcwF<-median(apply(sim, 1, getCWF, w=customWeights))
      adj<-cwF[i+1]/curcwF
      sim<-t(apply(sim, 1, scaleF, scale=adj))    
    }
    
    if(!is.na(catchval[i+1])){
      simtmp<-NA
      fun<-function(s){
        simtmp<<-t(apply(sim, 1, scaleF, scale=s))
        simcat<-apply(simtmp, 1, catch, nm=nm, cw=cw)
        return(catchval[i+1]-median(simcat))
      }
      ff <- uniroot(fun, c(0,100))$root
      sim <- simtmp
    }
    
    if(!is.na(catchval.exact[i+1])){
      simtmp<-sim
      funk<-function(k){
        one<-function(s){
          simtmp[k,]<<-scaleF(sim[k,],s)
          simcat<-catch(simtmp[k,], nm=nm, cw=cw)
          return(catchval.exact[i+1]-simcat)
        }
        ff <- uniroot(one, c(0,100))$root
      }
      dd <- sapply(1:nrow(sim),funk)
      sim <- simtmp
    }
    
    if(!is.na(landval[i+1])){
      simtmp<-NA
      fun<-function(s){
        simtmp<<-t(apply(sim, 1, scaleF, scale=s))
        simcat<-apply(simtmp, 1, catchFrac, nm=nm, w=lw, frac=lf)
        return(landval[i+1]-median(simcat))
      }
      ff <- uniroot(fun, c(0,100))$root
      sim <- simtmp
    }
    
    if(!is.na(nextssb[i+1])){
      if((sum(pm)!=0) | (sum(pf)!=0))stop("nextssb option only available if SSB is calculated in the beginning of the year")  
      simtmp<-NA
      fun<-function(s){
        simtmp<<-t(apply(sim, 1, scaleF, scale=s))
        simsim<-t(apply(simtmp, 1, function(s)step(s, nm=nm, recpool=recpool, scale=1, inyear=(i==0))))
        if(i!=0){
          if(deterministic)procVar<-procVar*0  
          simsim <- simsim + rmvnorm(nosim, mu=rep(0,nrow(procVar)), Sigma=procVar)
        }
        simnextssb<-apply(simsim, 1, ssb, nm=nm, sw=sw, mo=mo, pm=pm, pf=pf)
        return(nextssb[i+1]-median(simnextssb))
      }
      ff <- uniroot(fun, c(0,100))$root
      sim <- simtmp
    }
    
    fbarsim <- apply(sim, 1, fbar)
    fbarLsim <- apply(sim, 1, fbarFrac, lf=lf)
    catchsim <- apply(sim, 1, catch, nm=nm, cw=cw)
    landsim <- apply(sim, 1, catchFrac, nm=nm, w=lw, frac=lf)
    catchatagesim <- apply(sim, 1, catchatage, nm=nm)
    ssbsim <- apply(sim, 1, ssb, nm=nm, sw=sw, mo=mo, pm=pm, pf=pf)
    tsbsim <- apply(sim, 1, tsb, sw=sw)
    if(lagR){
      recsim <- exp(sim[,2])
    }else{
      recsim <- exp(sim[,1])
    }
    cwFsim <- rep(NA,nrow(sim))
    if(!missing(customWeights)){
      cwFsim <- apply(sim, 1, getCWF, w=customWeights)
    }
    simlist[[i+1]] <- list(sim=sim, fbar=fbarsim, catch=catchsim, ssb=ssbsim, rec=recsim,
                           cwF=cwFsim, catchatage=catchatagesim, land=landsim, fbarL=fbarLsim, tsb=tsbsim, year=y)
  }
  
  attr(simlist, "fit")<-fit
  
  collect <- function(x){
    quan <- quantile(x, c(.50,.025,.975))
    c(median=quan[1], low=quan[2], high=quan[3])
  }
  
  fbar <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$fbar))),3)
  fbarL <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$fbarL))),3)  
  rec <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$rec))))
  ssb <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$ssb))))
  tsb <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$tsb))))
  catch <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$catch))))
  land <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$land))))  
  caytable<-round(do.call(rbind, lapply(simlist, function(xx)apply(xx$catchatage,1,collect)))) 
  tab <- cbind(fbar,rec,ssb,catch)
  if(splitLD){
    tab<-cbind(tab,fbarL,fbar-fbarL,land,catch-land)
  }
  if(addTSB){
    tab<-cbind(tab,tsb)
  }
  if(!missing(customWeights)) tab <- cbind(tab,cwF=round(do.call(rbind, lapply(simlist, function(xx)collect(xx$cwF))),3))
  rownames(tab) <- unlist(lapply(simlist, function(xx)xx$year))
  nam <- c("median","low","high")
  basename<-c("fbar:","rec:","ssb:","catch:")
  if(splitLD){
    basename<-c(basename,"fbarL:","fbarD:","Land:","Discard:")    
  }
  if(addTSB){
    basename<-c(basename,"tsb:")    
  }
  if(!missing(customWeights)){
    basename<-c(basename,"cwF:")    
  }
  colnames(tab)<-paste0(rep(basename, each=length(nam)), nam)
  attr(simlist, "tab")<-tab
  shorttab<-t(tab[,grep("median",colnames(tab))])
  rownames(shorttab)<-sub(":median","",paste0(label,if(!is.null(label))":",rownames(shorttab)))
  attr(simlist, "shorttab")<-shorttab
  attr(simlist, "label") <- label
  attr(simlist, "caytable")<-caytable    
  class(simlist) <- "samforecast"
  simlist
}

# ------------------------------------------------------------------------------------------------------------------------------

forecastx2 <- 
  function(fit, 
           fscale=NULL, 
           catchval=NULL, 
           catchval.exact=NULL, 
           fval=NULL, 
           nextssb=NULL, 
           landval=NULL, 
           cwF=NULL, 
           nosim=1000, 
           year.base=max(fit$data$years), 
           ave.years=max(fit$data$years)+(-4:0), 
           rec.years=max(fit$data$years)+(-9:0), 
           label=NULL, 
           overwriteSelYears=NULL, 
           deterministic=FALSE, 
           processNoiseF=TRUE, 
           customWeights=NULL, 
           customSel=NULL, 
           lagR=FALSE, 
           splitLD=FALSE, 
           addTSB=FALSE, 
           useSWmodel=(fit$conf$stockWeightModel==1), 
           useCWmodel=(fit$conf$catchWeightModel==1), 
           useMOmodel=(fit$conf$matureModel==1), 
           useNMmodel=(fit$conf$mortalityModel==1), 
           savesim=FALSE){
    
  resample <- function(x, ...){
    if(deterministic){
      ret <- mean(x)
    }else{
      ret <- x[sample.int(length(x), ...)]
    }
    return(ret)
  }
  ns<-max(length(fscale), length(catchval), length(catchval.exact), length(fval), length(nextssb), length(cwF))    
  if(missing(fscale)&missing(fval)&missing(catchval)&missing(catchval.exact)&missing(nextssb)&missing(cwF))stop("No scenario is specified")    
  if(missing(fscale)) fscale <- rep(NA,ns)
  if(missing(fval)) fval <- rep(NA,ns)
  if(missing(catchval)) catchval <- rep(NA,ns)
  if(missing(catchval.exact)) catchval.exact <- rep(NA,ns)    
  if(missing(nextssb)) nextssb <-rep(NA,ns)
  if(missing(landval)) landval <-rep(NA,ns)  
  if(missing(cwF)) cwF <-rep(NA,ns)  

  if(!all(rowSums(!is.na(cbind(fscale, catchval, catchval.exact, fval, nextssb, landval, cwF)))==1)){
    stop("For each forecast year exactly one of fscale, catchval or fval must be specified (all others must be set to NA)")
  }

  if(!is.null(overwriteSelYears)){
    fromto <- fit$conf$fbarRange-(fit$conf$minAge-1)  
    Ftab <- faytable(fit)
    fixedsel <- colMeans(Ftab[as.integer(rownames(Ftab))%in%overwriteSelYears,,drop=FALSE])
    fixedsel <- fixedsel/mean(fixedsel[fromto[1]:fromto[2]])
  }

  if(!is.null(customSel)){
    fromto <- fit$conf$fbarRange-(fit$conf$minAge-1)  
    customSel <- customSel/mean(customSel[fromto[1]:fromto[2]])
  }

  odat<-fit$obj$env$data
  opar<-fit$obj$env$parList(par = fit$obj$env$last.par.best)
  omap<-fit$obj$env$map
  omap[names(which(lapply(opar,length)==0))]<-NULL
  if(useSWmodel & (fit$conf$stockWeightModel==0)){
    stop("stockWeightModel cannot be used for forecast when it was not part of the fitted model")
  }
  if(useSWmodel){
    rnSW<-1:(nrow(opar$logSW)+ns)+as.integer(rownames(odat$stockMeanWeight)[1])-1  
    opar$logSW<-matrix(0,nrow=nrow(opar$logSW)+ns, ncol=ncol(opar$logSW))
    oran<-unique(names(fit$obj$env$par[fit$obj$env$random]))
    objsw <- MakeADFun(odat, opar, random = oran, DLL = "stockassessment", map=omap)
    sdrep<- sdreport(objsw, par.fixed=fit$opt$par, ignore.parm.uncertainty=TRUE)
    idx<-names(sdrep$value)=="logSW"
    simLogSw <-rmvnorm(nosim, sdrep$value[idx], sdrep$cov[idx,idx])
  }
  if(useCWmodel & (fit$conf$catchWeightModel==0)){
    stop("catchWeightModel cannot be used for forecast when it was not part of the fitted model")
  }
  if(useCWmodel){
    rnCW<-1:(nrow(opar$logCW)+ns)+as.integer(rownames(odat$catchMeanWeight)[1])-1      
    opar$logCW<-matrix(0,nrow=nrow(opar$logCW)+ns, ncol=ncol(opar$logCW))
    oran<-unique(names(fit$obj$env$par[fit$obj$env$random]))
    objcw <- MakeADFun(odat, opar, random = oran, DLL = "stockassessment", map=omap)
    sdrep<- sdreport(objcw, par.fixed=fit$opt$par, ignore.parm.uncertainty=TRUE)
    idx<-names(sdrep$value)=="logCW"
    simLogCW <-rmvnorm(nosim, sdrep$value[idx], sdrep$cov[idx,idx])
  }
  if(useNMmodel & (fit$conf$mortalityModel==0)){
    stop("mortalityModel cannot be used for forecast when it was not part of the fitted model")
  }
  if(useNMmodel){
    rnNM<-1:(nrow(opar$logNM)+ns)+as.integer(rownames(odat$natMor)[1])-1            
    opar$logNM<-matrix(0,nrow=nrow(opar$logNM)+ns, ncol=ncol(opar$logNM))
    oran<-unique(names(fit$obj$env$par[fit$obj$env$random]))
    objnm <- MakeADFun(odat, opar, random = oran, DLL = "stockassessment", map=omap)
    sdrep<- sdreport(objnm, par.fixed=fit$opt$par, ignore.parm.uncertainty=TRUE)
    idx<-names(sdrep$value)=="logNM"
    simLogNM <-rmvnorm(nosim, sdrep$value[idx], sdrep$cov[idx,idx])
  }
  if(useMOmodel & (fit$conf$matureModel==0)){
    stop("matureModel cannot be used for forecast when it was not part of the fitted model")
  }
  if(useMOmodel){
    rnMO<-1:(nrow(opar$logitMO)+ns)+as.integer(rownames(odat$propMat)[1])-1                  
    opar$logitMO<-matrix(0,nrow=nrow(opar$logitMO)+ns, ncol=ncol(opar$logitMO))
    oran<-unique(names(fit$obj$env$par[fit$obj$env$random]))
    objmo <- MakeADFun(odat, opar, random = oran, DLL = "stockassessment", map=omap)
    sdrep<- sdreport(objmo, par.fixed=fit$opt$par, ignore.parm.uncertainty=TRUE)
    idx<-names(sdrep$value)=="logitMO"
    simLogitMO <-rmvnorm(nosim, sdrep$value[idx], sdrep$cov[idx,idx])
  }
    
  getF <- function(x, allowSelOverwrite=FALSE){
    idx <- fit$conf$keyLogFsta[1,]+1
    nsize <- length(idx)
    ret <- exp(x[nsize+idx])
    ret[idx==0] <- 0
    if(allowSelOverwrite){
      if(!is.null(overwriteSelYears)){
        fromto <- fit$conf$fbarRange-(fit$conf$minAge-1)    
        thisfbar<-mean(ret[fromto[1]:fromto[2]])
        ret<-fixedsel*thisfbar
      }
      if(!is.null(customSel)){
        fromto <- fit$conf$fbarRange-(fit$conf$minAge-1)    
        thisfbar<-mean(ret[fromto[1]:fromto[2]])
        ret<-customSel*thisfbar
      }
    }
    ret
  }

  fbar<-function(x){
    fromto <- fit$conf$fbarRange-(fit$conf$minAge-1)  
    mean(getF(x)[fromto[1]:fromto[2]])
  }
    
  fbarFrac<-function(x, lf){
    fromto <- fit$conf$fbarRange-(fit$conf$minAge-1)  
    mean((lf*getF(x))[fromto[1]:fromto[2]])
  }    

  getCWF<-function(x,w){
    sum(getF(x)*w)
  }    
    
  getN <- function(x){
    idx <- fit$conf$keyLogFsta[1,]+1
    nsize <- length(idx)
    ret <- exp(x[1:nsize])
    ret
  }

  getState <- function(N,F){
    k <- fit$conf$keyLogFsta[1,]
    F <- F[k>=0]
    k <- unique(k[k>=0]+1) 
    x <- log(c(N,F[k]))
    x
  }    

  getProcessVar <- function(fit){
    cof <- coef(fit)
    sdN <- exp(fit$pl$logSdLogN[fit$conf$keyVarLogN+1])
    sdN[1]<-0
    nN <- length(sdN)
    sdF <- exp(cof[names(cof)=="logSdLogFsta"][fit$conf$keyVarF+1])
    if(processNoiseF==FALSE){
      sdF <- sdF*0
    }
    k<-fit$conf$keyLogFsta[1,]
    sdF <- sdF[k>=0]
    k <- unique(k[k >= 0] + 1)
    sdF <-sdF[k]
    nF <- length(sdF)
    if(fit$conf$corFlag==0){
      corr <- diag(nF)    
    }
    if(fit$conf$corFlag==1){
      y <- cof[names(cof)=="itrans_rho"]
      rho <- 2/(1+exp(-2*y))-1
      corr <- matrix(rho, nrow=nF, ncol=nF)
      diag(corr) <- 1
    }
    if(fit$conf$corFlag==2){
      y <- cof[names(cof)=="itrans_rho"]
      rho <- 2/(1+exp(-2*y))-1
      corr <- diag(sdF)
      corr <- rho^abs(row(corr)-col(corr))
    }
    cov <- matrix(0,nrow=nN+nF,ncol=nN+nF)
    cov[1:nN,1:nN] <- diag(sdN^2)
    
    if(fit$conf$corFlag <3){
      cov[nN+1:nF,nN+1:nF] <- (sdF%*%t(sdF))*corr
    }else{
      if(fit$conf$corFlag ==3){
        sdU <- exp(cof[names(cof)=="sepFlogSd"][1])
        sdV <- exp(cof[names(cof)=="sepFlogSd"][2])
        
        diag(cov[nN+1:(nF-1),nN+1:(nF-1)]) <- sdU^2
        cov[nN+nF,nN+nF] <- sdV^2
      }
    }
    cov
  }
    
  step <- function(x, nm, recpool, scale, inyear=FALSE){
    F <- getF(x, allowSelOverwrite=!inyear)
    N <- getN(x)
    if(!inyear){
      Z <- F+nm
      n <- length(N)
      N <- c(resample(recpool,1),N[-n]*exp(-Z[-n])+c(rep(0,n-2),N[n]*exp(-Z[n])))
    }
    F <- F*scale
    xx <- getState(N,F)
    return(xx)
  }
    
  scaleF <- function(x, scale){
    F <- getF(x)*scale
    N <- getN(x)
    xx <- getState(N,F)
    return(xx)
  }
    
  sel<-function(x){
    getF(x)/fbar(x)
  }

  catch <- function(x, nm, cw){
    F <- getF(x)
    Z <- F+nm
    N <- getN(x)
    C <- F/Z*(1-exp(-Z))*N
    return(sum(cw*C))
  }

  catchFrac <- function(x, nm, w, frac){
    F <- getF(x)
    Z <- F+nm
    N <- getN(x)
    C <- F/Z*(1-exp(-Z))*N
    return(sum(frac*w*C))
  }
    
  catchatage <- function(x, nm){
    F <- getF(x)
    Z <- F+nm
    N <- getN(x)
    C <- F/Z*(1-exp(-Z))*N
    return(C)
  }

  ssb <- function(x, nm, sw, mo, pm, pf){
    F <- getF(x)
    N <- getN(x)*exp(-pm*nm-pf*F)
    return(sum(N*mo*sw))
  }

  tsb <- function(x, sw){
    F <- getF(x)
    N <- getN(x)
    return(sum(N*sw))
  }

  rectab<-rectable(fit)
  recpool<-rectab[rownames(rectab)%in%rec.years,1]

  # Get final state
  if(year.base==max(fit$data$years)){
    est <- fit$sdrep$estY
    cov <- fit$sdrep$covY
  }
  if(year.base==(max(fit$data$years)-1)){  
    est <- fit$sdrep$estYm1
    cov <- fit$sdrep$covYm1
  }
  if(year.base<(max(fit$data$years)-1)){
    stop("State not saved, so cannot proceed from this year")
  }
  if(deterministic)cov<-cov*0
  sim<-rmvnorm(nosim, mu=est, Sigma=cov)

  if(is.null(overwriteSelYears) & is.null(customSel)){  
    if(!isTRUE(all.equal(est,getState(getN(est),getF(est)))))stop("Sorry somthing is wrong here (check code for getN, getF, and getState)")  
  }
    
  doAve <- function(x,y)colMeans(x[rownames(x)%in%ave.years,,drop=FALSE], na.rm=TRUE) 
  ave.sw <- doAve(fit$data$stockMeanWeight)
  ave.cw <- doAve(fit$data$catchMeanWeight)
  ave.mo <- doAve(fit$data$propMat)
  ave.nm <- doAve(fit$data$natMor)
  ave.lf <- doAve(fit$data$landFrac)
  ave.lw <- doAve(fit$data$landMeanWeight)
  ave.pm <- doAve(fit$data$propM)
  ave.pf <- doAve(fit$data$propF)
  getThisOrAve <- function(x,y, ave){
    if((y %in% rownames(x))&(!all(is.na(x[which(rownames(x)==y),])))){
      ret <- x[which(rownames(x)==y),]
    }else{
      ret <- ave
    }
    ret
  }
  procVar<-getProcessVar(fit)  
  simlist<-list()
  rs <- NULL  
  for(i in 0:(length(fscale)-1)){    
    y<-year.base+i  
    if(!is.null(rs)){
      assign(".Random.seed", rs, envir = .GlobalEnv)
      rs <- NULL    
    }
    sw<-getThisOrAve(fit$data$stockMeanWeight, y, ave.sw)
    cw<-getThisOrAve(fit$data$catchMeanWeight, y, ave.cw)
    mo<-getThisOrAve(fit$data$propMat, y, ave.mo)
    nm<-getThisOrAve(fit$data$natMor, y, ave.nm)
    lf<-getThisOrAve(fit$data$landFrac, y, ave.lf)    
    lw<-getThisOrAve(fit$data$landMeanWeight, y, ave.lw)
    pm<-getThisOrAve(fit$data$propM, y, ave.pm)
    pf<-getThisOrAve(fit$data$propF, y, ave.pf)
    if(useNMmodel){
      sim<-t(sapply(1:nrow(sim),
	             function(s){
                       yidx<-which(rnNM==as.integer(y))   
                       thisnm<-matrix(exp(simLogNM[s,]),nrow=nrow(opar$logNM))[yidx,]
                       step(sim[s,], nm=thisnm, recpool=recpool, scale=1, inyear=(i==0))
	             }
                    ))      
    }else{
      sim <- t(apply(sim, 1, function(s)step(s, nm=nm, recpool=recpool, scale=1, inyear=(i==0)))) 
    }
    if(i!=0){  
      cof <- coef(fit)
      if(deterministic){procVar<-procVar*0}
      if(!is.null(overwriteSelYears)){nn<-length(fit$conf$keyLogFsta[1,]); procVar[-c(1:nn),-c(1:nn)] <- 0}
      if(!is.null(customSel)){nn<-length(fit$conf$keyLogFsta[1,]); procVar[-c(1:nn),-c(1:nn)] <- 0}
      
      if(fit$conf$corFlag <3){
        sim <- sim + rmvnorm(nosim, mu=rep(0,nrow(procVar)), Sigma=procVar)
      }else if(fit$conf$corFlag ==3){
        k<-fit$conf$keyLogFsta[1,]
        k <- unique(k[k >= 0] + 1)
        nF <- length(k)
        simTmp <- rmvnorm(nosim, mu=rep(0,nrow(procVar)), Sigma=procVar)
        rhoF = 2/(1 + exp(-2*cof[names(cof)=="sepFlogitRho"])) -1
        sepFalpha = cof[names(cof)=="sepFalpha"]
        for(jj in 1:nosim){
          #Calculates previous U and V.
          V = mean(sim[jj,(dim(sim)[2]-nF+1):(dim(sim)[2])]) 
          U = sim[jj,(dim(sim)[2]-nF+1):(dim(sim)[2]-1)] -V   
          
          #Extract new contributions to U and V
          Uepsilon = simTmp[jj,(dim(simTmp)[2]-nF+1):(dim(simTmp)[2]-1)]
          Vepsilon = simTmp[jj,dim(simTmp)[2]]
          
          #Calculates updated U and V
          Unew = rhoF[1]*U + Uepsilon+ sepFalpha[1:(length(sepFalpha)-1)] 
          Vnew = rhoF[2]*V + Vepsilon+ sepFalpha[length(sepFalpha)]
          
          #Calculate F based on U and V
          sim[jj,(dim(simTmp)[2]-nF+1):(dim(simTmp)[2]-1)] = Unew + Vnew
          sim[jj,dim(simTmp)[2]] =  -sum(Unew) + Vnew #subtract sum(Unew) since U sum to 0 over all ages
        
          #Update process for N
          sim[jj,1:(dim(simTmp)[2]-nF)] = sim[jj,1:(dim(simTmp)[2]-nF)]   + simTmp[jj,1:(dim(simTmp)[2]-nF)] #Simulated N is not affected by the separable structure of F
        }
      }else{
        stop("forcast not implemented for the given corflag")
      }
    }
    
 
    if(!is.na(fscale[i+1])){
      sim<-t(apply(sim, 1, scaleF, scale=fscale[i+1]))    
    }

    if(!is.na(fval[i+1])){
      curfbar<-median(apply(sim, 1, fbar))
      adj<-fval[i+1]/curfbar
      sim<-t(apply(sim, 1, scaleF, scale=adj))    
    }

    if(!is.na(cwF[i+1])){
      if(missing(customWeights))stop("customWeights must be supplied when using the cwF option")  
      curcwF<-median(apply(sim, 1, getCWF, w=customWeights))
      adj<-cwF[i+1]/curcwF
      sim<-t(apply(sim, 1, scaleF, scale=adj))    
    }

    if(!is.na(catchval[i+1])){
      simtmp<-NA
      fun<-function(s){
        simtmp<<-t(apply(sim, 1, scaleF, scale=s))
        if(useCWmodel | useNMmodel){  
          simcat<-sapply(1:nrow(simtmp), function(k){
            if(useCWmodel){      
              thisy <- which(rnCW==y);
              thiscw <-matrix(exp(simLogCW[k,]),nrow=nrow(opar$logCW))[thisy,];
            }else{
              thiscw <- cw
            }  
            if(useNMmodel){      
              thisy <- which(rnNM==y);
              thisnm <-matrix(exp(simLogNM[k,]),nrow=nrow(opar$logNM))[thisy,];
            }else{
              thisnm <- nm
            }  
            catch(simtmp[k,], nm=thisnm, cw=thiscw)}
          )
        }else{ 
          simcat<-apply(simtmp, 1, catch, nm=nm, cw=cw)
        }
        return(catchval[i+1]-median(simcat))
      }
      ff <- uniroot(fun, c(0,100))$root
      sim <- simtmp
    }

    if(!is.na(catchval.exact[i+1])){
      simtmp<-sim
      funk<-function(k){
        if(useCWmodel){
          thisy <- which(rnCW==y)
          thiscw <-matrix(exp(simLogCW[k,]),nrow=nrow(opar$logCW))[thisy,]
        }else{
          thiscw=cw
        }
        if(useNMmodel){
          thisy <- which(rnNM==y)
          thisnm <-matrix(exp(simLogNM[k,]),nrow=nrow(opar$logNM))[thisy,]
        }else{
          thisnm=nm
        }
        one<-function(s){
          simtmp[k,]<<-scaleF(sim[k,],s)  
          simcat<-catch(simtmp[k,], nm=thisnm, cw=thiscw)
          return(catchval.exact[i+1]-simcat)
        }
        ff <- uniroot(one, c(0,100))$root
      }
      dd <- sapply(1:nrow(sim),funk)
      sim <- simtmp
    }
    
    if(!is.na(landval[i+1])){
      simtmp<-NA
      fun<-function(s){
        simtmp<<-t(apply(sim, 1, scaleF, scale=s))
        if(useNMmodel){
          simcat<-sapply(1:nrow(simtmp), function(k){      
              thisy <- which(rnNM==y);
              thisnm <-matrix(exp(simLogNM[k,]),nrow=nrow(opar$logNM))[thisy,];
              catchFrac(simtmp[k,], nm=thisnm, w=lw, frac=lf)
            }
          )
        }else{
          simcat<-apply(simtmp, 1, catchFrac, nm=nm, w=lw, frac=lf)
        }
        return(landval[i+1]-median(simcat))
      }
      ff <- uniroot(fun, c(0,100))$root
      sim <- simtmp
    }

    if(!is.na(nextssb[i+1])){
      if((sum(pm)!=0) | (sum(pf)!=0))stop("nextssb option only available if SSB is calculated in the beginning of the year")  
      simtmp<-NA
      rs<-.Random.seed
      fun<-function(s){
        assign(".Random.seed", rs, envir = .GlobalEnv)
        simtmp<<-t(apply(sim, 1, scaleF, scale=s))

        if(useNMmodel){
          simsim<-t(sapply(1:nrow(simtmp),
	                 function(s){
                           yidx<-which(rnNM==as.integer(y))   
                           thisnm<-matrix(exp(simLogNM[s,]),nrow=nrow(opar$logNM))[yidx,]
                           step(simtmp[s,], nm=thisnm, recpool=recpool, scale=1, inyear=(i==0))
  	                 }
                        ))      
        }else{
          simsim <- t(apply(simtmp, 1, function(s)step(s, nm=nm, recpool=recpool, scale=1, inyear=(i==0)))) 
        }
        if(i!=0){
          if(deterministic)procVar<-procVar*0  
          simsim <- simsim + rmvnorm(nosim, mu=rep(0,nrow(procVar)), Sigma=procVar)
        }
        if(useSWmodel | useMOmodel | useNMmodel){
          ssbswmoi<-function(k){
            if(useSWmodel){    
              thisy <- which(rnSW==y)
              thissw <-matrix(exp(simLogSw[k,]),nrow=nrow(opar$logSW))[thisy,]
            }else{
              thissw <-sw
            }
            if(useMOmodel){    
              thisy <- which(rnMO==y)
              thismo <-matrix(plogis(simLogitMO[k,]),nrow=nrow(opar$logitMO))[thisy,]
            }else{
              thismo <-mo
            }
            if(useNMmodel){    
              thisy <- which(rnNM==y)
              thisnm <-matrix(exp(simLogNM[k,]),nrow=nrow(opar$logNM))[thisy,]
            }else{
              thisnm <-nm
            }
            ssb(simsim[k,],nm=thisnm,sw=thissw,mo=thismo,pm=pm,pf=pf)
          }  
          simnextssb <- sapply(1:nrow(simsim), ssbswmoi)
        }else{
          simnextssb <- apply(simsim, 1, ssb, nm=nm, sw=sw, mo=mo, pm=pm, pf=pf)
        }
        return(nextssb[i+1]-median(simnextssb))
      }
      ff <- uniroot(fun, c(0,100))$root
      sim <- simtmp
    }
    
    fbarsim <- apply(sim, 1, fbar)
    fbarLsim <- apply(sim, 1, fbarFrac, lf=lf)
    if(useCWmodel | useNMmodel){
      catchcwi<-function(k){
        if(useCWmodel){      
          thisy <- which(rnCW==y)
          thiscw <-matrix(exp(simLogCW[k,]),nrow=nrow(opar$logCW))[thisy,]
        }else{
          thiscw <- cw
        }
        if(useNMmodel){      
          thisy <- which(rnNM==y)
          thisnm <-matrix(exp(simLogNM[k,]),nrow=nrow(opar$logNM))[thisy,]
        }else{
          thisnm <- nm
        }
        catch(sim[k,], nm=thisnm, cw=thiscw)
      }
      catchsim <- sapply(1:nrow(sim), catchcwi)
    }else{
      catchsim <- apply(sim, 1, catch, nm=nm, cw=cw)
    }

    if(useNMmodel){
      landsim<-sapply(1:nrow(sim), function(k){      
          thisy <- which(rnNM==y);
          thisnm <-matrix(exp(simLogNM[k,]),nrow=nrow(opar$logNM))[thisy,];
          catchFrac(sim[k,], nm=thisnm, w=lw, frac=lf)
        }
      )
    }else{
      landsim<-apply(sim, 1, catchFrac, nm=nm, w=lw, frac=lf)
    }
    catchatagesim <- apply(sim, 1, catchatage, nm=nm)
    if(useSWmodel | useMOmodel | useNMmodel){
      ssbswmoi<-function(k){
        if(useSWmodel){    
          thisy <- which(rnSW==y)
          thissw <-matrix(exp(simLogSw[k,]),nrow=nrow(opar$logSW))[thisy,]
        }else{
          thissw <-sw
        }
        if(useMOmodel){    
          thisy <- which(rnMO==y)
          thismo <-matrix(plogis(simLogitMO[k,]),nrow=nrow(opar$logitMO))[thisy,]
        }else{
          thismo <-mo
        }
        if(useNMmodel){    
          thisy <- which(rnNM==y)
          thisnm <-matrix(exp(simLogNM[k,]),nrow=nrow(opar$logNM))[thisy,]
        }else{
          thisnm <-nm
        }
        ssb(sim[k,],nm=thisnm,sw=thissw,mo=thismo,pm=pm,pf=pf)
      }  
      ssbsim <- sapply(1:nrow(sim), ssbswmoi)
    }else{
      ssbsim <- apply(sim, 1, ssb, nm=nm, sw=sw, mo=mo, pm=pm, pf=pf)
    }
    tsbsim <- apply(sim, 1, tsb, sw=sw)
    if(lagR){
      recsim <- exp(sim[,2])
    }else{
      recsim <- exp(sim[,1])
    }
    cwFsim <- rep(NA,nrow(sim))
    if(!missing(customWeights)){
      cwFsim <- apply(sim, 1, getCWF, w=customWeights)
    }
    simlist[[i+1]] <- list(sim=sim, fbar=fbarsim, catch=catchsim, ssb=ssbsim, rec=recsim,
                           cwF=cwFsim, catchatage=catchatagesim, land=landsim, fbarL=fbarLsim, tsb=tsbsim, year=y)
  }
    
  attr(simlist, "fit")<-fit

  collect <- function(x){
    quan <- quantile(x, c(.50,.025,.975))
    c(median=quan[1], low=quan[2], high=quan[3])
  }
  
  fbar <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$fbar))),3)
  fbarL <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$fbarL))),3)  
  rec <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$rec))))
  ssb <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$ssb))))
  tsb <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$tsb))))
  catch <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$catch))))
  land <- round(do.call(rbind, lapply(simlist, function(xx)collect(xx$land))))  
  caytable<-round(do.call(rbind, lapply(simlist, function(xx)apply(xx$catchatage,1,collect)))) 
  tab <- cbind(fbar,rec,ssb,catch)
  if(splitLD){
    tab<-cbind(tab,fbarL,fbar-fbarL,land,catch-land)
  }
  if(addTSB){
    tab<-cbind(tab,tsb)
  }
  if(!missing(customWeights)) tab <- cbind(tab,cwF=round(do.call(rbind, lapply(simlist, function(xx)collect(xx$cwF))),3))
  rownames(tab) <- unlist(lapply(simlist, function(xx)xx$year))
  nam <- c("median","low","high")
  basename<-c("fbar:","rec:","ssb:","catch:")
  if(splitLD){
    basename<-c(basename,"fbarL:","fbarD:","Land:","Discard:")    
  }
  if(addTSB){
    basename<-c(basename,"tsb:")    
  }
  if(!missing(customWeights)){
    basename<-c(basename,"cwF:")    
  }
  colnames(tab)<-paste0(rep(basename, each=length(nam)), nam)
  attr(simlist, "tab")<-tab
  shorttab<-t(tab[,grep("median",colnames(tab))])
  rownames(shorttab)<-sub(":median","",paste0(label,if(!is.null(label))":",rownames(shorttab)))
  attr(simlist, "shorttab")<-shorttab
  attr(simlist, "label") <- label
  attr(simlist, "caytable")<-caytable    
  class(simlist) <- "samforecast"
  if(!savesim){  
    simlistsmall<-lapply(simlist, function(x)list(year=x$year))
    attributes(simlistsmall)<-attributes(simlist)
    return(simlistsmall)
  }else{
    return(simlist)
  }
}
