approxdata = function(models=c(),do.log=TRUE,
                      dependent="f_int",independent="freq_eff",group="band",id="runcat",modeltime=TRUE) { #,logspacein=FALSE
  # if yes then it needs to be converted back in order to add random numbers for the Gaussian distribution which happens outside the logspace?
  #------------------------------------------------------
  dep = dependent; ind = independent; idcol = id; groupcol = group
  sid = unique(models[,idcol])
  idm = which(models[,idcol]  %in% sid == TRUE) # indeces of interest in model table
  Npoints = sum(as.numeric(models$N))
  repi = data.frame(approx.flux = rep(0,Npoints), # replicate length of estimates (intensity)
                    band = rep(models[,groupcol],times=models$N), # replicate band
                    runcat = rep(models[,idcol],times=models$N))
  names(repi)[c(2,3)] = c(groupcol,idcol)
  for (j in 1:length(sid)) { # loop through sources
    h = which(models[,idcol] == sid[j])[1]
    alpha = models$alpha[h] # one alpha per source
    y0 = models$y0[h] # one y-intercept per source
    band2look = unique(repi[,groupcol])
    for (bb in band2look) { # loop through bands
      iband = which(models[,idcol] == sid[j] & models[,groupcol] == bb)
      if (length(iband) > 0) {
        sdband = models$sd_band[iband]
        mband = models$m_band[iband]
        if (length(mband) == 0) stop("error1...",iband,mband)
        if (is.na(mband) == TRUE) stop("error2...",mband,iband)
        N = models$N[which(models[,idcol] == sid[j] & models[,groupcol] == bb)]
        freqmean = models$freqmean[which(models[,idcol] == sid[j] & models[,groupcol] == bb)]
        FREQ = rep(freqmean,N)
        set.seed(1)
#         if (modeltime == FALSE & models$goodfit[iband] == TRUE) { # do not aid the model when the fit is good
#           mband = 0
#           sdband = models$sd_source[iband] # use standard deviation of model residuals instead of per band
#         }
        #mband
        randomnumber = rnorm(n=N,mean=0,sd=sdband) # random number in log space to mimic variation
        # try to improve statistical properties of random data, because rnorm is not very good at it
        rn2 = scale(randomnumber,center=FALSE,scale=sd(randomnumber)/sdband)
        MM = mean(rn2)
        SM = sd(rn2)
        #-mband
        randomnumber = scale(rn2,center=MM,scale=SM/sdband)
        # print(".")
        # print(mband)
        # if (max(abs(randomnumber)) > 10) print(max(abs(randomnumber)))
        # if (abs((mean(randomnumber)) > 0.5))    print(mean(randomnumber))
        index = which(repi[,idcol] == sid[j] & repi[,groupcol] == bb)
        if (modeltime == FALSE) {     # approach one use 1 model per source:
          if (do.log == TRUE) {
            repi$approx.flux[index] = exp((alpha * FREQ) + y0) + randomnumber
          } else {
            repi$approx.flux[index] = (alpha * FREQ) + y0 + randomnumber
          }
        } else {
          # if (do.log == TRUE) {
            # repi$approx.flux[index] = exp(mband) + randomnumber
          # } else {
            repi$approx.flux[index] = mband + randomnumber # no exp because mband is already in Jy
          # }
        }
#         if(max(repi$approx.flux[index]) > 4000) {
#           x11()
#           par(mfrow=c(2,2))
#           plot(repi$approx.flux[index],type="l",main="approx flux")
#           plot(randomnumber,type="l",main="randomnumber")
#           plot(mband,type="l",main="mband")
#           plot(exp(mband),type="l",main="exp mband")
#         }
      }
    }
  }
  rep.from.model = repi
}
