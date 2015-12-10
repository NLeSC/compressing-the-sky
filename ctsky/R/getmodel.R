getmodel = function(data=c(),dependent=c(),independent=c(),group=c(),id=c(),
                    mcmc=FALSE,include_ext1=FALSE,minN=30,do.res=FALSE,CX2=0.95,do.log=TRUE,
                    imidcol="imageid",xtrsrc="xtrsrc",modeltime=TRUE) {

  # unique identifiers of the input variables
  dep = dependent; ind = independent; idcol = id; groupcol = group
  data = data[with(data,order(get(idcol),get(groupcol))),]
  # ommit all sources with 30 or less measurements with extracty_type=0
  Nband = sapply(split(data,data[,idcol]),function(df) {min(
    sapply(split(df,df[,groupcol]),function(df2){
      nrow(df2[which(df2$extract_type==0),]) # now only use sources with at least 30 extract type 1 values per band
    }))
  })
  data = data[which((data[,idcol] %in% unique(data[,idcol])[which(Nband > minN)]) == TRUE),]
  # data = data[with(data,order(get(idcol),get(groupcol))),]

  numberofbands = sapply(split(data,data[,idcol]),function(df) {
    length(unique(df[,groupcol]))
  })
  # rename and or reformat variables
  if (do.log == TRUE) {
    data[,ind] = log(data[,ind] * 10^-6) # frequency
  } else {
    data[,ind] = data[,ind] * 10^-6 # frequency
  }


  data[,groupcol] =as.integer(data[,groupcol]) # band
  negflux = which(data[,dep] < 0) # check whether there are negative values as they are undesirable for log space
  if (length(negflux > 0)) data[,dep][negflux] = 0.00000001
  if (do.log == TRUE) {
    data[,dep] =log(data[,dep]) # intensity
  }
  # initialize output variables
  K = data.frame()
  resid = c()
  #   if (is.na(as.integer(unique(data[1,id]))) == 0) {
  #     uid = as.integer(unique(data[,id])) # unique source identifies
  #   } else {
  #     uid = as.character(unique(data[,id])) # unique source identifies
  #   }
  uid = unique(data[,id]) # unique source identifies
  for (i in 1:length(uid)) { # derive model per source
    if (i/30 == round(i/30)) cat(paste0(" ",round(100*(i/length(uid))),"%"))
    dat = data[which(data[,id] == uid[i]),]
    extp0 = which(dat$extract_type==0)
    extp1 = which(dat$extract_type==1)
    # fit the model:
    if (numberofbands[i] > 1) {
      if (include_ext1 == TRUE & mcmc == FALSE) {
        fit = lm(get(dep)~get(ind),data=dat,na.action=na.omit)
      } else if (include_ext1 == FALSE & mcmc == FALSE) { #assuming that there are at least 30 data points with extract type 0
        fit = lm(get(dep)~get(ind),data=dat[extp0,],na.action=na.omit)
      }  else if (include_ext1 == TRUE & mcmc == TRUE) {
        posterior = MCMCpack::MCMCregress(get(dep)~get(ind),b0=-0.7, B0 = 0.1,
                                          b0=0,B0=0.1, data=dat, verbose=FALSE) #sigma.mu = 5, sigma.var = 25
      } else if (include_ext1 == FALSE & mcmc == TRUE) {
        posterior = MCMCpack::MCMCregress(get(dep)[extp0]~get(ind)[extp0],b0=-0.7, B0 = 0.1,
                                          b0=0,B0=0.1,  data=dat, verbose=FALSE) #  sigma.mu = 5, sigma.var = 25,
      }
    } else { # fit model if there is only one frequency band using lm (no mcmc here)
      if (include_ext1 == TRUE) {
        fit = lm(get(dep)~ 1 + offset(-0.7*get(ind)),data=dat,na.action=na.omit) #force slope at -0.7
      } else {
        fit = lm(get(dep)~ 1 + offset(-0.7*get(ind)),data=dat[extp0,],na.action=na.omit) #force slope at -0.7
      }
    }
    if (numberofbands[i] > 1) {
      if (mcmc == FALSE) {
        if (nrow(coef(summary(fit))) == 2) {
          y0 = as.numeric(coef(fit)[1])
          alpha = as.numeric(coef(fit)[2])
          y0_se = summary(fit)$coefficients[1,2]
          alpha_se = summary(fit)$coefficients[2,2]
        } else {
          print("fit did not work, check that there is extract_type =0 in the measurements")
        }
      } else {
        y0 = mean(posterior[,1])
        alpha = mean(posterior[,2])
        y0_se = sd(posterior[,1])
        alpha_se = sd(posterior[,2])
      }
    } else {
      y0 = as.numeric(coef(fit)[1])
      alpha = -0.7
      y0_se = summary(fit)$coefficients[1,2]
      alpha_se = NA
    }
    #------------------------------------------------------
    freqmeanlist = sdlist = mlist = c() #= padflist = slopelist
    simulation = c()
    # create dataframe with unique links between frequency values and band number
    uband = as.integer(as.character(sort(unique(dat[,groupcol]))))
    linkm = data.frame(band=rep(0,length(uband)),cluster=rep(0,length(uband))) #,freqi=rep(0,length(uband))
    cnt = 1
    for (ubandi in unique(uband)) {
      linkm[cnt,1] = ubandi
      linkm[cnt,2] = mean(dat[,ind][which(dat[,groupcol]==ubandi)])
      cnt = cnt + 1
    }

    for (bandi in 1:length(uband)) {
      freqmeanlist = c(freqmeanlist,linkm[which(as.integer(linkm[,1]) == uband[bandi]),2] )
      if (include_ext1 == TRUE) {
        sel = which(dat[,groupcol] == uband[bandi])
      } else {
        sel = which(dat[,groupcol][extp0] == uband[bandi])
      }
      if (do.log == TRUE) {
        estimate = as.numeric(exp((alpha * (linkm[which(linkm[,1] == uband[bandi]),2])) + y0))
      } else {
        estimate = as.numeric((alpha * (linkm[which(linkm[,1] == uband[bandi]),2])) + y0)
      }
      if (length(estimate) == 0) {
        stop("inspect problem in getmodel.R: ",linkm)
      }
      if (include_ext1 == TRUE) {
        if (do.log == TRUE) {
          sdlist = c(sdlist,sd(exp(dat[,dep][sel])))
          mlist = c(mlist,mean(exp(dat[,dep][sel]))-estimate)
        } else {
          sdlist = c(sdlist,sd(dat[,dep][sel]))
          mlist = c(mlist,mean(dat[,dep][sel])-estimate)
        }
      } else {
        if (do.log == TRUE) {
          sdlist = c(sdlist,sd(exp(dat[,dep][extp0[sel]])))
          mlist = c(mlist,mean(exp(dat[,dep][extp0[sel]]))) #-estimate)
        } else {
          sdlist = c(sdlist,sd(dat[,dep][extp0[sel]]))
          mlist = c(mlist,mean(dat[,dep][extp0[sel]])) #-estimate)
        }
      }
      if(is.na(mlist[bandi]) == TRUE) stop("mlist error: ",length(sel),length(extp0[sel]))
      # get dat subset
      if (include_ext1 == TRUE) {
        da = dat[,dep][sel]
      } else {
        da = dat[,dep][extp0[sel]]
      }
      #------------------------------------------
      # pearson.test approach (based on dat that is still in the log space)
      if (do.log == TRUE) {
        PT = nortest::pearson.test(exp(da)) #pearson chi-square normality test outside log-space
        KT = moments::kurtosis(exp(da))
      } else {
        PT = nortest::pearson.test(da) #pearson chi-square normality test outside log-space
        KT = moments::kurtosis(da)
      }
      X2.pear = PT$statistic
      df.pear = PT$df
      X2.pear.red = X2.pear/df.pear
      critX2.pear1 = qchisq(CX2,df.pear)
      K = rbind(K,data.frame(runcat=uid[i],band=as.integer(uband[bandi]),freqmean=freqmeanlist[bandi],
                             N=length(da),Na=nrow(dat),
                             sd_band=sdlist[bandi],
                             X2=X2.pear,X2_crit=critX2.pear1,m_band=mlist[bandi],
                             df_band=df.pear,sd_source=0,df_source=0,alpha=0,y0=0,alpha_se=0, #cortestr=cortest$estimate
                             y0_se=0,X2_source=0,X2_crit_source=0,kurtosis=KT)) #,pslope=slopelist[bandi],padf=padflist[bandi]
      simulation = c(simulation,rnorm(n=1000,mean=mlist[bandi],sd=sdlist[bandi])) #
    }


    #-------------------------------------------
    # goodness of fit of the whole model based on simulated data
    PT = nortest::pearson.test(simulation) #pearson chi-square normality test outside log-space
    X2.pear = PT$statistic
    df.pear = PT$df
    criticalX2.pear = qchisq(CX2,df.pear)
    #put it all in dataframe
    K$alpha[which(K$runcat==uid[i])] = alpha
    K$alpha_se[which(K$runcat==uid[i])] = alpha_se
    K$y0[which(K$runcat==uid[i])] = y0
    K$y0_se[which(K$runcat==uid[i])] = y0_se
    K$X2_source[which(K$runcat==uid[i])] = X2.pear
    K$X2_crit_source[which(K$runcat==uid[i])] = criticalX2.pear
    K$df_source[which(K$runcat==uid[i])] = df.pear
    K$sd_source[which(K$runcat==uid[i])] = sd(simulation)
    if (do.res == TRUE) {      # use all data here, because now you create residuals
      if (do.log == TRUE) {
        if (modeltime == TRUE) {
          estimate = dat[,groupcol]
          uk = unique(estimate)
          for (k in uk) {
            estimate[which(estimate == k)] = K$m_band[which(K$band == k & K$runcat == uid[i])[1]]
          }
          res = estimate - exp(dat[,dep])
        } else {
          res = exp(dat[,ind] * alpha + y0) - exp(dat[,dep]) # residual relative to linear model
        }
      } else { # do.log = FALSE
        if (modeltime == TRUE) {
          estimate = dat[,groupcol]
          uk = unique(estimate)
          for (k in uk) {
            estimate[which(estimate == k)] = K$m_band[which(K$band == k & K$runcat == uid[i])[1]]
          }
          res = estimate - dat[,dep]
#
#
#           tmp = dat
#           tmp$estimate = estimate
#           print(names(tmp))
#           tmp = tmp[order(dep),]
#           x11()
#           plot(tmp[,dep],type="l",ylab="Intensity (Jy)",xlab="index (no unit)")
#           lines(tmp$estimate,col="red",type="l")
        } else {
          res = (dat[,ind] * alpha + y0) - dat[,dep] # residual relative to linear model
        }
      }
      resi = data.frame(rcid=dat[,idcol],rcxstrc=dat[,xtrsrc],
                        imageid=dat[,imidcol],res = res)
      resid = rbind(resid,resi)
    }
    rm(dat,fit)
  }


  names(K)[which(names(K)=="runcat")] = idcol
  names(K)[which(names(K)=="band")] = groupcol
  # K[,idcol] = as.integer(K[,idcol])
  if (do.res==TRUE) {
    invisible(list(resid=resid,models=K))
  } else {
    invisible(list(models=K))
  }
}
