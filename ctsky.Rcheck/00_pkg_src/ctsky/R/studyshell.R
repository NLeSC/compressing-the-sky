studyshell = function(data=c(), resultpath= c(),CX2=0.95,minN = 30,
                      dependent="f_int",independent="freq_eff",group="band",id="runcat",timecol="taustart_ts",
                      mcmc=FALSE,include_ext1=FALSE,do.res=FALSE,do.log=TRUE,
                      imidcol="imageid",xtrsrc="xtrsrc",error.low=c(),error.up=c(),prec=c(),modeltime=TRUE,do.bar=TRUE,
                      deplabel="Intensity (Jy)"){

  if (length(resultpath) == 0) stop("Error: argument resultpath not specified")
  if (dir.exists(resultpath) == FALSE) dir.create(resultpath)

  dep = dependent; ind = independent; idcol = id; groupcol = group; timecol = timecol
  if (length(error.up) == 0) error.up = error.low
  if (length(prec) == 0) prec = ceiling(abs(log(sd(data[,dep]),base=10))) +1 # prec number of decimals we are interested in

  xlabel = "Frequency (MHz)"
  ylabel = "Intensity (Jy)"
  stim = data.frame()
  #===================================
  # fit models and derive residuals
  cat("\nFit models and test model assumptions...")
  V = getmodel(data=data,dependent=dependent,independent=independent,group=group,id=id,
               mcmc=mcmc,include_ext1=include_ext1,minN=minN,do.res=do.res,CX2=CX2,do.log=do.log,
               imidcol=imidcol,xtrsrc=xtrsrc,modeltime=modeltime)

  if (do.res == TRUE) {
    resid = V$resid # including residuals for extract type 0 and 1
    models = V$models
  } else {
    models = V
  }
  models = models[order(models[,idcol],models[,groupcol]),]
  if (do.log==TRUE) {
    dl = "xy"
    dlx = "x"
  } else {
    dl = ""
    dlx = ""
  }
  cat("\nCreate visualisations...")
  m2 = models[,c(idcol,groupcol,"alpha","y0")]
  mydata2 = merge(m2,data,by=c(idcol,groupcol))
  mydata2 = mydata2[order(mydata2[,idcol],mydata2[,groupcol]),]
  getvis(data=mydata2,resultpath=resultpath,dependent=dep,independent=ind,
         group=groupcol,id=idcol,timecol=timecol,do.log=do.log,do.bar=do.bar,error.low=error.low,error.up=error.up,deplabel=deplabel)
  # clean up data, just like inside getmodel
  # ommit sources with less then minN measurements per band
  Nband = sapply(split(data,data[,idcol]),function(df) {min(
    sapply(split(df,df[,groupcol]),function(df2){
      nrow(df2[which(df2$extract_type==0),]) # now only use sources with at least 30 extract type 0 values per band
    }))
  })
  data = data[which((data[,idcol] %in% unique(data[,idcol])[which(Nband > minN)]) == TRUE),]

  #===================================================
  # Edit data format continuing, this also happens in getmodel()
  # For application with embedded MonetDB it is not logical to change variables, but here it is.
  negflux = which(data[,dep] < 0) # check whether there are negative values as they are undesirable for log space
  if (length(negflux > 0)) data[,dep][negflux] = 0.00000001 # QUESTION FOR BART: DO WE KEEP THESE VALUES?
  #     if (do.log == TRUE) { # seem redundant... delete?
  #       data[,dep] = log(data[,dep])
  #     }
  data$f_int_original = data[,dep]
  pr = 10^prec
  data$integer = as.integer(data[,dep]*pr) # note that these are not in log space but in Jx
  data$f_int_delta = c(data$integer[[1]],diff(data$integer))
  data$f_delta_int = as.integer(c(data[,dep][[1]],diff(data[,dep]))*pr)

  # cat("\n")
  cat("\nSummary characteristics of intensity values (Jy)\n")
  print(summary(data[,dep]))
  cat("\n")

  if (do.log) {
    data[,ind] = log(data[,ind] * 10^-6) # convert to MHz
  } else {
    data[,ind] = data[,ind] * 10^-6 # convert to MHz
  }
  data$time.num = as.numeric(as.POSIXlt(data[,timecol])) # convert to numeric timestamps
  data[,groupcol] = as.factor(data[,groupcol])
  data$time = as.numeric(as.POSIXlt(data[,timecol]))
  if (do.res == TRUE) {
    #======================================
    # evaluate lossless compression
    #==================================

    cat("Evaluate lossless compression...\n")
    cat(paste0("or the compression test values will be rounded to ",prec," decimal places\n"))
    A = data
    A$res = resid$res #cbind(data,resid$res)
    B = merge(x=A,y=models[which(models[,groupcol] == unique(models[,groupcol])[1]),c("alpha","y0","m_band",idcol)],by.x=idcol,by.y=idcol) #[which(models[,groupcol] == 23),]
    if (modeltime == FALSE) {     # approach one use 1 model per source:
      if (do.log == TRUE) {
        B$C = exp((B$alpha * B[,ind])+B$y0) - B$res
      } else {
        B$C = ((B$alpha * B[,ind])+B$y0) - B$res
      }
#       x11()
#       par(mfrow=c(2,2))
#       plot(B$C,type="l",main="replicated")
#       plot(B$res,type="l",main="residual")
#       plot(exp((B$alpha * B[,ind])+B$y0),type="l",main="model")
#       plot(A$f_int_original,type="l",main="original")
    } else {      # approach two use 1 model per band:
      uid = unique(B[,id])
      estimate = rep(0,nrow(B))
      for (j in uid) { # unique ids (sources)
        uk = unique(B[which(B[,id] == j),groupcol]) # unique bands
        for (k in uk) {
          estimate[which(B[,groupcol] == k & as.character(B[,id]) == j)] = models$m_band[which(models[,groupcol] == k & models[,id]== j)[1]]
        }
      }
      B$C = estimate - B$res # C should now be the same as the original values
    }
    print(summary(B$C - A$f_int_original))
    # cat("\n")
    rm(A,B)

    #===================================
    # now test compression for sources with at least minN values per band
    #=====================================
    # Experimental code for excluding tails (now commented out)
    # now exclude the two 2.5% tails in the residual distribution.do this here because it will not be part of getmodel.R
    #     tails = quantile(resid$res,probs=c(0.01,0.99))
    #     keepi = which(resid$res > tails[1] & resid$res < tails[2])
    keepi = c(1:nrow(data))

    #     print("Summary characteristics of residuals (Jy)")
    #     print(summary(resid$res))
    #     cat("\n")

    resid$res_int = as.integer(resid$res*pr) # note that these are not in log space but in Jx (residuals cannot be expressed in log space)
    resid$res_int_delta = c(resid$res_int[[1]],diff(resid$res_int))
    resid$res_delta_int = as.integer(c(resid$res[[1]],diff(resid$res))*pr)
#     x11()
#     par(mfrow=c(2,2))
#     plot(resid$res,type="l",main=1)
#     plot(resid$res*pr,type="l",main=2)
#     plot(data$f_int_original,type="l",main=3)
#     plot(data$integer,type="l",main=4)

    result = rbind(unlist(testcompr(data$f_int_original[keepi],path=resultpath)), # original data
                   unlist(testcompr(data$integer[keepi],path=resultpath)), # original data as integer
                   unlist(testcompr(data$f_int_delta[keepi],path=resultpath)), # original data as integer and then delta
                   unlist(testcompr(data$f_delta_int[keepi],path=resultpath)), # original data delta as integer
                   unlist(testcompr(resid$res[keepi],path=resultpath)), # residuals
                   unlist(testcompr(resid$res_int[keepi],path=resultpath)),
                   unlist(testcompr(resid$res_int_delta[keepi],path=resultpath)),
                   unlist(testcompr(resid$res_delta_int[keepi],path=resultpath)))
    result = result[,-1]
    result = rbind(result,format(as.matrix(result[5:8,]) / as.matrix(result[1:4,]),digits=3)) # result[1:4,],digits=3))
    result = as.data.frame(result)
    row.names(result) = c("original","original integer","original integer delta","original delta integer",
                          "residual","residual integer","residual integer delta","residual delta integer",
                          "ratio original","ratio integer","ratio integer delta","ratio delta integer")
    # store models
    write.csv(result,paste0(resultpath,"compression_test.csv"))
    S = read.csv(paste0(resultpath,"/compression_test.csv"))
    plotnames = c("integer","integer delta","delta integer")
    # plot
    pdf(file=paste0(resultpath,"/compression.pdf"),width = 9,height = 9) #
    par(mar=c(4,5,5,2),mfrow=c(3,1))
    for (i5 in 1:3) {
      i7 = (as.matrix(S[c((1+i5),(5+i5)),3:ncol(S)])/S[8,2])*100
      # print(max(i7))
      yup = max(i7)*1.8
      # if (yup > 110) yup = 100
      barplot(height=i7,names.arg=colnames(S)[3:ncol(S)],ylim=c(0,yup),
              beside=TRUE,ylab="compression (% of uncompressed)",
              xlab="compression technique",
              legend.text = c("original data","model residuals"),main=plotnames[i5], #Impact on lossless compression
              cex.axis=1.1,cex.names=1.2,cex.lab=1.2,cex=1.1,cex.main=2,args.legend = c(cex=1.0))
    }
    dev.off()
  }
  #=================================
  # identify poor fits and label them
  models$goodfitc = models$goodfitb = models$goodfit = TRUE
  C1 = which(models$X2 > models$X2_crit  | models$X2_source > models$X2_crit_source) #& models$kurtosis < 10) # & models$kurtosis < 10
  C2 = which(models$X2_source > models$X2_crit_source) #& models$kurtosis < 10
  C3 = which(models$X2 > models$X2_crit) #& models$kurtosis < 10
  models$goodfit[C1] = FALSE
  models$goodfitb[C2] = FALSE
  models$goodfitc[C3] = FALSE
  Ndigits = 3
  # calculate fraction of poor fits
  tmp1 = round(sum(models$N[which(models$goodfit==FALSE)]) / sum(models$N),digits=Ndigits) # out of total measuremns
  tmp5 = round(length(which(models$goodfit==FALSE)) / nrow(models),digits=Ndigits) # output of total bands
  tmp6 = round(length(unique(models[,idcol][which(models$goodfit==FALSE)])) / length(unique(models[,idcol])),digits=Ndigits) # out of total sources
  tmp2 = round(sum(models$N[which(models$goodfitb==FALSE)]) / sum(models$N),digits=Ndigits)
  tmp7 = round(length(which(models$goodfitb==FALSE)) / nrow(models),digits=Ndigits)
  tmp8 = round(length(unique(models[,idcol][which(models$goodfitb==FALSE)])) / length(unique(models[,idcol])),digits=Ndigits)
  tmp3 = round(sum(models$N[which(models$goodfitc==FALSE)]) / sum(models$N),digits=Ndigits)
  tmp9 = round(length(which(models$goodfitc==FALSE)) / nrow(models),digits=Ndigits)
  tmp10 = round(length(unique(models[,idcol][which(models$goodfitc==FALSE)])) / length(unique(models[,idcol])),digits=Ndigits)
  stim = rbind(stim,data.frame(X2.conf=CX2,
                               band.assump.ban=tmp9,source.assump.ban=tmp7,both.assump.ban=tmp5,
                               band.assump.dat=tmp3,source.assump.dat=tmp2,both.assump.dat=tmp1,
                               band.assump.src=tmp10,source.assump.src=tmp8,both.assump.src=tmp6))
  #=============================================
  # replicate the original data based on statisitcal properties from models without using residuals or the original data
  #==============================================
  cat("\nReplicate data...")
  rep.data = approxdata(models, dependent=dep,independent=ind,group=groupcol,id=idcol,do.log=do.log,modeltime=modeltime) #,logspacein=TRUE
  rep.data = rep.data[order(rep.data[,idcol],rep.data[,groupcol]),]

  # lines(comp$original,type="l",col="red")
  # print(models)
  # print(summary(rep.data))

  # now remove extract_type 1
  exclude = which(data$extract_type==1)
  if (length(exclude) > 0) {
    data3 = data[-exclude,]
  } else {
    data3 = data
  }
  data3 = data3[order(data3[,idcol],data3[,groupcol]),]
  idx = which(data3[,idcol] %in% rep.data[,idcol] == TRUE & data3[,groupcol] %in% rep.data[,groupcol] == TRUE)
  comp = rep.data
  comp$original = data3$f_int_original[idx] # original values outside log space
  comp$original.se = data3[,error.low][idx] # standard error of original values outside log space
  if (do.log == TRUE) {
    comp[,ind] = exp(data3[,ind][idx])
  } else {
    comp[,ind] = data3[,ind][idx]
  }
  comp$extract_type = data3$extract_type[idx]
  comp$version = data3$version[idx]
  comp = comp[order(comp[,idcol],comp[,groupcol]),]
  # note that diff mean and sd are stored in statcompr.R based on extracttype0
  # the model was developed on all the data excluding extracttype0
  # therefore if we now evaluate this we should only do it for extracttype0

  #=====================================
  # investigate relationship between approximated data and original data
  # require("nortest")
  cat("\nInvestigate approximation....")
  study  = data.frame(runcat=sapply(split(comp,f=list(comp[,idcol],comp[,groupcol])),
                                    function(df){out = df[,idcol][1]}),
                      band=sapply(split(comp,f=list(comp[,idcol],comp[,groupcol])),
                                  function(df){out = df[,groupcol][1]}),
                      approx.flux=sapply(split(comp,f=list(comp[,idcol],comp[,groupcol])),
                                         function(df){out = mean(df$approx.flux)}),
                      mean.error=sapply(split(comp,f=list(comp[,idcol],comp[,groupcol])),
                                        function(df){out = mean(df$approx.flux)-mean(df$original)}),
                      mean.original=sapply(split(comp,f=list(comp[,idcol],comp[,groupcol])),
                                           function(df){out = mean(df$original)}),
                      mean.ratio=sapply(split(comp,f=list(comp[,idcol],comp[,groupcol])),
                                        function(df){out = mean(df$approx.flux)/mean(df$original)}),
                      sd.error=sapply(split(comp,f=list(comp[,idcol],comp[,groupcol])),
                                      function(df){out = sd(df$approx.flux)-sd(df$original)}),
                      sd.ratio=sapply(split(comp,f=list(comp[,idcol],comp[,groupcol])),
                                      function(df){out = sd(df$approx.flux)/sd(df$original)}),
                      N=sapply(split(comp,f=list(comp[,idcol],comp[,groupcol])),
                               function(df){out = nrow(df)}),
                      meanflux=sapply(split(comp,f=list(comp[,idcol],comp[,groupcol])),
                                      function(df){out = mean(df$original)}),
                      sdflux=sapply(split(comp,f=list(comp[,idcol],comp[,groupcol])),
                                    function(df){out = sd(df$original)}),
                      original.se=sapply(split(comp,f=list(comp[,idcol],comp[,groupcol])),
                                         function(df){ # out = mean(df$original.se)
                                           ezi = df$original.se
                                           wxi = 1/(ezi^2)
                                           out = sqrt(1/sum(wxi) ) # standard deviation
                                         }),
                      X2=sapply(split(comp,f=list(comp[,idcol],comp[,groupcol])),
                                function(df){out = mean(nortest::pearson.test(df$original)$statistic)}))



  # goodfit = models$goodfit)
  # the above action assumes that all sources have the same number of bands. if this is not the case then
  # dataframe models will not cbind well with dataframe study. therefore, we merge them seperatedly
  names(study)[1] = idcol
  names(study)[2] = groupcol
  study = merge(study,models[,c(idcol,groupcol,"goodfit")],by=c(idcol,groupcol))

  study = study[order(study[,idcol],study[,groupcol]),]
  pdf(file= paste0(resultpath,"reproduction_with model_noresiduals_perband.pdf"),width = 9,height = 9)
  par(mfrow=c(2,2),mar=c(5,6,4,2)+0.1,lwd=2)
  CX = 0.6; CL = 1.3; CA = 1.1
  yxx = range(c(-0.1,0.1,study$mean.error,study$sd.error))
  yxxb = range(c(0.98,1.02,study$mean.ratio,study$sd.ratio))
  plot(study$mean.original,study$mean.error,
       xlab="Intensity (Jy)",ylab="difference in means (Jy)",main="mean",
       type="p",pch=20,cex=CX,cex.lab=CL,cex.axis=CA,col="black",bty="l",ylim=yxx,log=dlx)
  abline(h=0,lty=3)
  plot(study$mean.original,study$sd.error,
       xlab="Intensity (Jy)",ylab="difference in std dev (Jy)",main="stdev",
       type="p",pch=20,cex=CX,cex.lab=CL,cex.axis=CA,bty="l",ylim=yxx,col="black",log=dlx)
  abline(h=0,lty=3)
  plot(study$mean.original,study$mean.ratio,
       xlab="Intensity (Jy)",ylab="approximated / original mean",main="mean",
       type="p",pch=20,cex=CX,cex.lab=CL,cex.axis=CA,bty="l",ylim=yxxb,col="black",log=dlx)
  abline(h=1,lty=3)
  plot(study$mean.original,study$sd.ratio,
       xlab="Intensity (Jy)",ylab="approximated / original stdev",main="stdev",
       type="p",pch=20,cex=CX,cex.lab=CL,cex.axis=CA,bty="l",ylim=yxxb,col="black",log=dlx)
  abline(h=1,lty=3)
  dev.off()
  #---------------------------------------------
  # now create plot of the sources that were considered poor fit:
  # idpoor = unique(models[,idcol][which(models$goodfit == FALSE)])
  idpoor = unique(models[,idcol][which(models$X2_source > models$X2_crit_source )]) # non linear #& models$kurtosis < 10
  Z = comp[which(comp[,idcol] %in% idpoor == TRUE),]
  V1 = which(Z$version == 1)
  V2 = which(Z$version == 2)
  pdfname = paste0(resultpath,"sources with poor fit.pdf")
  if (file.exists(pdfname)) file.remove(pdfname)
  if (length(idpoor) > 0) {
    pdf(file= pdfname,width = 9,height = 7)
    par(mfrow=c(2,3))
    for (j in 1:length(idpoor)) {
      if (length(V1) > 0 & length(V2) > 0) {
        g = which(Z[,idcol] == idpoor[j] & Z$version == 1)
        plot(Z[,ind][g],Z$original[g],log=dl,pch=20,type="p",cex=0.5,cex.lab=CL,cex.axis=CA,
             main=idpoor[j],xlab=xlabel,ylab=ylabel,ylim=range(Z$original[which(Z[,idcol] == idpoor[j])]))
        g = which(Z[,idcol] == idpoor[j] & Z$version == 2)
        lines(Z[,ind][g],Z$original[g],pch=20,type="p",cex=0.5,col="blue")
      }  else {
        g = which(Z[,idcol] == idpoor[j])
        plot(Z[,ind][g],Z$original[g],log=dl,pch=20,type="p",cex=0.5,cex.lab=CL,cex.axis=CA,
             main=idpoor[j],xlab=xlabel,ylab=ylabel)
      }
    }
    dev.off()
  }
  studyshell = V
}
