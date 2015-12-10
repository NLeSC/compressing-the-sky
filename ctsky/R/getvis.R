getvis = function(data=c(),resultpath  = c(),
                  dependent="f_int",independent="freq_eff",group="band",
                  id="runcat",timecol="taustart_ts",
                  do.log=TRUE,do.bar=TRUE,error.low="err_low",error.up="err_up",deplabel="Intensity (Jy)") {
  dep = dependent; ind = independent; idcol = id; groupcol = group
  if (length(resultpath) == 0) stop("Please specify resultpath")
  if (length(error.up) == 0) error.up = error.low
  # input: mydata2 truncated with only sources of interest
  # - mydata2 with: runcat, band, intensity, f_int_err, time, frequency, alpha, y0
  # plots: intensity vs freq, normal distributions, intensity vs time
  uid = unique(data[,idcol])
  pdf(file=paste0(resultpath,"/rsm_modelfits.pdf"),width=8.27,height=11.7)
  # x11()
  data[,ind] = data[,ind]*10^-6
  negs = which(data[,dep] < 0.000001)
  if (length(negs) > 0) data[,dep][negs] = 0.000001

  #-----------------------------------------------
  # loop through sources for creating plots
  for (i in 1:length(uid)) {
    dat = data[which(data[,idcol] == uid[i]),]
    uband = sort(unique(dat[,groupcol]))
    linkm = data.frame(band=rep(0,length(uband)),cluster=rep(0,length(uband))) #,freqi=rep(0,length(uband))
    cnt = 1
    for (ubandi in unique(uband)) {
      linkm[cnt,1] = ubandi
      linkm[cnt,2] = mean(dat[,ind][which(dat[,groupcol] == ubandi)])
      cnt = cnt + 1
    }
    tb5 = length(unique(uband))
    if (tb5 < 5) {
      tb6 = 5
    } else if (tb5 >= 5 & tb5 <= 7) {
      tb6 = 4
    } else if (tb5 > 7) {
      tb6 = 5
    }
    extp0 = which(dat$extract_type==0)
    extp1 = which(dat$extract_type==1)
    par(mfrow=c(tb6,2),mar=c(4,5,4,2)+0.1,oma=c(0,0,0,0))
    MY = range(c(dat[,dep]))
    # plot dots
    if (do.log==TRUE) {
      dl = "xy"
    } else {
      dl = ""
    }
    plot(dat[,ind][extp0],dat[,dep][extp0],type="p",pch=20,cex=1,
         ylim=MY,xlab="Frequency (MHz)",ylab=deplabel,
         log=dl,main=paste0("source id: ",uid[i])) # ,   "  alpha: ",round(100*dat$alpha[1])/100)
    if (length(extp1) > 0) lines(exp(dat[,ind][extp1]),exp(dat[,dep][extp1]),type="p",pch=17,cex=0.6,lwd=0.6)
    # plot model
    xx = seq(min(dat[,ind]),max(dat[,ind]),by=diff(range(dat[,ind])) / 10)
    yy = exp(dat$alpha[1]*log(xx) + dat$y0[1])

    lines(xx,yy,type="l",col="red")
    legend("topright",legend=c("type = 0","type = 1"),pch=c(20,17),cex=0.5)

    plot.new()
    for (bandi in 1:length(uband)) {
      poslist = c("topleft","topright","bottomleft","bottomright")
      da = dat[,dep][which(dat[,groupcol] == uband[bandi])]
      dfr = dat[,ind][which(dat[,groupcol] == uband[bandi])]

      err.low = dat[,error.low][which(dat[,groupcol] == uband[bandi])]
      err.up = dat[,error.up][which(dat[,groupcol] == uband[bandi])]
      if (error.up != error.low) { # calculate difference
        err.low = abs(err.low-da)
        err.up = abs(err.up-da)
      }
      # now create histograms:
      h =hist(da,breaks=20,plot=FALSE)
      OB=c(h$counts)
      xhist = (h$breaks[1:length(h$breaks) - 1] + h$breaks[2:length(h$breaks)]) / 2
      steps = xhist[2]-xhist[1]
      nextra = 5
      b1 = rep(xhist[1],nextra) - (c(nextra:1)*steps)
      b2 = rep(xhist[length(xhist)],nextra) + (c(1:nextra)*steps)
      xhist = c(b1,xhist,b2)
      OB = c(rep(0,nextra),OB,rep(0,nextra))
      ES = dnorm(xhist,mean=mean(da),sd=sd(da))
      ES = ES *  (sum(OB)/(sum(ES)))
      p = barplot(OB,names.arg=as.character(format(xhist,digits=2)),ylim=c(0,max(OB,ES)),
                  main="", #paste0("Band ",uband[bandi]," | Frequency (MHz): ",round(dfr[1]))
                  cex.main=0.8,cex.lab=1,horiz=FALSE,xlab=deplabel,ylab="N")
      lines(p,ES,col="red")
      time = dat[,timecol][which(dat[,groupcol] == uband[bandi])]
      plot(as.POSIXlt(time,origin="1970-01-01 00:00.00"),da,type="p",pch=20,cex.main=1,
           ylim=range(da)*c(0.8,1.2),main=paste0("Band ",uband[bandi]), #" | Frequency (MHz): ",round(dfr[1])),
           xlab="timestamp",ylab=deplabel)
      if (do.bar == TRUE) {
        error.bar <- function(x, y, upper, lower=upper, length=0.02,...){
          if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
            stop("vectors must be same length")
          x = as.numeric(x)
          nonneg = which(upper > 0.003 & lower > 0.003 & is.na(upper) == FALSE & is.na(lower) == FALSE)
          if (length(nonneg) > 0) {
            arrows(x[nonneg],y[nonneg]+upper[nonneg], x[nonneg], y[nonneg]-lower[nonneg], angle=90, code=3, length=length, ...)
          }
        }
        error.bar(x=as.POSIXlt(time,origin="1970-01-01 00:00.00"),y=da, lower=err.low,upper=err.up)
        grid()
      }
    }
  }
  dev.off()
  dummy = FALSE
}
