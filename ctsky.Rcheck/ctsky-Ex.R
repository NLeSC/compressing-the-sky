pkgname <- "ctsky"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "ctsky-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('ctsky')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("approxdata")
### * approxdata

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: approxdata
### Title: approximate data based on the models as derived with function
###   getmodel
### Aliases: approxdata

### ** Examples

## Not run: 
##D rep.data = approxdata(models=models)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("approxdata", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ctsky-package")
### * ctsky-package

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ctsky-package
### Title: Compressing Radio Astronomy Data With Domain Inspired
###   Statistical Models
### Aliases: ctsky-package ctsky

### ** Examples

## Not run: 
##D library(MonetDB.R)
##D conn = dbConnect(MonetDB.R(),host="localhost", dbname="rsm", user="monetdb",
##D                             password="monetdb")
##D D = dbGetQuery(conn,paste("SELECT a1.runcat,a1.xtrsrc,i2.freq_eff,
##D                             i2.taustart_ts,x1.extract_type,x1.f_int,
##D                             x1.f_int_err,i2.band,i2.id
##D                             FROM assocxtrsource a1,
##D                             (SELECT t1.runcat
##D                             FROM
##D                             (SELECT a.runcat, i.band, count(*)
##D                             FROM assocxtrsource a, runningcatalog r,
##D                             extractedsource x, image i
##D                             WHERE a.runcat = r.id and a.xtrsrc = x.id and x.image = i.id
##D                             GROUP BY runcat, band having count(*) > 30) t1
##D                             GROUP BY t1.runcat) t2, runningcatalog r1,
##D                             extractedsource x1, image i2
##D                             WHERE
##D                             a1.runcat = r1.id and a1.runcat = t2.runcat and
##D                             a1.xtrsrc=x1.id and x1.image= i2.id
##D                             ORDER BY a1.runcat,a1.xtrsrc",sep=""))
##D dbDisconnect(conn)
##D 
##D D = D[order(D$runcat,D$band),]
##D studyshell(data=D,dependent="f_int",independent="freq_eff",group="band",id="runcat",
##D             resultpath= "~/CTS/results/",do.res=TRUE,CX2=0.95,minN = 30)
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ctsky-package", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getmodel")
### * getmodel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getmodel
### Title: derive models from data
### Aliases: getmodel

### ** Examples

## Not run: 
##D V = getmodel(data=D,dependent="f_int",independent="freq_eff",group="band",id="runcat",
##D                  mcmc=FALSE,include_ext1=FALSE,minN=minN,doinR=doin.R,CX2=CX2)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getmodel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getvis")
### * getvis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getvis
### Title: generate visualisation from the data as store these in a pdf
### Aliases: getvis

### ** Examples

## Not run: 
##D getvis(data=mydata2)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getvis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("studyshell")
### * studyshell

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: studyshell
### Title: shell function for studying the data
### Aliases: studyshell

### ** Examples

## Not run: 
##D studyshell(data=D,resultpath= "~/CTS/results/",CX2=0.95)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("studyshell", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("testcompr")
### * testcompr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: testcompr
### Title: test data compression
### Aliases: testcompr

### ** Examples

## Not run: 
##D result = testcompr(data=D,path=filepath)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("testcompr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
