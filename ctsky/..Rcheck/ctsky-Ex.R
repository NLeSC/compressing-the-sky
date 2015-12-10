pkgname <- "ctsky"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ctsky')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("approxdata")
### * approxdata

flush(stderr()); flush(stdout())

### Name: approxdata
### Title: approximate data based on the models as derived with function
###   getmodel
### Aliases: approxdata

### ** Examples

## Not run: 
##D rep.data = approxdata(models=models)
## End(Not run)



cleanEx()
nameEx("getmodel")
### * getmodel

flush(stderr()); flush(stdout())

### Name: getmodel
### Title: derive models from data
### Aliases: getmodel

### ** Examples

## Not run: 
##D V = getmodel(data=D,dependent="f_int",independent="freq_eff",group="band",id="runcat",
##D                  mcmc=FALSE,include_ext1=FALSE,minN=minN,doinR=doin.R,CX2=CX2)
## End(Not run)



cleanEx()
nameEx("getvis")
### * getvis

flush(stderr()); flush(stdout())

### Name: getvis
### Title: generate visualisation from the data as store these in a pdf
### Aliases: getvis

### ** Examples

## Not run: 
##D getvis(data=mydata2)
## End(Not run)



cleanEx()
nameEx("studyshell")
### * studyshell

flush(stderr()); flush(stdout())

### Name: studyshell
### Title: shell function for studying the data
### Aliases: studyshell

### ** Examples

## Not run: 
##D studyshell(data=D,resultpath= "~/CTS/results/",CX2=0.95)
## End(Not run)



cleanEx()
nameEx("testcompr")
### * testcompr

flush(stderr()); flush(stdout())

### Name: testcompr
### Title: test data compression
### Aliases: testcompr

### ** Examples

## Not run: 
##D result = testcompr(data=D,path=filepath)
## End(Not run)



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
