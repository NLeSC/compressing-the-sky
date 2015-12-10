rm(list=ls()); graphics.off()
require(MonetDB.R)
require(ctsky)
require(MCMCpack)
require(nortest)

cat("\nLoad data from MonetDB...") # Note that I am calculating f_ab, err_low, err_up on the fly
conn = dbConnect(MonetDB.R(),host="localhost", dbname="demo", user="monetdb", password="monetdb")
M = dbGetQuery(conn,paste("select c.*, POWER(10,-0.4 * (c.magnitude - c.sdss_offset - 8.90)) as f_ab,
                                      POWER(10,-0.4 * (c.magnitude - c.uncertainty - c.sdss_offset - 8.90)) as err_low,
                                      POWER(10,-0.4 * (c.magnitude + c.uncertainty - c.sdss_offset - 8.90)) as err_up
                          FROM (SELECT a.*, b.sdss_offset FROM filter b, dfsptn a WHERE b.id = a.filter) c limit 6108")) #6108
dbDisconnect(conn)

cat("\nDefine missing variables...")
M$taustart_ts = as.POSIXlt(M$exposure_midpoint*(1440*60),origin="1858-11-17")
M$freq_eff = (1/abs(M$sdss_offset)) * 10^6 # calculate frequency from wavelength (not sure if this is correct)
M$extract_type = 0; M$imageid = M$xtrsrc = 1111  # dummy value because not available in Canadian dataset
M$f_ab = M$f_ab * 1000 # convert to milli Jy to allow for easier reading in plot (arrows are not plotted for very small error bars in Jy)
cat("\n===================================\n")
cat("\nFit models, test model assumptions, and generate visualisation...")
cat("\nRun analyses with magnitude:\n")
VM = studyshell(data=M,resultpath= "~/CTS/results_canada_mag/",dependent="magnitude",independent="freq_eff",time="taustart_ts",
               id = "star",minN=30,group="filter",do.res=TRUE,CX2=0.95,do.log=FALSE,
               imidcol="imageid",xtrsrc="xtrsrc",error.low="uncertainty",error.up=c(),prec=c(),modeltime=TRUE,do.bar=TRUE,
               deplabel="Magnitude")
cat("\n===================================\n")
cat("\nRun analyses with intensity:\n")
VI = studyshell(data=M,resultpath= "~/CTS/results_canada_int/",dependent="f_ab",independent="freq_eff",time="taustart_ts",
               id = "star",minN=30,group="filter",do.res=TRUE,CX2=0.95,do.log=FALSE,
               imidcol="imageid",xtrsrc="xtrsrc",error.low="err_low",error.up="err_up",prec=c(),modeltime=TRUE,do.bar=TRUE,
               deplabel="Intensity (milli Jy)") #"f_int_err"
