rm(list=ls())
require(MonetDB.R)
require(ctsky)
require(MCMCpack)
require(nortest)
cat("\nLoad data from MonetDB...")
conn = dbConnect(MonetDB.R(),host="localhost", dbname="rsm", user="monetdb", password="monetdb")
D = dbGetQuery(conn,paste("SELECT a1.runcat,a1.xtrsrc,i2.freq_eff,i2.taustart_ts,x1.extract_type,x1.f_int,
                            x1.f_int_err,i2.band,i2.id
                            FROM assocxtrsource a1,
                            (SELECT t1.runcat
                            FROM
                            (SELECT a.runcat, i.band, count(*)
                            FROM assocxtrsource a, runningcatalog r, extractedsource x, image i
                            WHERE a.runcat = r.id and a.xtrsrc = x.id and x.image = i.id
                            GROUP BY runcat, band having count(*) > 30) t1
                            GROUP BY t1.runcat) t2, runningcatalog r1, extractedsource x1, image i2 WHERE
  a1.runcat = r1.id and a1.runcat = t2.runcat and a1.xtrsrc=x1.id and x1.image= i2.id ORDER BY a1.runcat,a1.xtrsrc",sep=""))
dbDisconnect(conn)

D = D[order(D$runcat,D$band),]
names(D)[which(names(D)=="id")] = "imageid"
cat("\n===================================\n")
cat("\nRun analyses with intensity:\n")
VI = studyshell(data=D,resultpath= "~/CTS/results_lofar/",dependent="f_int",independent="freq_eff",time="taustart_ts",
                id = "runcat",minN=30,group="band",do.res=TRUE,CX2=0.95,do.log=TRUE,
                imidcol="imageid",xtrsrc="xtrsrc",error.low="f_int_err",error.up=c(),prec=c(),modeltime=TRUE,do.bar=TRUE,
                deplabel="Intensity (Jy)")
