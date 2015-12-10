\name{ctsky-package}
\alias{ctsky-package}
\alias{ctsky}
\docType{package}
\title{
\packageTitle{ctsky}
}
\description{
\packageDescription{ctsky}
}
\details{
\packageDESCRIPTION{ctsky}
\packageIndices{ctsky}
The package holds a set of functions I developed as part of the
path-finding project Compressing the sky into a large number of statistical
models.\cr
\cr
The package requires that MonetDB is installed with embedded R.\cr
\cr
The function \link{studyshell} is mainly a sanity check for all the code
as it reproduces all the performance evaluations The function \link{getmodel}
is probably the most important function: It generates the models and tests for
model assumptions.
}
\author{
\packageAuthor{ctsky}
}
\examples{
\dontrun{
library(MonetDB.R)
conn = dbConnect(MonetDB.R(),host="localhost", dbname="rsm", user="monetdb",
                            password="monetdb")
D = dbGetQuery(conn,paste("SELECT a1.runcat,a1.xtrsrc,i2.freq_eff,
                            i2.taustart_ts,x1.extract_type,x1.f_int,
                            x1.f_int_err,i2.band,i2.id
                            FROM assocxtrsource a1,
                            (SELECT t1.runcat
                            FROM
                            (SELECT a.runcat, i.band, count(*)
                            FROM assocxtrsource a, runningcatalog r,
                            extractedsource x, image i
                            WHERE a.runcat = r.id and a.xtrsrc = x.id and x.image = i.id
                            GROUP BY runcat, band having count(*) > 30) t1
                            GROUP BY t1.runcat) t2, runningcatalog r1,
                            extractedsource x1, image i2
                            WHERE
                            a1.runcat = r1.id and a1.runcat = t2.runcat and
                            a1.xtrsrc=x1.id and x1.image= i2.id
                            ORDER BY a1.runcat,a1.xtrsrc",sep=""))
dbDisconnect(conn)

D = D[order(D$runcat,D$band),]
studyshell(data=D,dependent="f_int",independent="freq_eff",group="band",id="runcat",
            resultpath= "~/CTS/results/",do.res=TRUE,CX2=0.95,minN = 30)

}
}
