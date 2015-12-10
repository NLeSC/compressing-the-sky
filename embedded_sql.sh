#!/bin/bash

#dbname=lc
dbname=rsm
uname=monetdb
passw=monetdb
sqlloc=~/CTS/dump.merge_rsm.sql
sname=trap

echo "===create and release database:"
monetdbd start monetdbfarm
monetdb stop ${dbname}
monetdb destroy -f ${dbname}
monetdb create ${dbname}
monetdb set embedr=true ${dbname}
monetdb start ${dbname}
monetdb release ${dbname}

echo "===set user and schema:"
mclient -d ${dbname} -s"\
--ALTER USER \"monetdb\" RENAME TO \"${uname}\";
--ALTER USER SET PASSWORD '${passw}' USING OLD PASSWORD 'monetdb';
CREATE SCHEMA \"${sname}\" AUTHORIZATION \"${uname}\";
ALTER USER \"${uname}\" SET SCHEMA \"${sname}\";\
"
echo "===fill database with data from dump:"
DOTMONETDBFILE=~/.rsm mclient -d ${dbname} ${sqlloc}

echo "===create table with data that needs to be processed in R:"
DOTMONETDBFILE=~/.rsm mclient -d ${dbname} -s"\
CREATE TABLE mydata AS SELECT runcat, freq_eff, extract_type, f_int, band, taustart_ts, f_int_err, xtrsrc, id FROM (
SELECT a1.runcat,a1.xtrsrc,i2.freq_eff,i2.taustart_ts,x1.extract_type,x1.f_int,x1.f_int_err,i2.band,i2.id
                            FROM assocxtrsource a1,
                            (SELECT t1.runcat
                            FROM
                            (SELECT a.runcat, i.band, count(*)
                            FROM assocxtrsource a, runningcatalog r, extractedsource x, image i
                            WHERE a.runcat = r.id and a.xtrsrc = x.id and x.image = i.id
                            GROUP BY runcat, band having count(*) > 30) t1
                            GROUP BY t1.runcat) t2, runningcatalog r1, extractedsource x1, image i2 WHERE
  a1.runcat = r1.id and a1.runcat = t2.runcat and a1.xtrsrc=x1.id and x1.image= i2.id)
AS si_view WITH DATA;\
"
echo "===create function:"
DOTMONETDBFILE=~/.rsm mclient -d ${dbname} -s"\
CREATE FUNCTION rapi01(runcat INTEGER, freq_eff double, extract_type BOOLEAN, f_int DOUBLE, band INTEGER, taustart_ts TIMESTAMP, f_int_err DOUBLE, xtrsrc INTEGER, id INTEGER)
  RETURNS TABLE (runcat INTEGER, band INTEGER, freqmean DOUBLE, N integer, Na integer, sd_band DOUBLE, X2 DOUBLE, X2_crit DOUBLE, m_band DOUBLE, df_band DOUBLE, sd_source DOUBLE, df_source DOUBLE,  alpha DOUBLE, y0 DOUBLE, alpha_se DOUBLE, y0_se DOUBLE, X2_source DOUBLE, X2_crit_source DOUBLE, kurtosis DOUBLE) LANGUAGE R{
	D = data.frame(runcat=runcat,xtrsrc=xtrsrc,freq_eff=freq_eff,taustart_ts=taustart_ts,extract_type=extract_type,f_int=f_int,f_int_err=f_int_err,band=band,imageid=id)
	install.packages('/home/vincent/CTS/ctsky_0.2.tar.gz',repos=NULL,type='source')
	require(ctsky)
	install.packages(pkgs=c('nortest','moments'),repos='http://cran-mirror.cs.uu.nl')
	getmodel(data=D,dependent='f_int',independent='freq_eff',group='band',id='runcat',
               mcmc=FALSE,include_ext1=FALSE,minN=30,do.res=FALSE,CX2=0.95,
		do.log=TRUE,imidcol='imageid',xtrsrc='xtrsrc',modeltime=FALSE)
};\
"
echo "------------------------------"
echo "===apply function and store as view:"
DOTMONETDBFILE=~/.rsm mclient -d ${dbname} -s"\
CREATE VIEW models AS SELECT * FROM rapi01((SELECT runcat, freq_eff, extract_type, f_int, band, taustart_ts, f_int_err, xtrsrc, id FROM mydata AS X))
;\
"
echo "------------------------------"
echo "===create view of approximated intensity"
DOTMONETDBFILE=~/.rsm mclient -d ${dbname} -s"\
CREATE VIEW mydata2 AS
	SELECT c.* , EXP(y0+(alpha*LOG(freq_eff*POWER(10,-6)))) AS int_approx
	FROM
	(SELECT
		a.*, b.alpha, b.y0
	FROM
		mydata a
		RIGHT JOIN (SELECT * FROM models WHERE X2_source > X2_crit_source) b
	ON a.runcat = b.runcat AND a.band = b.band) c
;\
"
echo "===create function2:"
DOTMONETDBFILE=~/.rsm mclient -d ${dbname} -s"\
CREATE FUNCTION rapi02(runcat INTEGER, band INTEGER, alpha DOUBLE, y0 DOUBLE, xtrsrc INTEGER, freq_eff DOUBLE, taustart_ts TIMESTAMP, extract_type INTEGER, f_int DOUBLE, f_int_err DOUBLE)
  RETURNS TABLE (dummy BOOLEAN) LANGUAGE R{
	D = data.frame(runcat=runcat,band=band,alpha=alpha,y0=y0,xtrsrc=xtrsrc,freq_eff=freq_eff,taustart_ts=taustart_ts,extract_type=extract_type,f_int=f_int,f_int_err=f_int_err)
	install.packages('/home/vincent/CTS/ctsky_0.2.tar.gz',repos=NULL,type='source')
	require(ctsky)
	install.packages(pkgs=c('nortest','moments'),repos='http://cran-mirror.cs.uu.nl')
save(D,file='/home/vincent/CTS/removeme.RData')
getvis(data=D,resultpath='/home/vincent/CTS/results',dependent='f_int',independent='freq_eff',group='band',id='runcat',timecol='taustart_ts',do.log=TRUE,do.bar=TRUE,error.low='f_int_err',error.up=c(),deplabel='Intensity (Jy)')
};\
"
echo "------------------------------"
echo "===apply function and store as view:"
DOTMONETDBFILE=~/.rsm mclient -d ${dbname} -s"\
SELECT * FROM rapi02((SELECT runcat, band, alpha, y0, xtrsrc, freq_eff, taustart_ts, extract_type, f_int, f_int_err FROM mydata2 AS X))
;\
"
