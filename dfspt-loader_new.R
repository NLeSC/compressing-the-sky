rm(list=ls())
library(MonetDB.R)

con <- dbConnect(MonetDB.R(), dbname="demo")

print("add table dfsptn")
if (dbExistsTable(con, "dfsptn")) dbRemoveTable(con, "dfsptn")
dbSendQuery(con, "CREATE TABLE dfsptn (exposure_midpoint DECIMAL(12,7), magnitude DECIMAL(5,3), uncertainty DECIMAL(5,3), filter TINYINT, star STRING)");
dbSendQuery(con, paste0("copy into dfsptn from '/home/vincent/CTS/data/dfspt.csv.bz2' using delimiters ' ','\n','' locked")); #

print("add table filter")
if (dbExistsTable(con, "filter")) dbRemoveTable(con, "filter")
dbSendQuery(con, "CREATE TABLE filter (id INT, band VARCHAR(2), sdss_offset DECIMAL(4,3))");
dbSendQuery(con, "INSERT INTO filter (id,band,sdss_offset) VALUES (1, 'U', -0.241),(2, 'G', -0.153),
(3, 'R', -0.024),(4, 'I', -0.085),(5, 'I2', -0.003),(6, 'Z', 0.074)");

dbDisconnect(con)
