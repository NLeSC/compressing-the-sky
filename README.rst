Compressing the Sky
====================

Path-finding project with CWI Database Architectures Group
on compress and handling astronomical data (Lofar)
Project duration: June 2015-December 2015

Guidance on folder structure:
-----------------------------
- ctsky_0.2.tar.gz in the root directory is the R-package source code
- /ctsky includes all files from which ctsky_0.2.tar.gz is compiled
- ctsky.Rcheck/ctsky-manual.pdf is the package manual
- due to file size i have not uploaded any example data
- embedded_sql.sh is an example of how to embedded the package in MonetDB SQL code
- dfspt-explore.R and lofar-explore.R are example files for exploring the astronomy data from within R based on calles to the ctsky-package

Notes on how to install MonetDB with R integration:
-------------------------------------------
- check MonetDB website for latest updates and instructions, these are the commands I used
- hg clone http://dev.monetdb.org/hg/MonetDB/ MonetDB
- cd MonetDB
- hg update Jul2015
- ./bootstrap
- ./configure —prefix=/some/install/dir —enable-rintegration=yes —enable-optimize
- make -j clean install
- into ~/.bashrc: export PATH=$PATH:/some/install/dir/bin

