Compressing the Sky
====================

Path-finding project with Netherlands eScience Center
Code relates to our work to compress and model astronomical data (Lofar)
Project duration: June 2015-December 2015

Guidance on folder structure:
-----------------------------
- ctsky_0.2.tar.gz in the root directory is the R-package source code
- /ctsky includes all files from which ctsky_0.2.tar.gz is compiled

For my own reference, notes on how to install MonetDB with R integration:
-------------------------------------------
- hg clone http://dev.monetdb.org/hg/MonetDB/ MonetDB
- cd MonetDB
- hg update Jul2015
- ./bootstrap
- ./configure —prefix=/some/install/dir —enable-rintegration=yes —enable-optimize
- make -j clean install
- into ~/.bashrc: export PATH=$PATH:/some/install/dir/bin
