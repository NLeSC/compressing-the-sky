testcompr = function(data=c(),path=c()) {

  compress <- function(filename, method, level=NA) {
    cfile <- paste0(filename, ".", method, ".", level)
    if (!file.exists(cfile)) {
      cflag <- ""
      if (!is.na(level)) {
        cflag <- paste0("-",level)
      }
      system(paste0(method, " -c -k ", cflag, "  ", filename, " > ", cfile))
    }
    compress = file.info(cfile)$size
  }

  test_ratio <- function(filename)
    data.frame(filename=filename, original=file.info(filename)$size,
               gzip_low=compress(filename, "gzip", 1),
               gzip_dft=compress(filename, "gzip", NA),
               gzip_hig=compress(filename, "gzip", 9),
               bzip2_low=compress(filename, "bzip2", 1),
               bzip2_dft=compress(filename, "bzip2", NA),
               bzip2_hig=compress(filename, "bzip2", 9),
               xz_low=compress(filename, "xz", 1),
               xz_dft=compress(filename, "xz", NA),
               xz_hig=compress(filename, "xz", 9))

  write_bin_int <- function(filename, vector) {
    f <- file(filename, open="wb", raw=T)
    writeBin(vector,f, size=4)
    close(f)
    test_ratio(filename)
  }
  sb = write_bin_int(filename=paste0(path,"data.bat"), vector=data)

#   print(paste0(path,"data.bat"))
#   print(file.info(paste0(path,"data.bat"))$size)
  saveRDS(sb,"bs.rds")

  removefile = function(fn) {
    if (file.exists(fn)) file.remove(fn)
  }
  removefile(paste0(path,"data.bat"))
  removefile(paste0(paste0(path,"data.bat"), ".gzip.", 1))
  removefile(paste0(paste0(path,"data.bat"), ".gzip.", NA))
  removefile(paste0(paste0(path,"data.bat"), ".gzip.", 9))
  removefile(paste0(paste0(path,"data.bat"), ".bzip2.", 1))
  removefile(paste0(paste0(path,"data.bat"), ".bzip2.", NA))
  removefile(paste0(paste0(path,"data.bat"), ".bzip2.", 9))
  removefile(paste0(paste0(path,"data.bat"), ".xz.", 1))
  removefile(paste0(paste0(path,"data.bat"), ".xz.", NA))
  removefile(paste0(paste0(path,"data.bat"), ".xz.", 9))
  testcompr = sb

}
