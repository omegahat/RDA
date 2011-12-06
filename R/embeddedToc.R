toc.len.len = 4 + 4 * 2 + 4 + 4
toc.len.str.len =  8 + 4 + 4

setClass("RDAFile", contains = "character")

setMethod("names", "RDAFile",
           function(x)
             names(getTOC(x)))

setClass("RDATOC",
          representation(names = "character", offsets = "integer",
                         timeRead = "POSIXct", filename = "character"))

setMethod("show", "RDATOC",
            function(object)
                show(object@offsets))

setMethod("$", "RDATOC",
          function(x, name) {
            con = gzfile(x@filename, "rb")
            on.exit(close(con))
            seek(con, x@offsets[name])
            id = readSym(con)
browser()
            readItem(con)
#            readRObject(con)
          })

 

getTOC =
function(file, reopen = FALSE)
{
   timeRead = Sys.time()
   nbytes = file.info(file)[1, "size"]
   con = gzfile(file, "rb")
   on.exit(close(con))

   info = getTOCInfo(con, nbytes)
   numEls = info["numEls"]
   toc.size = (numEls + 3) * 4 + 4 + 4
   # Reopen the file for the moment.
   # Eventhough seek(con) gives the same value on the newly opened connection or the old
   # connection, the next read of the integers is 4 bytes off.

   if(reopen) {
      close(con)
      con = gzfile(file, "rb")     
   } else {
      seek(con, 0)
   }
   seek(con, nbytes - toc.size - toc.len.len - toc.len.str.len - 4 - 4)     

   i = replicate(2, RDA:::readXDRInteger(con)) 
   toc = replicate(numEls + 2, RDA:::readXDRInteger(con)) # note numEls + 2, not 3 as the last entry for .toc.len is garbage (why?)

   if(reopen) {
     close(con)   
     con = gzfile(file, "rb")
   }

   ids = getTOCNames(con, toc[numEls + 1])[1:(numEls+2)]
   new("RDATOC", names = ids,
                 offsets = structure(toc, names = ids), 
                 timeRead = timeRead,
                 filename = path.expand(file))
}

getTOCInfo =
function(con, nbytes)
{
  if(is.character(con)) {
     if(missing(nbytes))
        nbytes = file.info(con)[1, "size"]
     con = gzfile(con, "rb")
  }
toc.len.len = 2 * 4    
  seek(con, nbytes - toc.len.len, "start")
  off = readXDRInteger(con)
print(off)  
  seek(con, off)
  readIntegerVector(con)
  structure(replicate(4, RDA:::readXDRInteger(con))[3:4], names = c("numEls", "totalNumCharsInNames"))
}

getOffsets =
  #
  #  This assumes the offsets integer vector is the last 
  # element of the RDA file.
  #
function(con, nbytes)
{
  if(is.character(con)) {
     if(missing(nbytes))
        nbytes = file.info(con)[1, "size"]
     con = gzfile(con, "rb")
  } else {
    cur = seek(con)
    on.exit(seek(con, cur))
  }

  toc.len.len = 2 * 4    
  seek(con, nbytes - toc.len.len, "start")
  off = readXDRInteger(con)


  seek(con, off, "start")
  readSym(con, FALSE)
  readXDRInteger(con)
  readVectorValues(con, INTSXP)  
}


getTOCNames = 
function(con, offset)
{
  seek(con, offset, "start")
  RDA:::readXDRInteger(con) # SYMSXP
  RDA:::readXDRInteger(con) # CHARSXP
  RDA:::readString(con, len = RDA:::readXDRInteger(con))
  RDA:::readXDRInteger(con) # STRSXP
  num = RDA:::readXDRInteger(con) # length

  replicate(num, {
                   RDA:::readXDRInteger(con)
                    RDA:::readString(con, RDA:::readXDRInteger(con))
                 })
}


getNamesByOffset =
  #
  # just academic if we embedd the .tocNames
  # But not if we don't but want to get the names
  # by jumping from offset to offset and getting the
  # SYMSXP as a string.
function(file)  
{
  info = getTOC(file)
  con = gzfile(file, "rb")
  on.exit(close(con))
  sapply(info@offsets,
          function(off) {
             seek(con, off)
             readSym(con)
         })
}
