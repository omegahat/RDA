
#
# Missing values.
#
# Instead of reading the values (e.g. int, double), can we seek ahead
# eventhough we are in a gzfile(). We want to advance say 40 bytes for
# a vector of 10 integers.
#
#
#
#

# Toc for an RDA file.
# From Rinternals.h
NILSXP = 0	  
SYMSXP = 1	  
LISTSXP = 2	  
CLOSXP = 3	  
ENVSXP = 4	  
PROMSXP = 5	  
LANGSXP = 6	  
SPECIALSXP = 7	  
BUILTINSXP = 8	  
CHARSXP = 9	  
LGLSXP = 10	  
INTSXP = 13	  
REALSXP = 14	  
CPLXSXP = 15	  
STRSXP = 16	  
DOTSXP = 17	  
ANYSXP = 18	  
VECSXP = 19	  
EXPRSXP = 20	  
BCODESXP = 21    
EXTPTRSXP = 22    
WEAKREFSXP = 23    
RAWSXP = 24    
S4SXP = 25    

# From serialize.c
REFSXP =            255
NILVALUE_SXP =      254
GLOBALENV_SXP =     253
UNBOUNDVALUE_SXP =  252
MISSINGARG_SXP =    251
BASENAMESPACE_SXP = 250
NAMESPACESXP =      249
PACKAGESXP =        248
PERSISTSXP =        247
CLASSREFSXP =       246
GENERICREFSXP =     245
# Byte code which is conditionally defined. Doesn't hurt
# us to include it, but they don't end in SXP so we can't
# include them with the simple pattern in the dput call below.
#BCREPDEF          244
#BCREPREF          243
EMPTYENV_SXP =	  242
BASEENV_SXP =	  241

#############################################################################################


# dput(sapply(objects(pattern = ".*SXP$"), get))
SEXPTypes =
structure(c(18, 241, 250, 21, 8, 9, 246, 3, 15, 17, 242, 4, 20, 
22, 245, 253, 13, 6, 10, 2, 251, 249, 0, 254, 248, 247, 5, 24, 
14, 255, 25, 7, 16, 1, 252, 19, 23), .Names = c("ANYSXP", "BASEENV_SXP", 
"BASENAMESPACE_SXP", "BCODESXP", "BUILTINSXP", "CHARSXP", "CLASSREFSXP", 
"CLOSXP", "CPLXSXP", "DOTSXP", "EMPTYENV_SXP", "ENVSXP", "EXPRSXP", 
"EXTPTRSXP", "GENERICREFSXP", "GLOBALENV_SXP", "INTSXP", "LANGSXP", 
"LGLSXP", "LISTSXP", "MISSINGARG_SXP", "NAMESPACESXP", "NILSXP", 
"NILVALUE_SXP", "PACKAGESXP", "PERSISTSXP", "PROMSXP", "RAWSXP", 
"REALSXP", "REFSXP", "S4SXP", "SPECIALSXP", "STRSXP", "SYMSXP", 
"UNBOUNDVALUE_SXP", "VECSXP", "WEAKREFSXP"))

##############################################################################################

tocRDA =
function(filename = system.file("sampleData", "m.rda", package = "RTools"),
         as.data.frame = TRUE)
{
  if(inherits(filename, "connection"))
    con = filename
  else {
    con = gzfile(filename, "rb")
    on.exit(close(con))
  }

  attr(con, "symbols") = e = new.env()
  e$.count = 0
  e$.packages = character()
#  e$.offsets = integer()
        
  header = readHeader(con)
  type = getFlags(con)

  readDottedPair(con, type, ignore = !as.data.frame, verbose = FALSE, computeOffsets = TRUE)
}


readTag =
function(con)
{
    # SYMSXP
  tp = getFlags(con)
  if(inherits(tp, "REFSXP")) {
    readItem(con, tp)
  } else if(!inherits(tp, "SYMSXP")) {
recover()
  } else {
      # CHARSXP
    readXDRInteger(con)
      # The content!
    val = readString(con)
    addToCachedSymbols(val, attr(con, "symbols"))
    val
  }
}


addToCachedSymbols =
function(val, env, verbose = FALSE)
{
  if(is.null(env))
    return()

  env$.count = env$.count + 1
  if(verbose)
     cat("caching", val, class(val), "as", env$.count, "\n")
  assign(as.character(env$.count), val, env)
}

getCachedSymbol =
function(index, env)
{
  if(is.null(index))
    return(NA)
  
  if(is.character(index))
    return(index)

  if(length(index) == 1 && is.na(index))
    return(NA)
  
  if(is.null(env))
    return()
  
   if(inherits(index, "REFSXP"))
     index = index["refIndex"]

   if(inherits(env, 'connection'))
     env = attr(env, 'symbols')

 # temporary
  if(!exists(as.character(index), env, inherits = FALSE))
    return(as.character(index))
  
  get(as.character(index), env)
}


readCharacterVector =
function(con, len = readXDRInteger(con))
{
  replicate(len, { getFlags(con); readString(con)})
}


getDottedElementInfo =
  #
  #  consume a tag if appropriate
  #  read the flag
  #
function(con, type, verbose = 0)
{

  hasTag = type["hasTag"] && !inherits(type, "REFSXP")

  if(hasTag) {
    id = readTag(con)
    if(verbose)
      cat(id, "\n") # getCachedSymbol(id, con)
  }

  type = getFlags(con)

  if(hasTag)  
    attr(type, "name") = id
  
  type
}


readDottedPair =
  #
  #<NOTANYMORE> Starts by reading just after the LISTSXP flag
  # i.e. the caller is assumed to have eaten that herself.
  #</NOTANYMORE>
  #
function(con, type = getFlags(con), ignore = TRUE, verbose = 0,
          computeOffsets = FALSE)
{

  if(inherits(type, "NILVALUE_SXP"))
    return(NULL)

  ans = list(getDottedElementInfo(con, type, verbose))
  ans[[1]] = readItem(con, ans[[1]])
  i = 2  
  while(TRUE) {
    type = getFlags(con)
    if(inherits(type, "NILVALUE_SXP"))   {
       break
    }

    ans[[i]] = getDottedElementInfo(con, type, verbose = verbose)
    if(verbose)
      cat(i, class(ans[[i]]), "\n")

    ans[[i]] = readItem(con, ans[[i]])

    i = i + 1
  }

 
 ids = sapply(ans, function(x) getCachedSymbol(attr(x, "name"), attr(con, "symbols")))

 if(ignore)
    return(ans)
  
  makeDataFrame(ans, con)
}

makeDataFrame =
  #
  # Turn the result of the readDottedPair loop into a data frame
  # Each element has the basic elements
  #         type hasAttrs hasTag isS4Object levels length
  # or maybe not length
  # 
function(els, env = NULL)
{
    # The names of the objects as the  row names.
    # But for SYMSXP, etc. we may have REFSXPs
  rownames = sapply(els, function(x) getCachedSymbol(attr(x, "name"), env))    

  w = sapply(els, inherits, "REFSXP")
  if(any(w))
     els[w] = lapply(els[w], getCachedSymbol, env)

  
    # find the variables from each 
  vars = unique(unlist(lapply(els, names)))
    # Get the value of each variable from each element into a list
    # so we can put it the variables into a data frame.
  tmp = lapply(vars, function(id) sapply(els, `[`, id))
  names(tmp) = vars

    # Deal with the following variables as logicals.
  logicals = c("hasAttrs", "hasTag", "isS4Object")
  tmp[logicals] = lapply(tmp[logicals], as.logical)

    # Add the name of the type
  tmp$typeName = names(SEXPTypes) [ match(tmp$type, SEXPTypes) ]

  as.data.frame(tmp, row.names = rownames)[, rev(seq(along = tmp))]
}


readAttributes =
function(con, eatLISTSXP = TRUE)
{
  if(eatLISTSXP)
    readDottedPair(con, ignore = TRUE)
  else
    readDottedPair(con, c(), ignore = TRUE)    
}


readItem =
function(con, flags = getFlags(readXDRInteger(con)))
 UseMethod("readItem", flags)


readItem.CHARSXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
  flags["value"]  = readString(con)
  flags
}


readItem.PROMSXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
  if(flags["hasAttrs"])
    readAttributes(con)

  if(flags["hasTag"])
    readTag(con)
  
  x = readItem(con)
  flags
}

readItem.UNBOUNDVALUE_SXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
    readXDRInteger(con)
}

readItem.NAMESPACESXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
   zero = readXDRInteger(con)
   # Have to add to cached symbol table.
   tmp = readCharacterVector(con)
   flags["name"] = tmp[1]
   if(length(tmp) > 1)
     flags["version"] = tmp[2]   
   addPackage(flags["name"], con)
   flags
}

addPackage =
function(pkg, where)
{
  e = attr(where, "symbols")
  e$.packages = c(e$.packages, where)
  pkg
}

readItem.BUILTINSXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
  flags["name"]  = readString(con)
  flags
}


readItem.REFSXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
  #  Determine when the index is not in the packed type
  #  but the next integer in the stream. See serialize.c - InRefIndex
  #
  if(flags["refIndex"] == 0)
    flags["refIndex"] = readXDRInteger(con)

  getCachedSymbol(flags, attr(con, "symbol"))
  
  flags
}

readItem.S4SXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
  ans = readDottedPair(con) # , verbose = TRUE)  # readAttributes(con, TRUE)
  flags["len"] = length(ans)
  flags
}

readItem.GLOBALENV_SXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
  flags
}



readItem.ENVSXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
  addToCachedSymbols(flags, attr(con, "symbols"))  
  locked = readXDRInteger(con)

  tp = getFlags(con)
  x = readItem(con, tp)   # enclos

  tp = getFlags(con)
  readItem(con, tp)       # FRAME
#  readItem(con)

  tp = getFlags(con)  
  readItem(con, tp)       # HASHTAB

  tp = getFlags(con)
  if(!inherits(tp, "NILVALUE_SXP"))
      readDottedPair(con, tp) # ATTRIB


  flags
}

addRestoredEnv =
function()
{

}

readItem.CLOSXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
  # e.g. source
  if(flags["hasAttrs"]) 
    readAttributes(con, TRUE)

      # environment
    #XXX need to be smarter about this.
  tp = getFlags(con)
  if(!inherits(tp, "GLOBALENVSXP"))
    readItem(con, tp)
  
      # parameter list as a dotted pair.
  tp = getFlags(con)

  args = if(inherits(tp, "LISTSXP"))
            readDottedPair(con, tp, ignore = TRUE)
         else if(!inherits(tp, "NILVALUE_SXP"))
           cat("function parameter type", class(tp), "\n")

      # read the body of the function
  tp = getFlags(con)
  body = readItem(con, tp)

  flags
#  readDottedPair(con)
}

readItem.VECSXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
   len = flags["len"] = readXDRInteger(con)
   tmp = replicate(len, {
                  tp = getFlags(con)
                  readItem(con, tp)
              })
   
   if(flags["hasAttrs"]) {
       readAttributes(con)
   }

   flags
}


readItem.SYMSXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
   readXDRInteger(con)
   val = readString(con)
#   cat("SYMSXP:", val, "\n")
   val
}

readItem.LANGSXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
   if(flags["hasAttrs"])
     readAttributes(con, TRUE)

   tp = getFlags(con)
   # If this is a SYMSXP, probably "{"
   if(inherits(tp, "SYMSXP"))
     readItem(con, tp)

   tp = getFlags(con)

   readDottedPair(con, tp, ignore = TRUE)
}


readItem.MISSINGARG_SXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
  flags
}

readItem.NILVALUE_SXP =
function(con, flags = getFlags(readXDRInteger(con)))
   NULL


readItem.LISTSXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
  x = readDottedPair(con, flags, ignore = TRUE)
  flags
}

readItem.STRSXP =
function(con, flags = getFlags(readXDRInteger(con)))
{
    flags["length"] = len = readXDRInteger(con)
    readCharacterVector(con, len)
    if(flags["hasAttrs"])
      readAttributes(con)
    flags
}  

readItem.default =
  #
  #  called when we 
  #
function(con, flags = getFlags(readXDRInteger(con)))
{
  orig =  flags

  isVector = isVectorType(flags["type"])
  
  if(!isVector && flags["hasTag"]) {
     id = flags["name"] = readTag(con)
     flags = getFlags(con)
     flags["name"] =  id
  }

    # Some types have attributes before the "payload"
    # (to avoid recursive calls when serializing)
    # so deal with those now for those types.
  if(flags["hasAttrs"] && attrsAtFront(flags["type"]))
    readAttributes(con, TRUE)

  
  if(isVector) {
    flags["length"] = len = readXDRInteger(con)
    readVector(con, flags["type"], len)
  } else if(!inherits(flags, "NILSXP"))
     stop("unhandled case in readItem() ", flags["type"], " ", class(flags))

  if(flags["hasAttrs"] && !attrsAtFront(flags["type"]))
    readAttributes(con, TRUE)
  
  flags
}

attrsAtFront =
function(type)
{
   type %in% c(LANGSXP, CLOSXP, PROMSXP, DOTSXP)
}  


readHeader =
function(con)
{  
  header = list(format = c(readChar(con, 3), readChar(con, 1)))
  readChar(con, 1) # the \n
  header$type = readChar(con, 1) # X, A, B
  readChar(con, 1) # the \n

     # The pair of versions
  header$version = replicate(3, readXDRInteger(con))

  invisible(header)
}

  # after this, we a LISTSXP (type 2)
  #   (tmp = readXDRInteger(con)
  #   type = bitAnd(tmp, 255L)  # DECODE_TYPE()


################################
#
#  Utilities for making sense of integer codes.
#

decodeType =
function(val)
{
  bitAnd(val, 255L)
}

readFlag = getFlags =
function(val)
{
  if(is(val, "connection"))
    val = readXDRInteger(val)

  type = bitAnd(val, 255L)  
  class = names(SEXPTypes)[ match( bitAnd(val, 255L), SEXPTypes) ]
  if(is.na(class))
    stop("Don't recognize the type")
  
  structure(c(type = type,
              if(class != "REFSXP")
                 c(hasAttrs = bitAnd(val, 2L^9L) > 0)
              else
                  c(refIndex = bitShiftR(val, 8)),
              hasTag = bitAnd(val, 2L^10L) > 0,
              isS4Object = bitAnd(val, 2L^8L) > 0,
              levels = NA), ###
            class = class)
}

# Vectors


numBytesPerEl =
  c('10' = 4,
    '13' = 4,
    '14' = 8
    )
  
isVectorType =
function(type)
  as.character(type) %in% names(numBytesPerEl)


readVectorValues =
function(con, type, len = readXDRInteger(con))
{
  if(type == "16")
     return(readCharacterVector(con, len))
  
  getEl = switch(as.character(type),
                   "10" = readXDRInteger,
                   "13" = readXDRInteger,
                   "14" = readXDRDouble,
                   "16" = function(con) {
                             readXDRInteger(con)
                             readString(con)
                           })
  ans = replicate(len, getEl(con))
  if(type == 10)
    as.logical(ans)
  else
    ans
}


readVector =
function(con, type, len = readXDRInteger(con))
{
         # and ignore these.
   readBin(con, "raw", len * numBytesPerEl[ as.character(type) ])
# It would be nice to be able to just move forward.   
#   seek(con, len * numBytesPerEl[ as.character(type) ], origin = "current")
}


########################
#
#  Primitives - Scalars
#

readString =
function(con, len = readXDRInteger(con))
{
  if(len != -1)
     paste(readChar(con, len), collapse = "")
  else
    as.character(NA)
}

readXDRInteger =
function(con)
{
  els = readBin(con, "raw", 4)
  .Call("R_convertXDRInteger", els)
}

readXDRDouble =
function(con)
{
  els = readBin(con, "raw", 8)
  .Call("R_convertXDRDouble", els)
}


readSym =
function(con, readSYMSXP = TRUE)
{
  if(readSYMSXP)
     t = RDA:::readXDRInteger(con) # SYMSXP identifier
  
  t = RDA:::readXDRInteger(con) # CHARSXP identifier
  nchar = RDA:::readXDRInteger(con) # number of characters
  RDA:::readString(con, nchar)
}
