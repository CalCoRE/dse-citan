getCoreDSEWorks <- function(scopusPath="./data/scopus.csv",
                            wosPath="./data/wos.txt",
                            rmList="./data/rm-list.txt") {

  scopus <- convert2df(file = scopusPath, dbsource = 'scopus',
                       format = "csv")
  message("Scopus record count: ",count(scopus))
  
  wos <- convert2df(file = wosPath, dbsource = 'wos',
                    format = "plaintext")
  message("WoS record count: ",count(wos))

  coreDSEworks <- mergeDbSources(scopus, wos)
  message("Records after automated merge: ",count(coreDSEworks))

  # rm-list.txt identifies:
  # records that are so incomplete that the text cannot be found;
  # records that are duplicated (which breaks referenceMatrix construction); and
  # records the represent full proceedings for conferences that were not
  # primarily focused on data science education.
  rmlist <- scan(rmList, what="", sep="\n")
  
  coreDSEworks <- coreDSEworks[ ! coreDSEworks$UT %in% rmlist, ]
  message("Removed additional records manually using ",rmList)
  
  message("There are currently ", count(coreDSEworks), " records in coreDSEworks.")
  
  coreDSEworks <- formatForTables(coreDSEworks)
  
  return(coreDSEworks)
}

formatForTables <- function(coreDSEworks) {
  coreDSEworks$CR <- coreDSEworks$CR_raw #for new data format
  
  #extract only the first author
  coreDSEworks$FA <- vapply(strsplit(coreDSEworks$AU,";"), 
                            `[`, 1, FUN.VALUE=character(1)) 
  
  #replace blanks with NA in relevant cols
  coreDSEworks$Page.start <- ifelse(coreDSEworks$Page.start=="",NA,coreDSEworks$Page.start)
  coreDSEworks$VL <- ifelse(coreDSEworks$VL=="",NA,coreDSEworks$VL)
  coreDSEworks$IS <- ifelse(coreDSEworks$IS=="",NA,coreDSEworks$IS)
  
  coreDSEworks$AU <- gsub(";", ", ", coreDSEworks$AU)
  
  #paste together in a format that mimics the refWorks collection
  coreDSEworks$CF <- paste( coreDSEworks$AU,
                            coreDSEworks$TI, 
                            coreDSEworks$SO, 
                            ifelse(is.na(coreDSEworks$VL), "", coreDSEworks$VL),
                            ifelse(is.na(coreDSEworks$IS), "", coreDSEworks$IS),
                            ifelse(is.na(coreDSEworks$Page.start), "", 
                                   paste0( "PP ", coreDSEworks$Page.start, 
                                           "-", coreDSEworks$Page.end)),
                            coreDSEworks$PY )
  
  #strip the long whitespaces
  coreDSEworks$CF <- gsub("\\s+", " ", coreDSEworks$CF)
  
  #strip the special chars
  coreDSEworks$CF <- gsub('[\\:\\(\\)+\\?\\|\\"\\“\\”\\,\'\\`\\‘\\.\\*]', "", coreDSEworks$CF)
  
  return(coreDSEworks)
}