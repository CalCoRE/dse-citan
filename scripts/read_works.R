getCoreDSEWorks <- function(scopusPath="./data/scopus.csv",
                            wosPath="./data/wos.txt",
                            rmList="./data/rm-list.txt") {

  scopus <- convert2df(file = scopusPath, dbsource = 'scopus',
                       format = "csv")
  print(paste("Scopus record count:",count(scopus)))
  
  wos <- convert2df(file = wosPath, dbsource = 'wos',
                    format = "plaintext")
  print(paste("WoS record count:",count(wos)))

  coreDSEworks <- mergeDbSources(scopus, wos)
  print(paste("After merge:",count(coreDSEworks)))

  # rm-list.txt identifies:
  # records that are so incomplete that the text cannot be found;
  # records that are duplicated (which breaks referenceMatrix construction); and
  # records the represent full proceedings for conferences that were not
  # primarily focused on data science education.
  rmlist <- scan(rmList, what="", sep="\n")

  coreDSEworks <- coreDSEworks[ ! coreDSEworks$UT %in% rmlist, ]
  print( paste("Removed",length(rmlist),"records manually using",rmList) )
  
  return(coreDSEworks)
}
