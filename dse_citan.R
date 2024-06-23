getCoreDSEWorks <- function(scopusPath="./data/scopus.csv",
                            wosPath="./data/wos.txt",
                            rmList="rm-list.txt",
                            titleList="title-list.txt") {

  scopus <- convert2df(file = scopusPath, dbsource = 'scopus',
                       format = "csv")
  # 276 from Scopus
  wos <- convert2df(file = "./data/wos.txt", dbsource = 'wos',
                    format = "plaintext")
  # 170 from WoS

  coreDSEworks <- mergeDbSources(scopus, wos)
  rm(scopus, wos)
  # cleaning and merging yields 293 records

  # rm-list.txt identifies:
  # records that are so incomplete that the text cannot be found;
  # records that are duplicated (which breaks referenceMatrix construction); and
  # records the represent full proceedings for conferences that were not
  # primarily focused on data science education.
  rmlist <- scan(rmList, what="", sep="\n")

  # I identified rm-list members manually; if you want to check my work uncomment
  # what's below and review the two CSV outputs. I could automate some of this
  # with doi match but it's not reliable. Maybe build in as a step.
  # write.csv(coreDSEworks[ coreDSEworks$UT %in% rmlist, ], "removedRefWorks.csv")
  # write.csv(mergeDbSources(scopus, wos), "inspectWorks.csv")

  coreDSEworks <- coreDSEworks[ ! coreDSEworks$UT %in% rmlist, ]
  print( paste("Removed",length(rmlist),"records using",rmList) )

  # Some records (e.g. websites, reference works) do not have TI populated
  # This duplicates the SO col to TI to make the record user-friendly
  #TODO WHY THE HECK JUST LOOK AT THE EMPTIES?
  titlelist <- scan("title-list.txt", what="", sep="\n")
  for(ut in titlelist) { coreDSEworks["TI"][coreDSEworks["UT"]==ut] <-
    coreDSEworks["SO"][coreDSEworks["UT"]==ut] }
  print(paste("Transferred",length(titlelist),"source names to blank titles"))

  rm(rmlist,titlelist,ut)

  return(coreDSEworks)
}

getMatches <- function(refs,dist=5){
  # first, get the rows and columns of items that closely but not exactly match
  # be patient, this is a monster. Full refs list of 8592
  # yields 595 dupe matches. Mains are lower indices = more frequently ref'd
  # TODO: these need a tree structure since some typos are typos of typos.
  matches <- as.data.frame(which(stringdist::stringdistmatrix(
    refs[["CR"]], refs[["CR"]]) < dist, arr.ind=TRUE)) %>%
    filter(row<col)
  colnames(matches) <- c("main","dupe")
  return( matches %>% arrange(matches,main) )
}
