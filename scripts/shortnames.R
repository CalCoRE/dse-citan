refShortNames <- function(refWorks) {
  # let's approximate the shortnames of the references so we can map
  # them to the network analyses below. Note there will
  # be duplicate shortnames for some records, but we're only interested in the
  # most popular works. by matching to any record with this simplified shortname,
  # i also catch lingering duplicate records for the more influential works
  # shaping the field, which i add to the manual cleaning file over time.
  # note only first initial
  shortname <- tolower(paste0(
    word(refWorks$correctedCR), " ",  # first word (last name)
    str_sub(word(refWorks$correctedCR,2),1,1),     # second word (first initial)
    str_sub(refWorks$correctedCR,-5,-1))) # year
  refWorks$shortname <- gsub(',','',shortname)
  
  # for each distinct shortname - holding off for now, but may be needed to
  # link mutiple author-year refs
  parentRefShortnames <- as.data.frame(refWorks %>% distinct(shortname))
  
  # note this is a great place to identify lingering duplicates, check
  # especially for excessive repeats
  for( i in 1:nrow(parentRefShortnames) ) {
    # get all nonzero-after-correction records with this shortname
    records <- refWorks %>%
      filter(shortname %in% parentRefShortnames$shortname[i] ) %>%
      filter(correctedFreq > 0)
    # if there's more than one record, append numbers to identify each
    if( count( records ) > 1 ) {
      indices <- which(
        refWorks$shortname == parentRefShortnames$shortname[i] &
          refWorks$correctedFreq > 0, arr.ind = TRUE)
      append = 1
      for( index in indices ) {
        # if there are more than three records, let us know the name to check
        # for dupes
        if(append > 2) {
          print(refWorks[index,]$shortname)
        }
        refWorks[index,]$shortname <- paste0(
          refWorks$shortname[index] , "-", append )
        append <- append + 1
      }
    }
  }
  
  return( refWorks )
}

coreShortNames <- function(coreDSEworks,cleanRefLookup) {
  coreDSEworks$shortnameRefs <- ""
  # create a search string to match coreDSEworks to refNet shortnames. 
  for( i in 1:nrow(coreDSEworks) ) {
    # pull apart the refList for this core work into a dataframe
    records <- as.data.frame(str_split(coreDSEworks$CR[i],"; "))
    colnames(records) <- c("ref")
    
    # use cleanRefs lookup to replace duplicate records with main
    records[] <- cleanRefLookup$shortname[match(unlist(records), cleanRefLookup$CR)]
    
    # stitch it all back together and put it back in the core work
    coreDSEworks$shortnameRefs[i] <- paste(records$ref,collapse="|")
  }
  return(coreDSEworks)
}

lookupRefByShortname <- function(shortname) {
  
  # remember the shortnames we constructed are last name, first initial.
  # sometimes refNet's shortnames have last name, first name full.
  lookupShortname <- tolower(paste0(
    word(shortname), " ",  # first word (last name)
    str_sub(word(shortname,2),1,1),     # second word (first initial)
    str_sub(shortname,3))) # year
  
  records <- cleanRefLookup %>% 
    filter( lookupShortname %in% shortname )
  
  return( records )
}