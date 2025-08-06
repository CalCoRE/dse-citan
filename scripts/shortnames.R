refShortNames <- function(refWorks,n=3) {
  # let's approximate the shortnames of the references so we can map
  # them to the network analyses below. Note there will
  # be duplicate shortnames for some records, but we're only interested in the
  # most popular works. by matching to any record with this simplified shortname,
  # i also catch lingering duplicate records for the more influential works
  # shaping the field, which i add to the manual cleaning file over time.
  # note only first initial
  
  # make a shortname "last first yyyy"
  
  author <- tolower(paste0(
    word(refWorks$correctedCR), # first word (last name)
    " ",  
    substr(word(refWorks$correctedCR,2),0,1)))
    
  year <- tolower(str_extract(refWorks$correctedCR,regex("\\b(19|20)\\d{2}\\b")))
  
  shortname <- paste(author,year)
  
  refWorks$firstAuthor <- toupper(author)
  refWorks$year <- year
  # get commas out
  refWorks$shortname <- gsub(',','',shortname)
  
  # pull each distinct shortname
  parentRefShortnames <- as.data.frame(refWorks %>% distinct(shortname))
  
  # for each distinct shortname
  for( i in 1:nrow(parentRefShortnames) ) {
    # get all nonzero-after-correction records with this shortname
    records <- refWorks %>%
      filter(freqCit > 0) %>%
      filter(shortname %in% parentRefShortnames$shortname[i] ) %>%
      arrange(desc(freqCit))
    
    # if there's more than one record, append numbers to less popular duplicates
    if( count( records ) > n ) {
      # Uncomment the line below to print shortnames that need appending.
      # This can be useful for identifying duplicate records.
       print(paste("Multiple records for shortname ",
                  parentRefShortnames$shortname[i]))
      
      indices <- refWorks %>% 
        filter(shortname == parentRefShortnames$shortname[i]) %>%
        filter(freqCit > 0) %>%
        arrange(desc(freqCit))
      append = 1
      for( ref in indices$CR ) {
        # if there are more than two records, let us know the name to check
        # for dupes
        refWorks[refWorks$CR==ref,]$shortname <- paste0(
          refWorks[refWorks$CR==ref,]$shortname, 
          ifelse(append > 1, paste0("-", append ), ""))

        append <- append + 1
        
        # Uncomment the line below to print refs associated with each shortname. 
        # This can be useful for identifying duplicate records.
        #print(ref)
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
    word(shortname,2),     # second word (first initial)
    str_sub(shortname,3))) # year
  
  records <- cleanRefLookup %>% 
    filter( lookupShortname %in% shortname )
  
  return( records )
}