mapCleanRefs <- function(refWorks) {
  print("Be patient. Maybe go grab a coffee.")
  
  # manual fix this one record resulting from a wayward semicolon
  # semicolon is a separator value so we can't strip it
  # this part of the reference is reintroduced to its parent via manualdupes
  refWorks <- refWorks %>% 
    filter(CR != "JOURNAL OF STATISTICS EDUCATION 27 3 PP 265-274")
  
  # STEP 1
  # combine matching refworks after excluding special characters.
  refWorks <- cleanSpecialChars(refWorks)

  # STEP 2
  # load up some manual duplication referenceID'd like to fix
  # if you are updating this analysis, the biggest offender are references with
  # very large author lists, because these lists are treated differently by 
  # different communities. you can print out the likely matchGroup during
  # the shortnaming process in dse_citan.R
  refWorks <- cleanManualDuplicates(refWorks,"./data/manualdupes.txt")
  
  # PART 3
  refWorks <- autoMatch(refWorks,.80)

  return(refWorks)  
}

cleanSpecialChars <- function(refWorks,charExcludeList='[\\:\\(\\)+\\?\\|\\"\\“\\”\\,\'\\`\\‘\\.\\*]') {
  
  for( referenceID in 1:nrow(refWorks) ) { # for each refwork
    compareTo <- refWorks$CR[referenceID] # use this name to compare others
    matchGroup <- refWorks[refWorks$CR==compareTo,] # copy the matches
    
    if( nrow( matchGroup ) > 1 ) { # if more than one match
      tally <- sum(refWorks[refWorks$CR==compareTo,]$Freq) #tally all match freq
      
      # first set all the matching refWorks to 0 frequency
      refWorks[refWorks$CR==compareTo,]$Freq <- 0
      
      # except set my parent back to the sum of frequencies in matchGroup
      # and set the parent as uncounted; still subject
      # to further aggregation in subsequent steps
      refWorks$Freq[referenceID] <- tally
    }
  }
  
  # drop the refs that were duplicates
  return(refWorks %>% filter(Freq > 0) %>% filter(str_length(CR) > 10)) 
}

cleanManualDuplicates <- function(refWorks,file="./data/manualdupes.txt") {
  manual <- read.csv("./data/manualdupes.txt", sep=",")
  
  # for each item in refWorks
  for( refCR in refWorks$CR ) {
    compareTo <- refWorks[refWorks$CR==refCR,]$CR
    
    # if this reference is one of the ones that needs to be manually corrected
    if( compareTo %in% manual$CR ) {
      # then set the correctedCR for this ref to the manual correction
      refWorks[refWorks$CR==refCR,]$correctedCR <- 
        manual$correctedCR[manual$CR==compareTo]
    }
  }
  
  refWorks[refWorks$correctedCR == "",]$correctedCR <- 
    refWorks[refWorks$correctedCR == "",]$CR
  
  return(refWorks)
}


autoMatch <- function(refWorks,threshold=.80) {
  # referenceID'm matching on the middle thresold % of the ref. if there are 
  # multiple versions of something like a textbook, this would aggregate them. 
  # It looks at the middle part since the first and last part is where 
  # reasonable differences emerge - journal abbreviations, different levels of 
  # detail (doi, conference dates, page numbers etc).
  
  # for each unique correctedCR (e.g. CRs that have not already been identified
  # as duplicates)
  for( currentCorrectedCR in refWorks$correctedCR ) {
    
    # if there is any meat to the entry
    if( str_length(currentCorrectedCR) > 40 ) {
      
      # make a group from the refs that match the middle threshold 
      matchGroup <- refWorks %>% 
        filter( CR %like% 
                  substr(currentCorrectedCR,
                         str_length(currentCorrectedCR) * (1 - threshold) * .5,
                         str_length(currentCorrectedCR) - 
                           (str_length(currentCorrectedCR) * (1 - threshold) * .5)) )
      
      # if we find this group has any entries other than the CR itself,
      # assign the correctedCR
      if( nrow(matchGroup) > 1 ) {
        
        # make the most cited entry the parent. look first at corrected Freq 
        # then uncorrected freq. if there are ties, just whatever gets sorted 
        # to the top wins.
        correctCR <- (matchGroup %>% 
                        arrange(desc(freqAgg),desc(Freq)))[1,]$correctedCR 
        
        # pull the parent out of the reference group
        matchGroup <- matchGroup[matchGroup$CR!=correctCR,]
        
        refWorks[refWorks$CR %in% matchGroup$CR,]$correctedCR <- correctCR
      }
    } 
  }
  
  return( refWorks )
}

# this function aggregates ref frequencies across all CR listed in refWorks
# to the parent correctedCR. This will overcount when a reference appears 
# more than once in the same ref list. It is useful when used in combo with
# the lookup-based counter below to triangulate findings.
correctFrequenciesAgg <- function(refworks) {
  # after all the item corrections are done, tally up the sums
  
  # for each reference with a correction
  for( correctCR in refWorks$correctedCR ) {
    
    # if more than one entry has this as a correctedCR, then
    # we'll tally up the sums
    if( nrow(refWorks[refWorks$correctedCR==correctCR,]) > 1 ) {
      
      totalRefs <- sum( refWorks[refWorks$correctedCR==correctCR,]$Freq )
      
      #set all the corretedFreq to 0 transfer to parent
      refWorks[refWorks$correctedCR==correctCR,]$freqAgg <- 0
      
      # assign the sum of all frequencies to the parent
      refWorks[refWorks$CR==correctCR,]$freqAgg <- totalRefs
      
    }
  }
  
  print(paste("We now have",
              nrow(refWorks %>% filter(freqAgg > 0)),
              "parent refs."))
  return(refWorks)
}

# this function aggregates ref frequencies by identifying the number of 
# records in coreDSEworks that include the reference in the CR list.
# this is the aggregation that's used in the analysis reported.
correctFrequenciesCited <- function(refworks,coreDSEworks) {
  # after all the item corrections are done, tally up the sums
  refWorks$freqCit <- 0
  
  # for each reference with a correction
  for( correctCR in refWorks$correctedCR ) {

    refWorks[refWorks$CR==correctCR,]$freqCit <- 
      sum(str_detect(coreDSEworks$CR, correctCR), na.rm=TRUE)
  }

  print(paste("We now have",
              nrow(refWorks %>% filter(freqCit > 0)),
              "parent refs."))
  return(refWorks)
}

rewriteCleanRefs <- function(coreDSEworks,charExcludeList='[\\:\\(\\)+\\?\\|\\"\\“\\”\\,\'\\`\\‘\\.\\*]') {
  # the moment of truth: the less common duplicates of references in the
  # actual reference list of coreDSEworks are replaced with the
  # most common form: so citations are aggregated, linked, and mapped properly.
  
  coreDSEworks$CR <- as.list(str_split(gsub(charExcludeList,'',coreDSEworks$CR_raw),";"))
  coreDSEworks$CR <- lapply(coreDSEworks$CR, function(x) trimws(x) )
  coreDSEworks$CR <- lapply(coreDSEworks$CR, function(x) refWorks$correctedCR[match(unlist(x), refWorks$CR)])
  coreDSEworks$CR <- lapply(coreDSEworks$CR, function(x) paste(x,collapse="; "))
  
  return( coreDSEworks )
}

# TODO: col 14 is doi. See if I can use this to make titles clickable. so cute!