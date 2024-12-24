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
  refWorks <- fuzzyMatch(refWorks,.80)
  
  # manual disaggregation of GAISE II vs GAISE COLLEGE
  #refWorks[refWorks$CR=="BARGAGLIOTTI A FRANKLIN C ARNOLD P GOULD R JOHNSON S PEREZ L SPANGLER DA PRE-K-12 GUIDELINES FOR ASSESSMENT AND INSTRUCTION IN STATISTICS EDUCATION II GAISE II 2020",]$correctedCR <- ""
  #refWorks[refWorks$CR=="DATA SCIENCE FOR UNDERGRADUATES OPPORTUNITIES AND OPTIONS 2018",]$correctedFreq <-
  #  refWorks[refWorks$CR=="DATA SCIENCE FOR UNDERGRADUATES OPPORTUNITIES AND OPTIONS 2018",]$correctedFreq -
  #  refWorks[refWorks$CR=="BARGAGLIOTTI A FRANKLIN C ARNOLD P GOULD R JOHNSON S PEREZ L SPANGLER DA PRE-K-12 GUIDELINES FOR ASSESSMENT AND INSTRUCTION IN STATISTICS EDUCATION II GAISE II 2020",]$Freq


  return(refWorks)  
}

cleanSpecialChars <- function(refWorks,charExcludeList='[\\:\\(\\)+\\?\\|\\"\\“\\”\\,\'\\`\\‘\\.\\*]') {
  count <- nrow(refWorks)
  
  refWorks$counted <- FALSE
  
  for( referenceID in 1:nrow(refWorks) ) { # for each refwork
    if( ! refWorks$counted[referenceID] ) { #if not already counted
      compareTo <- refWorks$CR[referenceID] # use this name to compare others
      matchGroup <- refWorks[refWorks$CR==compareTo,] # copy the matches
      
      if( nrow( matchGroup ) > 1 ) { # if more than one match
        
        # first set all the matching refWorks to 0 frequency and counted = true
        refWorks[refWorks$CR==compareTo,]$counted <- TRUE
        refWorks[refWorks$CR==compareTo,]$whatCorrected <- paste(refWorks[refWorks$CR==compareTo,]$whatCorrected,"specialchars")
        
        # except set my parent back to the sum of frequencies in matchGroup
        # and set the parent as uncounted; still subject
        # to further aggregation in subsequent steps
        refWorks$counted[referenceID] <- FALSE
        refWorks$whatCorrected[referenceID] <- ""
      }
    }
  }
  refWorksMatchesRemoved <- refWorks[!refWorks$counted,]
  
  print(paste("Removed",
              count-nrow(refWorksMatchesRemoved),
              "duplicate records with special characters."))
  
  return(refWorksMatchesRemoved) # drop the refs that were duplicates
}

cleanManualDuplicates <- function(refWorks,file="./data/manualdupes.txt") {
  manual <- read.csv("./data/manualdupes.txt", sep=",")
  count <- 0
  
  # for each item in refWorks
  for( referenceID in 1:nrow(refWorks) ) {
    compareTo <- refWorks$CR[referenceID]
    
    # if this reference is one of the ones that needs to be manually corrected
    if( compareTo %in% manual$CR ) {
      # then set the correctedCR for this ref to the manual correction
      refWorks$correctedCR[referenceID] <- 
        manual$correctedCR[manual$CR==compareTo]
      refWorks$whatCorrected[referenceID] <- paste(refWorks$whatCorrected[referenceID],"manualfirstloop")
      count <- count + 1
    }
  }
  
  print(paste("Mapped",count,"manual duplicate records to parents"))
  refWorks[refWorks$correctedCR == "",]$correctedCR <- 
    refWorks[refWorks$correctedCR == "",]$CR
  
  return(refWorks)
}


fuzzyMatch <- function(refWorks,threshold=.80) {
  count <- 0
  
  # referenceID'm matching on the middle thresold % of the ref. if there are 
  # multiple versions of something like a textbook, this would aggregate them. 
  # It looks at the middle part since the first and last part is where 
  # reasonable differences emerge - journal abbreviations, different levels of 
  # detail (doi, conference dates, page numbers etc).
  pb = txtProgressBar(min = 0, max = nrow(refWorks), initial = 0, style = 3)
  bar <- 0
  
  # for each unique correctedCR (e.g. CRs that have not already been identified
  # as duplicates)
  for( currentCorrectedCR in refWorks$correctedCR ) {
    
    setTxtProgressBar(pb,bar)
    
    # if there is any meat to the entry
    if( str_length(currentCorrectedCR) > 40 ) {
      
      # make a group from the refs that match the middle threshold %
      # length * (1-threshold) * .5
      matchGroup <- refWorks %>% 
        filter( !counted ) %>% #that haven't already been captured manually
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
                        arrange(desc(correctedFreq),desc(Freq)))[1,]$CR
        
        # pull the parent out of the reference group
        matchGroup <- matchGroup[matchGroup$CR!=correctCR,]
        
        # and keep count of all child matches
        count <- count + nrow(matchGroup)
        
        refWorks[refWorks$CR %in% matchGroup$CR,]$correctedCR <- correctCR
        refWorks[refWorks$CR %in% matchGroup$CR,]$whatCorrected <- paste(refWorks[refWorks$CR %in% matchGroup$CR,]$whatCorrected,"auto-match group")
      }
    } 
    bar <- bar + 1
  }
  print(paste("Found",count,"additional matches using fuzzy text."))
  
  
  return( refWorks )
}

correctFrequencies <- function(refworks) {
  # after all the item corrections are done, tally up the sums
  
  # for each reference with a correction
  for( correctCR in refWorks$correctedCR ) {
    
    # if more than one entry has this as a correctedCR, then
    # we'll tally up the sums
    if( nrow(refWorks[refWorks$correctedCR==correctCR,]) > 1 ) {
      
      totalRefs <- sum( refWorks[refWorks$correctedCR==correctCR,]$Freq )
      
      #set all the corretedFreq to 0 transfer to parent
      refWorks[refWorks$correctedCR==correctCR,]$correctedFreq <- 0
      
      # assign the sum of all frequencies to the parent
      refWorks[refWorks$CR==correctCR,]$correctedFreq <- totalRefs
      
    }
  }
  
  print(paste("We now have",
              nrow(refWorks %>% filter(correctedFreq > 0)),
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
