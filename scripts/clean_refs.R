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
        refWorks[refWorks$CR==compareTo,]$Freq <- 0
        refWorks[refWorks$CR==compareTo,]$counted <- TRUE
        
        # except set my parent back to the sum of frequencies in matchGroup
        # and set the parent as uncounted; still subject
        # to further aggregation in subsequent steps
        refWorks$Freq[referenceID] <- sum( matchGroup$Freq )
        refWorks$counted[referenceID] <- FALSE
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
      count <- count + 1
    }
  }
  
  print(paste("Mapped",count,"manual duplicate records to parents"))
  
  # after all the item corrections are done, tally up the sums
  for( referenceID in 1:nrow(refWorks) ) {
    compareTo <- refWorks$CR[referenceID]
    
    # if this one is the parent to any corrected refs
    if( compareTo %in% refWorks$correctedCR ) {
      # aggregate the child frequencies to the parent
      refWorks$correctedFreq[referenceID] <- 
        refWorks$Freq[referenceID] + 
        sum( refWorks[refWorks$correctedCR==compareTo,]$Freq )
      # call this one counted
      refWorks[refWorks$correctedCR==compareTo,]$counted <- TRUE
      refWorks[refWorks$correctedCR==compareTo,]$correctedFreq <- 0
    } 
  }
  
  print(paste("We now have",
              nrow(refWorks %>% filter(correctedFreq > 0)),
              "parent refs."))
  
  return(refWorks)
}

fuzzyMatch <- function(refWorks,threshold=.80) {
  count <- 0
  
  # referenceID'm matching on the first 70% of the ref. if there are multiple versions
  # of something like a textbook, this would aggregate them. It looks at the
  # first part since the last part is most likely where reasonable differences
  # emerge - journal abbreviations, different levels of detail (doi,
  # conference dates, page numbers etc).
  pb = txtProgressBar(min = 0, max = nrow(refWorks), initial = 0, style = 3)
  
  # for each item that hasn't yet been counted
  for( referenceID in 1:nrow(refWorks) ) {
    
    setTxtProgressBar(pb,referenceID)
    
    # get a clean version of the reference text
    compareTo <- refWorks$CR[referenceID]
    
    # if there is any meat to the entry
    if( str_length(compareTo) > 40 ) {
      
      # make a group from the refs 
      matchGroup <- refWorks %>% 
        filter( !counted ) %>% #that haven't already been captured manually
        filter( CR %like% substr(compareTo,0,str_length(compareTo)*threshold) ) # and match the start
      
      # if we find this group has any entries, let's consolidate them
      # all our refWorks are distinct strings, so we can just use that to
      # select what we want from matchGroup and apply it to refWorks
      if( nrow(matchGroup) > 1 ) {
        
        # make the most cited entry the parent. look first at corrected Freq 
        # then uncorrected freq. if there are ties, just whatever gets sorted 
        # to the top wins.
        correctCR <- (matchGroup %>% 
                        arrange(desc(correctedFreq),desc(Freq)))[1,]$CR
        
        # pull the parent out of the reference group
        matchGroup <- matchGroup[matchGroup$CR!=correctCR,]
        
        # call the parent counted
        refWorks[refWorks$CR==correctCR,]$counted <- TRUE
        
        # and keep count of all child matches
        count <- count + nrow(matchGroup)
        
        # add the sum of childrens' Freq to the parent
        if( refWorks[refWorks$CR == correctCR,]$correctedFreq > 0 ) {
          #if there's a corrected parent frequency add that to the others
          refWorks[refWorks$CR == correctCR,]$correctedFreq <-
            refWorks[refWorks$CR == correctCR,]$correctedFreq +
            sum(matchGroup$Freq)
        } else { # just add all the frequencies
          refWorks[refWorks$CR == correctCR,]$correctedFreq <- 
            refWorks[refWorks$CR == correctCR,]$Freq +
            sum(matchGroup$Freq)
        }
        
        #### FOR ALL IN THE MATCH GROUP
        ### UPDATE THE CRS
        ### UPDATE THE FREQ
        for( i in 1:nrow(matchGroup) ) {
          
          matchCR <- matchGroup$CR[i]
          #print(matchCR)
          refWorks[refWorks$CR==matchCR,]$correctedCR <- correctCR
          refWorks[refWorks$CR==matchCR,]$correctedFreq <- 0
        }
      }
    } 
  }
  print(paste("Found",count,"additional matches using fuzzy text."))
  print(paste("We now have",
              nrow(refWorks %>% filter(correctedFreq > 0)),
              "parent refs."))
  
  # finally, if a ref doesn't have a correctedCR then its existing CR
  # is the correct CR... copy that over.
  refWorks[refWorks$correctedCR == "",]$correctedCR <- 
    refWorks[refWorks$correctedCR == "",]$CR
  
  return( refWorks )
}

rewriteCleanRefs <- function(coreDSEworks,charExcludeList='[\\:\\(\\)+\\?\\|\\"\\“\\”\\,\'\\`\\‘\\.\\*]') {
  # the moment of truth: the less common duplicates of references in the
  # actual reference list of coreDSEworks are replaced with the
  # most common form: so citations are aggregated, linked, and mapped properly.
  for( coreWorkID in 1:nrow(coreDSEworks) ) {
    # pull apart the refList for this core work into a dataframe
    thisRefsList <- as.data.frame(str_split(coreDSEworks$CR_raw[coreWorkID],";"))
    colnames(thisRefsList) <- c("ref")
    thisRefsList$ref <- gsub(charExcludeList,'',thisRefsList$ref)
    
    # use cleanRefs lookup to replace duplicate records with main
    thisRefsList[] <- refWorks$correctedCR[match(unlist(thisRefsList), refWorks$CR)]
    coreDSEworks$debugmode[coreWorkID] <- thisRefsList
    
    # stitch it all back together and put it back in the core work
    coreDSEworks$CR[coreWorkID] <- paste(thisRefsList$ref,collapse="; ")
  }
  return( coreDSEworks )
}
