mapCleanRefs <- function(refWorks,charExcludeList="[\\:\\(\\)+\\?\\|\\\"\\“\\”\\,'\\`\\‘\\.]") {
  print("Be patient. Maybe go grab a coffee.")
  
  # manual fix this one record resulting from a wayward semicolon
  # semicolon is a separator value so we can't strip it
  # this part of the reference is reintroduced to its parent via manualdupes
  refWorks <- refWorks %>% 
    filter(CR != "JOURNAL OF STATISTICS EDUCATION 27 3 PP 265-274")
  refWorks$counted <- FALSE
  
  
  # STEP 1
  # combine matching refworks after exclusing special characters.
  for( i in 1:nrow(refWorks) ) { # for each refwork
    if( ! refWorks$counted[i] ) { #if not already counted
      compareTo <- refWorks$CR[i] # with this same name
      dupes <- refWorks[refWorks$CR==compareTo,] # here they are
      
      if( nrow( dupes ) > 1 ) { # if more than one
        
        # first set all the dupes to 0 frequency and counted = true
        refWorks[refWorks$CR==compareTo,]$Freq <- 0
        refWorks[refWorks$CR==compareTo,]$counted <- TRUE
        
        #except set my parent back to the sum of freqencies and still subject
        #to further aggregation in subsequent steps
        refWorks$Freq[i] <- sum( dupes$Freq )
        refWorks$counted[i] <- FALSE
      }
    }
  }
  refWorks <- refWorks[!refWorks$counted,] # just keep the aggregated refs
  
  #### A TEST for the above - do the sums match? 10/8 yes
  
  
  # now, we turn refWorks into a lookup table. 
  refWorks$correctedFreq <- 0
  refWorks$correctedCR <- ""
  

  
  # load up some manual duplications i'd like to fix
  # if you are updating this analysis, the biggest offender are references with
  # very large author lists, because these lists are treated differently by 
  # different communities. you can print out the likely dupes during
  # the shortnaming process in dse_citan.R
  manual <- read.csv("./data/manualdupes.txt", sep=",")
  for( i in 1:nrow(refWorks) ) {
    compareTo <- refWorks$CR[i]
    
    # point the bad refs to the corrected ref 
    if( compareTo %in% manual$CR ) {
      refWorks$correctedCR[i] <- manual$correctedCR[manual$CR==compareTo]
    }
  }
  
  # after all the pointers are done, tally up the sums
  for( i in 1:nrow(refWorks) ) {
    compareTo <- refWorks$CR[i]
    
    # if this one is a correct one
    if( compareTo %in% refWorks$correctedCR ) {
      # aggregate all freq to this ref
      refWorks$correctedFreq[i] <- refWorks$Freq[i] + sum( refWorks[refWorks$correctedCR==compareTo,]$Freq )
      refWorks[refWorks$correctedCR==compareTo,]$counted <- TRUE
    }
  }
  
  
  # I'm matching on the first 70% of the ref. if there are multiple versions
  # of something like a textbook, this would aggregate them. It looks at the
  # first part since the last part is most likely where reasonable differences
  # emerge - journal abbreviations, different levels of detail (doi,
  # conference dates, page numbers etc).
  pb = txtProgressBar(min = 0, max = nrow(refWorks), initial = 0, style = 3)
  
  # for each item that hasn't yet been counted
  for( i in 1:nrow(refWorks) ) {

    # get a clean version of the reference text
    compareTo <- refWorks$CR[i]
    
    # if there is any meat to the entry
    if( str_length(compareTo) > 40 ) {
      
      # make a group from the refs 
      refGroup <- refWorks %>% 
        filter( !counted ) %>% #that haven't already been captured manually
        filter( CR %like% substr(compareTo,0,str_length(compareTo)*.70) ) # and match the start
      
      # if we find this group has any entries, let's consolidate them
      # all our refWorks are distinct strings, so we can just use that to
      # select what we want from refGroup and apply it to refWorks
      if( nrow(refGroup) > 1 ) {
        
        # make the most cited entry the parent. look first at corrected Freq 
        # then uncorrected freq. if there are ties, just whatever gets sorted 
        # to the top wins.
        correctCR <- (refGroup %>% 
                        arrange(desc(correctedFreq),desc(Freq)))[1,]$CR
        
        # pull the parent out of the reference group
        refGroup <- refGroup[refGroup$CR!=correctCR,]
        
        for( i in 1:nrow(refGroup)) {
          refWorks[refWorks$CR==refGroup$CR[i],]$counted <- TRUE
          refWorks[refWorks$CR==refGroup$CR[i],]$correctedCR <- correctCR
        }
        
        
        ### FIX, add the max of Freq or correctedFreq. Probably just a loop.
        if( refWorks[refWorks$CR == correctCR,]$correctedFreq > 0 ) {
          #if there's a corrected frequency add that to the others
          refWorks[refWorks$CR == correctCR,]$correctedFreq <-
            refWorks[refWorks$CR == correctCR,]$correctedFreq +
            sum(refGroup$Freq)
        } else { # just add all the frequencies
          refWorks[refWorks$CR == correctCR,]$correctedFreq <- 
            sum(refGroup$Freq) + refWorks[refWorks$CR == correctCR,]$Freq
        }
        #### END FIX
      }
    }
    setTxtProgressBar(pb,i)
  }
  
  
  # manual disaggregation of GAISE II vs GAISE COLLEGE
  refWorks[refWorks$CR=="BARGAGLIOTTI A FRANKLIN C ARNOLD P GOULD R JOHNSON S PEREZ L SPANGLER DA PRE-K-12 GUIDELINES FOR ASSESSMENT AND INSTRUCTION IN STATISTICS EDUCATION II GAISE II 2020",]$correctedCR <- ""
  refWorks[refWorks$CR=="DATA SCIENCE FOR UNDERGRADUATES OPPORTUNITIES AND OPTIONS 2018",]$correctedFreq <-
  refWorks[refWorks$CR=="DATA SCIENCE FOR UNDERGRADUATES OPPORTUNITIES AND OPTIONS 2018",]$correctedFreq -
  refWorks[refWorks$CR=="BARGAGLIOTTI A FRANKLIN C ARNOLD P GOULD R JOHNSON S PEREZ L SPANGLER DA PRE-K-12 GUIDELINES FOR ASSESSMENT AND INSTRUCTION IN STATISTICS EDUCATION II GAISE II 2020",]$Freq

  
  
  #finally, put the correctedCR to all the actual correct CRs for easy lookup
  refWorks[refWorks$correctedCR=="",]$correctedCR <- refWorks[refWorks$correctedCR=="",]$CR
}

cleanRefs <- function(coreDSEworks) {
  # the moment of truth: the less common duplicates of references in the
  # actual reference list of coreDSEworks are replaced with the
  # most common form: so citations are aggregated, linked, and mapped properly.
  for( i in 1:nrow(coreDSEworks) ) {
    # pull apart the refList for this core work into a dataframe
    thisRefsList <- as.data.frame(str_split(coreDSEworks$CR[i],"; "))
    colnames(thisRefsList) <- c("ref")
    thisRefsList$ref <- gsub(charExcludeList,'',thisRefsList$ref)
    
    # use cleanRefs lookup to replace duplicate records with main
    thisRefsList[] <- refWorks$correctedCR[match(unlist(thisRefsList), refWorks$CR)]
    
    # stitch it all back together and put it back in the core work
    coreDSEworks$CR[i] <- paste(thisRefsList$ref,collapse="; ")
  }
  return( coreDSEworks )
}
