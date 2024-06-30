mapCleanRefs <- function(refWorks,charExcludeList='[\\:\\(\\)+\\?\\|\\"\\“\\”\\,]') {
  print("Be patient. Maybe go grab a coffee.")
  
  # fix this one record resulting from a wayward semicolon
  # this part of the reference is reintroduced to its family via manualdupes
  refWorks <- refWorks %>% 
    filter(CR != "JOURNAL OF STATISTICS EDUCATION, 27, 3, PP. 265-274")

  refWorks$CR <- gsub(charExcludeList,'',refWorks$CR) #NEW
  
  cleanRefLookup <- as.data.frame(matrix(ncol=4)) ## if it breaks, set nrow=1
  colnames(cleanRefLookup) <- c("Freq", "CR", "correctedFreq", "correctedCR")
  refWorks$counted <- FALSE
  
  # load up some manual duplications i'd like to fix
  # if you are updating this analysis, the biggest offender are references with
  # very large author lists, because these lists are treated differently by 
  # different communities. luckily, these tend to be rare so identify them,
  # clean them, and move on with your life
  manual <- read.csv("manualdupes.txt", sep=";")
  manual$CR <- gsub(charExcludeList,'',manual$CR)
  manual$correctedCR <- gsub(charExcludeList,'',manual$correctedCR)
  
  # I'm matching on the first 70% of the ref. if there are multiple versions
  # of something like a textbook, this would aggregate them. It looks at the
  # first part since the last part is most likely where reasonable differences
  # emerge - journal abbreviations, different levels of detail (doi,
  # conference dates, page numbers etc).
  pb = txtProgressBar(min = 0, max = nrow(refWorks), initial = 0, style = 3)
  for( i in 1:nrow(refWorks) ) {
    
    # get a clean version of the reference text
    compareTo <- gsub(charExcludeList,'',refWorks$CR[i])
    
    # if there is any meat to the entry
    if( str_length(compareTo) > 40) {
      
      # get the group of like refs that haven't already been captured
      refGroup <- refWorks %>% filter(counted==FALSE) %>%
        filter( gsub(charExcludeList,'',CR) %like% substr(
          compareTo,0,str_length(compareTo)*.70) )
      
      manualRefs <- manual %>% filter(correctedCR == compareTo)
      manualGroup <- refWorks %>% 
        filter(gsub(charExcludeList,'',CR) %in% manualRefs$CR)
      
      refGroup <- rbind(refGroup,manualGroup)
      
      # if we find this ref has any like entries, let's consolidate them
      if( nrow(refGroup) > 0 ) { 
        
        #make the most popular one parent
        refGroup$correctedCR <- compareTo
        
        #zero out cite count of children and add to parent
        refGroup$correctedFreq <- 0
        refGroup$correctedFreq[1] <- sum(refGroup$Freq)
        
        #pull these refWorks out of the list waiting to be grouped
        refWorks$counted[refWorks$CR %in% refGroup$CR] <- TRUE
        
        #add the adjusted group entries to cleanRefs
        cleanRefLookup <- bind_rows(cleanRefLookup, refGroup)
      } else {
        refWorks$counted[i] <- TRUE #ignore in future groupings
      }
    } else {
      refWorks$counted[i] <- TRUE #ignore in future groupings
    }
    setTxtProgressBar(pb,i)
  } 
  return( cleanRefLookup )
}
