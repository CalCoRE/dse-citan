# Bibliometric analysis exploring what papers that describe themselves as
# "data science education" cite as references. This is designed to be run from
# top to bottom.

# Michelle Hoda Wilkerson
# updated Jun 27, 2024

library(agop)
library(bibliometrix)
library(dplyr)
library(stringr)
library(textTools)

# this script expects the working directory to be this source file's location
source("read_works.R")

# Records last updated 6/14/24.
# Query inputted to Scopus and WoS were "data science education" in the
# title, abstract, or keywords of a record.
# Remove corrections, retractions, notes. Keep early access records.

coreDSEworks <- getCoreDSEWorks()

# get a data frame of the references.
refWorks <- as.data.frame(
  citations(coreDSEworks, field = "article", sep = ";")$Cited)

# The reference list has typos that cause several references
# to be incorrectly counted and mapped. Below I'm making a lookup
# table that maps all the less common (but matching) reference formats
# back to whatever the most popular form of the reference is.

cleanRefLookup <- as.data.frame(matrix(ncol=4)) ## if it breaks, set nrow=1
colnames(cleanRefLookup) <- c("Freq", "CR", "correctedFreq", "correctedCR")
refWorks$counted <- FALSE

# NOTE: If the loop below breaks, or if your cleanRefs looks wonky, there is
# probably a new wonky character somewhere you need to exclude using the
# list below. Find the character by looking for cleanRefs records with
# uncharacteristically high citation counts mapped to misidentified dupes.
charExcludeList <- '[\\:\\(\\)+\\?\\|\\"]'

pb = txtProgressBar(min = 0, max = nrow(refWorks), initial = 0, style = 3)

# I'm matching on the first 70% of the ref. if there are multiple versions
# of something like a textbook, this would aggregate them. It looks at the
# first part since the last part is most likely where reasonable differences
# emerge - journal abbreviations, different levels of detail (doi,
# conference dates, page numbers etc).
for( i in 1:nrow(refWorks) ) {

  # get a clean version of the reference text
  compareTo <- gsub(charExcludeList,'',refWorks$CR[i])

  # if there is any meat to the entry
  if( str_length(compareTo) > 40) {

    # get the group of like refs that haven't already been captured
    refGroup <- refWorks %>% filter(counted==FALSE) %>%
      filter( gsub(charExcludeList,'',CR) %like% substr(
        compareTo,0,str_length(compareTo)*.70) )

    # if we find this ref has any like entries, let's consolidate them
    if( nrow(refGroup) > 0 ) { 
      
      #make the most popular one parent
      refGroup$correctedCR <- refGroup$CR[1]

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
} # You may want to go grab a coffee while this executes...

# the moment of truth: the less common forms of references in the
# actual reference list of coreDSEworks references are replaced with the
# most common form, so citations are counted, linked, and mapped properly.
for( i in 1:nrow(coreDSEworks) ) {
  # pull apart the refList for this core work into a dataframe
  thisRefsList <- as.data.frame(str_split(coreDSEworks$CR[i],"; "))
  colnames(thisRefsList) <- c("ref")

  # use cleanRefs lookup to replace duplicate records with main
  thisRefsList[] <- cleanRefLookup$correctedCR[match(unlist(thisRefsList), cleanRefLookup$CR)]

  # stitch it all back together and put it back in the core work
  coreDSEworks$CR[i] <- paste(thisRefsList$ref,collapse="; ")
}

# finally, let's approximate the shortnames of the references so we can match
# them up to how they appear in the matrix analyses below. Note that there will
# be duplicate shortnames for some records, but we're only interested in the
# most popular works. by matching to any record with this simplified shortname,
# i also catch lingering duplicate records for the more popular works that are
# most importantly shaping the analysis, which i add to the manual cleaning
# file over time.
shortname <- tolower(paste(
  word(cleanRefLookup$correctedCR),   # first word (last name)
  word(cleanRefLookup$correctedCR,2),     # second word (first initial)
  str_sub(cleanRefLookup$correctedCR,-5,-2))) # year
cleanRefLookup$shortname <- gsub(',','',shortname)

# clean up a bit
rm(pb,refGroup,thisRefsList,shortname,compareTo,i)

# build co-citation network of DSE cited works
refMatrix <- biblioNetwork(coreDSEworks, analysis = "co-citation",
                           network = "references", sep = "; ")

# summary results of core DSE works
coreBibAnalysis <- biblioAnalysis(data.frame(coreDSEworks))
summary(coreBibAnalysis, max=10)

# Map the network featured in the proposal text.
# I set a cutoff to include only papers referenced 4 or more times in the
# network. The clusters, however, are robust to cutoff changes
cutoff = as.integer(count(refWorks %>% filter(Freq>3)))
refNet=networkPlot(refMatrix, n = cutoff,
                   Title = "Co-Citation Network of Top DSE Reference Works",
                   size.cex=TRUE, size=15, remove.multiple=FALSE,
                   remove.isolates = TRUE, labelsize=.7, edgesize = 5,
                   edges.min=0, type = "fruchterman", cluster="louvain")
# louvain clustering seeks to "cut clusters at their joints"
net2VOSviewer(refNet,".")

# Let's check out the top "brokers" in our reference network - that is,
# the papers that are cited alongside works from otherwise distinct clusters.
# string their shortname of the appended # to match to refWorks record.
# (if there are duplicates, can find out from popularity)
refBrokers <- refNet[["cluster_res"]] %>% filter( btw_centrality >= 100 )
refBrokers %>% arrange(desc(btw_centrality)) %>% head(10)
refBrokers$vertex <- gsub("-[1-9]","",refBrokers$vertex )

# Let's join to the rest of their ref records

############ BROKEN
brokersLookup <- unlist(paste0(refBrokers$vertex))
brokers <- cleanRefLookup %>%
  filter( correctedFreq > 0 ) %>% 
  filter( !is.na( shortname ) ) %>% 
  filter( shortname %in% refBrokers$vertex )
############

mainRefs <- cleanRefLookup %>% filter(correctedFreq > 0)

# Now, look for "core" cluster membership of the referenced papers. These are
# papers that have low betweenness centrality. Examining only these papers that
# are cited only with others in their community, but not more broadly, can
# help us identify what are thematic distinctions between clusters.

# Find the records with the most name matches in the ref list.
# This is incomplete; ideally I'd use labelShort() and removeDuplicatedlabels()
# in biblioNetwork to look for closer matches. For now, this will signal the
# papers to pull and examine further but does not ensure exact ref matches.

# get first author names of member authors for each cluster
referenceClusterAuthors <- refNet[["cluster_res"]] %>%
  # restrict this to only authors of papers that are not very connected
  # outside of their specific cluster
  filter( btw_centrality < 100 ) %>%
  group_by(cluster) %>%
  summarize(authors = list(vertex))
