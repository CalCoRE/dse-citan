# Bibliometric analysis exploring what papers that describe themselves as
# "data science education" cite as references
# Michelle Hoda Wilkerson
# updated Jun 27, 2024

library(agop)
library(bibliometrix)
library(dplyr)
library(stringr)
library(textTools)

source("dse_citan.R")

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

cleanRefs <- as.data.frame(matrix(nrow=1,ncol=4))
colnames(cleanRefs) <- c("Freq", "CR", "correctedFreq", "correctedCR")
refWorks$counted <- FALSE

# NOTE: If the loop below breaks, or if your cleanRefs looks wonky, there is
# probably a new funky character somewhere you need to exclude using the
# list below. Find the character by looking for cleanRefs records with
# uncharacteristically high citation counts mapped to misidentified dupes.
charExcludeList <- '[\\:\\(\\)+\\?\\|]'

# I'm matching on the first 75% of the ref. if there are multiple versions
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
        compareTo,0,str_length(compareTo)*.75) )

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
      cleanRefs <- bind_rows(cleanRefs, refGroup)
    } else {
      refWorks$counted[i] <- TRUE #ignore in future groupings
    }
  } else {
    refWorks$counted[i] <- TRUE #ignore in future groupings
  }
}

# the moment of truth, whereby the less common forms of references in the
# actual reference list of coreDSEworks references are replaced with the
# most common form so citations are counted and networked properly.
for( i in 1:nrow(coreDSEworks) ) {
  thisRefsList <- as.data.frame(str_split(coreDSEworks$CR[i],"; "))
  colnames(thisRefsList) <- c("ref")

  thisRefsList[] <- cleanRefs$correctedCR[match(unlist(thisRefsList), cleanRefs$CR)]

  #typoRefs <- match(thisRefsList$ref,cleanRefs$CR)
  #thisRefsList$ref[!is.na(typoRefs)] <-
  # cleanRefs$correctedCR[na.omit(
  #   match(thisRefsList$ref,cleanRefs$CR))]
  # if this

  #thisRefsList$ref[thisRefsList$ref %in% cleanRefs$CR] <-

  #cleanRefs$correctedCR[cleanRefs$CR %in% thisRefsList$ref]

  #if( any(cleanRefs$CR==thisRefsList$ref) ) {
  #  thisRefsList$ref <- cleanRefs[cleanRefs$CR==thisRefsList$ref]$correctedCR
  #}
  coreDSEworks$CR[i] <- paste(thisRefsList$ref,collapse="; ")
  print(paste(i,"Replaced"))
}

# build co-citation network of DSE cited works
refMatrix <- biblioNetwork(coreDSEworks, analysis = "co-citation",
                           network = "references", sep = "; ")

# summary results of core DSE works
coreBibAnalysis <- biblioAnalysis(data.frame(coreDSEworks))
summary(coreBibAnalysis, max=10)

# Map the network featured in the proposal text.
# I set a cutoff to include only papers referenced 4 or more times in the
# network. The clusters, however, are robust to cutoff changes
cutoff = as.integer(count(refWorks %>% filter(Freq>2)))
refNet=networkPlot(refMatrix, n = cutoff,
                   Title = "Co-Citation Network of Top 100 Cited Papers",
                   size.cex=TRUE, size=15, remove.multiple=FALSE,
                   remove.isolates = TRUE, labelsize=.7, edgesize = 5,
                   edges.min=0, type = "fruchterman", cluster="louvain")
# louvain clustering seeks to "cut clusters at their joints"
net2VOSviewer(refNet,".")

# Let's check out the "brokers" in our reference network - that is, the papers
# that have been cited alongside works from otherwise distinct clusters.
brokers <- refNet[["cluster_res"]] %>% filter( btw_centrality >= 100 )

# Let's sort them by popularity
############ BROKEN
refWorks %>% filter(brokers %in% refWorks$CR)
############

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
  summarize(authors = list(word(vertex)))
