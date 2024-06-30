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
source("clean_refs.R")

# Records last updated 6/14/24.
# Query inputted to Scopus and WoS were "data science education" in the
# title, abstract, or keywords of a record.
# Remove corrections, retractions, notes. Keep early access records.

coreDSEworks <- getCoreDSEWorks()

# get a data frame of the references.
refWorks <- as.data.frame(
  citations(coreDSEworks, field = "article", sep = ";")$Cited)

# NOTE: If cleaning breaks, your cleanRefs look wonky, or you find
# unreasonably similar records when perusing results, there is probably
# a new weird character to blame. Add it here.
charExcludeList <- '[\\:\\(\\)+\\?\\|\\"\\“\\”\\,]'

# The reference list has typos abd different ref styles that cause references
# to be incorrectly counted and mapped. Below I'm making a lookup
# table that maps all the less common (but matching) reference formats
# back to whatever the most popular form of the reference is.
cleanRefLookup <- mapCleanRefs(refWorks,charExcludeList)

# the moment of truth: the less common duplicates of references in the
# actual reference list of coreDSEworks are replaced with the
# most common form: so citations are aggregated, linked, and mapped properly.
for( i in 1:nrow(coreDSEworks) ) {
  # pull apart the refList for this core work into a dataframe
  thisRefsList <- as.data.frame(str_split(coreDSEworks$CR[i],"; "))
  colnames(thisRefsList) <- c("ref")
  thisRefsList$ref <- gsub(charExcludeList,'',thisRefsList$ref)

  # use cleanRefs lookup to replace duplicate records with main
  thisRefsList[] <- cleanRefLookup$correctedCR[match(unlist(thisRefsList), cleanRefLookup$CR)]

  # stitch it all back together and put it back in the core work
  coreDSEworks$CR[i] <- paste(thisRefsList$ref,collapse="; ")
}

# let's approximate the shortnames of the references so we can map
# them to the network analyses below. Note there will
# be duplicate shortnames for some records, but we're only interested in the
# most popular works. by matching to any record with this simplified shortname,
# i also catch lingering duplicate records for the more influential works
# shaping the field, which i add to the manual cleaning file over time.
shortname <- tolower(paste0(
  word(cleanRefLookup$correctedCR), " ",  # first word (last name)
  word(cleanRefLookup$correctedCR,2),     # second word (first initial)
  str_sub(cleanRefLookup$correctedCR,-5,-1))) # year
cleanRefLookup$shortname <- gsub(',','',shortname)

# clean up to get to the real work
rm(shortname,compareTo,i)

# build co-citation network of DSE cited works
refMatrix <- biblioNetwork(coreDSEworks, analysis = "co-citation",
                           network = "references", sep = "; ")

# summary results of core DSE works
coreBibAnalysis <- biblioAnalysis(data.frame(coreDSEworks))
summary(coreBibAnalysis, max=10)

# Map the network featured in the proposal text.
# I set a cutoff to include only papers referenced 3 or more times in the
# network. The clusters, however, are robust to cutoff changes
cutoff = as.integer(count(refWorks %>% filter(Freq>2)))
refNet=networkPlot(refMatrix, n = cutoff,
                   Title = "Co-Citation Network of Top DSE Reference Works",
                   size.cex=TRUE, size=15, remove.multiple=FALSE,
                   remove.isolates = TRUE, labelsize=.7, edgesize = 5,
                   edges.min=0, type = "fruchterman", cluster="louvain")
# louvain clustering seeks to "cut clusters at their joints"

# to reproduce the visualization featured in the proposal submission,
# set attraction=2, repulsion=0 and set size variation to 0 within VOSviewer
net2VOSviewer(refNet,".")

# Let's check out the top "brokers" in our reference network - that is,
# the papers that are cited alongside works from otherwise distinct clusters.
# string their shortname of the appended # to match to refWorks record.
# (if there are duplicates, can find out from popularity)
refBrokers <- refNet[["cluster_res"]] %>% 
  filter( btw_centrality >= 150 ) %>% 
  arrange(desc(btw_centrality))
refBrokers$vertex <- gsub("-[1-9]","",refBrokers$vertex )
refBrokers %>% head(10)

# Let's join to the rest of their ref records

brokersLookup <- unlist(paste0(refBrokers$vertex))
brokers <- cleanRefLookup %>%
  filter( correctedFreq > 0 ) %>% 
  filter( !is.na( shortname ) ) %>% 
  filter( shortname %in% refBrokers$vertex )
brokers$CR %>% head(10)

# Now, look for "core" cluster membership of the referenced papers. These are
# papers that have low betweenness centrality. Examining only these papers that
# are cited only with others in their community, but not more broadly, can
# help us identify what are thematic distinctions between clusters.

# get first author names of member authors for each cluster
referenceClusterAuthors <- refNet[["cluster_res"]] %>%
  # restrict this to only authors of papers that are not very connected
  # outside of their specific cluster
  filter( btw_centrality < 75 ) %>%
  group_by(cluster) %>%
  summarize(authors = list(vertex))

getClusterHub <- function(i) {
  clusterLookup <- referenceClusterAuthors$authors[[i]]
  return( cleanRefLookup %>%
    filter( correctedFreq > 0 ) %>% 
    filter( !is.na( shortname ) ) %>% 
    filter( shortname %in% clusterLookup ) %>%
    arrange( desc(correctedFreq) ) )
}

print("Cluster 1: Popular, insulated members")
getClusterHub(1) %>% head(5)

print("Cluster 2: Popular, insulated members")
getClusterHub(2) %>% head(5)

print("Cluster 3: Popular, insulated members")
getClusterHub(3) %>% head(5)