# Bibliometric analysis exploring what papers that describe themselves as
# "data science education" cite as references. This is designed to be run from
# top to bottom.

# Michelle Hoda Wilkerson
# updated Sep 6, 2024

setwd("~/Documents/GitHub/dse-citan")

#library(agop)
library(dplyr) 
library(stringr) # for cleaning and matching duplicate references
library(textTools) # for matching duplicate references
library(bibliometrix)
#library(visNetwork)

# this script expects the working directory to be this source file's location
source("scripts/read_works.R")
source("scripts/clean_refs.R")
source("scripts/shortnames.R")

# Records last updated 6/14/24.
# Query inputted to Scopus and WoS were "data science education" in the
# title, abstract, or keywords of a record.
# Remove corrections, retractions, notes. Keep early access records.

coreDSEworks <- getCoreDSEWorks()
coreDSEworks$CR <- coreDSEworks$CR_raw

# get a data frame of the references.
refWorks <- as.data.frame(
  citations(coreDSEworks, field = "article", sep = ";")$Cited)

# The reference list has typos and different ref styles that cause references
# to be incorrectly counted and mapped. Below I'm making a lookup
# table that maps all the less common (but matching) reference formats
# back to whatever the most popular form of the reference is.
charExcludeList <- '[\\:\\(\\)+\\?\\|\\"\\“\\”\\,\'\\`\\‘\\.\\*]'
refWorks$CR <- gsub(charExcludeList,'',refWorks$CR) # the original cited works
refWorks <- mapCleanRefs(refWorks)
#cleanRefLookup <- read.csv("data/Jul30refworks.csv")

# Replace refs list in the core works with cleaned refs
coreDSEworks <- cleanRefs(coreDSEworks)

# create shortnames and add them as a new ref list to coreDSEworks
refWorks <- refShortNames(refWorks)
coreDSEworks <- coreShortNames(coreDSEworks,refWorks)


###################### PART 1 ##########################
##                                                    ##
##            The basic bibliometrics                 ##
##                                                    ##
########################################################
# summary results of core DSE works
coreBibAnalysis <- biblioAnalysis(data.frame(coreDSEworks))
summary(coreBibAnalysis, max=10)

###################### PART 2a #########################
##                                                    ##
##            Construct the ref network               ##
##                                                    ##
########################################################

# build co-citation network of DSE cited works
refMatrix <- biblioNetwork(coreDSEworks, analysis = "co-citation",
                           network = "references", sep = "; ")

# Map the network featured in the proposal text.
# I set a cutoff to include only papers referenced 3 or more times in the
# network. The clusters, however, are robust to cutoff changes
cutoff = as.integer(count(refWorks %>% filter(Freq>2)))
refNet=networkPlot(refMatrix, n = cutoff,
                   Title = "Co-Citation Network of Top DSE Reference Works",
                   size.cex=TRUE, size=15, remove.multiple=FALSE,
                   remove.isolates = TRUE, labelsize=.7, edgesize = 5,
                   edges.min=0, type = "fruchterman", cluster = "louvain")
# louvain clustering seeks to "cut clusters at their joints"

# to reproduce the visualization featured in the proposal submission,
# set attraction=2, repulsion=0 and set size variation to 0 within VOSviewer
net2VOSviewer(refNet,".")

###################### PART 2b #########################
##                                                    ##
##            Identify reference brokers              ##
##                                                    ##
########################################################

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
brokers <- refWorks %>%
  filter( correctedFreq > 0 ) %>% 
  filter( !is.na( shortname ) ) %>% 
  filter( shortname %in% refBrokers$vertex )
brokers$CR %>% head(10)

###################### PART 2c #########################
##                                                    ##
##              Identify reference hubs               ##
##                                                    ##
########################################################

# Now, look for "core" cluster membership of the referenced papers. These are
# papers that have low betweenness centrality. Examining only these papers that
# are cited only with others in their community, but not more broadly, can
# help us identify what are thematic distinctions between clusters.

# get first author names of member authors for each cluster
referenceClusterAuthors <- refNet[["cluster_res"]] %>%
  # restrict this to only authors of papers that are not very connected
  # outside of their specific cluster
  filter( btw_centrality < 50 ) %>%
  group_by(cluster) %>%
  summarize(authors = list(vertex))

getClusterHub <- function(i) {
  clusterLookup <- referenceClusterAuthors$authors[[i]]
  return( refWorks %>%
    filter( correctedFreq > 0 ) %>% 
    filter( !is.na( shortname ) ) %>% 
    filter( shortname %in% clusterLookup ) %>%
    arrange( desc(correctedFreq) ) )
}

for( i in 1:nrow(distinct(refNet[["cluster_res"]], cluster)) ) {
  print(paste("Cluster", i, "Popular, insulated members") )
  hubs <- getClusterHub(i) 
  print( hubs %>% head(5) %>% select(correctedFreq, correctedCR) )
}

###################### PART 2d #########################
##                                                    ##
## Identify core works that reference across clusters ##
##                                                    ##
########################################################

## for each coreDSEwork, identify which cluster each
## paper they are referencing is from. ignore reference
## works with very high betweenness centrality (those are)
## the ones that are often reference with others anyway

## The below code returns all the refNet info for mapped refs. Keeping
## it here in case it becomes useful. Instead, I'm just gonna return a
## list of the clusters to determine each core work's dependence on
## each cluster.

# i can probably do this without looping with an apply of some kind...
for( i in 1:nrow(coreDSEworks) ) {
  coreDSEworks$refInfo[i] <- list(refNet[["cluster_res"]] %>% 
       filter( str_detect(vertex, coreDSEworks$shortnameRefs[i]) == TRUE ) %>%
    select(cluster))
}

# OK! We have a list of the refnet clusters cited by each paper, for each of
# that paper's references. (remember that as written, refnet only includes the
# highest cited papers, so there are lots of nulls too.) Now we can find out
# which papers cite broadly (evenly across clusters) versus which are more 
# insular (citing only from one cluster).

# for our hub and broker analyses, let's just look at the papers that are 
# citing at least 15 works within the refnet. Again, remember that refnet 
# does not include all documented references unless you adjusted it above. 
# if you did adjust it, you may want to adjust these selection methods.

coreCitingWorks <- coreDSEworks %>% 
  filter( length(coreDSEworks$refInfo) > 15 )

# next up: identify works that cite all three. My guess is the records
# will be sparse enough that for now, I can eyeball from there.
            

###################### PART 2e #########################
##                                                    ##
## Identify core works that reference within clusters ##
##                                                    ##
########################################################

# list by desc percent of items in each cluster

