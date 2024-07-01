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

# for each distinct shortname - holding off for now, but may be needed to
# link mutiple author-year refs
# parentRefShortnames <- as.data.frame(cleanRefLookup %>% distinct(shortname))
# # note this is a great place to identify lingering duplicates, check
# # especially for excessive repeats
# for( i in 1:nrow(parentRefShortnames) ) {
#   # get all nonzero-after-correction records with this shortname
#   records <- cleanRefLookup %>% 
#     filter(shortname %in% parentRefShortnames$shortname[i] ) %>% 
#     filter(correctedFreq > 0)
#   # if there's more than one record, append numbers to identify each
#   if( count( records ) > 1 ) {
#     indices <- which(
#       cleanRefLookup$shortname == parentRefShortnames$shortname[i] &
#         cleanRefLookup$correctedFreq > 0, arr.ind = TRUE)
#     append = 1
#     for( index in indices ) {
#       print(index)
#       cleanRefLookup[index,]$shortname <- paste0( 
#         cleanRefLookup$shortname[index] , "-", append )
#       append <- append + 1
#     }
#   }
# }

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

# clean up to get to the real work
rm(shortname,i,index, indices,records,parentRefShortnames)

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
                   edges.min=0, type = "fruchterman", cluster="louvain")
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
brokers <- cleanRefLookup %>%
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
  return( cleanRefLookup %>%
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
  coreDSEworks$refInfo[i] <- paste(refNet[["cluster_res"]] %>% 
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