# Sneak peek code at a bibliometric analysis exploring the emergent literature
# on data science education.
# Michelle Hoda Wilkerson
# Jun 15, 2024

library(agop)
library(bibliometrix)
library(dplyr)
library(stringr)
library(textTools)

source("dse_citan.R")

# Records last updated 6/14/24.
# Query inputted to Scopus and WoS were "data science education" in the
# title, abstract, or keywords of a record.
# Removed corrections, retractions, notes. Kept early access records.

coreDSEworks <- getCoreDSEWorks()

# get a data frame of the references.
refWorks <- as.data.frame(
  citations(coreDSEworks, field = "article", sep = ";")$Cited)

# The reference list has typos that cause several references
# to be incorrectly counted and mapped.

# Here I use stringdist to build a lookup table of near-dupes.
# Computing the pairwise string distances
# of an 8.5K matrix of refs takes hours on a typical computer, so here I'm
# using a saved csv.

#matches <- read.csv("matches5.csv")

# NOTICE: Uncomment the lines below to reconstruct the matrix. It takes
# hours to run on a standard laptop... don't forget to save your new matches!
#matches <- as.data.frame(which(stringdist::stringdistmatrix(
#  refWorks[["CR"]], refWorks[["CR"]]) < 5, arr.ind=TRUE)) %>%
#  filter(row<col)

#colnames(matches) <- c("main","dupe")

cleanRefs <- as.data.frame(matrix(nrow=1,ncol=4))
colnames(cleanRefs) <- c("Freq", "CR", "correctedFreq", "correctedCR")
refWorks$counted <- FALSE

#note I'm matching on the first 80% of the ref. if there are multiple versions
# of something like a textbook, this aggregates them.
for( i in 1:nrow(refWorks) ) {

  # is there any meat to this entry
  compareTo <- gsub('[\\:\\(\\)+\\?\\|]','',refWorks$CR[i])

  if( str_length(compareTo) > 25) {

    # get the group of like refs that haven't already been captured
    refGroup <- refWorks %>% filter(counted==FALSE) %>%
      filter( gsub('[\\:\\(\\)+\\?\\|]','',CR) %like% substr(
        compareTo,0,str_length(compareTo)*.75) )
    print(i)

    # if this ref has any like entries let's consolidate them
    if( nrow(refGroup) > 0 ) {
      print(i)
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
      refWorks$counted[i] <- TRUE
    }
  } else {
    refWorks$counted[i] <- TRUE
  }
}



# matching faulty duplicates should be transitive, but is not yet:
# The matches table may identify B as a dupe of A and C as a dept of B, without
# a direct link between A and C. Below, I climb through parent/child pairs
# as trees to generate a list of transitively matching duplicates.
getDupes <- function(mainRefIndex,l=list()) {
  if( any(matches %>% filter(main == mainRefIndex ) ) ) {
    children <- (matches %>% filter(main == mainRefIndex))$dupe
    l <- lapply(children, getDupes)
  }
  return( unname(append(unlist(l),mainRefIndex)))
}

# There's another thing about matching. Conference proceedings in particular
# can be very different at the end. They might abbreviate venues, include
# location information, pages numbers, etc. See refWorks with indices
# 740-752. So here, we'll look for references that are only cited once or twice,
# and clump up those for whom the first 80% of the string matches. This won't
# catch anything but it will aggregate the most significant references to a
# level where final cleaning is done by hand.
stringsToCheck <- refWorks %>% filter(correctedFreq < 3)


# We only want the true parent ("root") of each collection of dupes.
# These are entries in matches where the "main" is not also a "dupe".
mainsOnly <- (matches %>% filter(!(main %in% matches$dupe)))$main
# Now, build a list of all the dupe collections.
alldupelists <- unique(lapply( mainsOnly, getDupes )) # get lists of dupe branches
# The lowest dupe index represents the most highly cited (or the first,
# alphaetically, in the  case of a tie) version from a dupe reference list.
rootRefs <- lapply( alldupelists, min )

## Finally, this function looks at each root, sums the # refs of all the
## children, and returns the "correct" (most used/earliest alphabetical)
## reference. I use this to construct a corrected lookup table.
aggSum <- function(myDupes) {
  sum <- sum(refWorks$Freq[id=myDupes])
  return( sum )
}

refWorks$correctedFreq <- refWorks$Freq
refWorks$correctedRef <- refWorks$CR

for( i in rootRefs ) { # for each
  myDupes <- unique(getDupes(i))
  refWorks$correctedFreq[id=myDupes] <- 0 # set corrected freq to 0
  refWorks$correctedFreq[id=i] <- aggSum(myDupes) # for all but parent
  refWorks$correctedRef[id=myDupes] <- as.character(refWorks$CR[i]) #set CR to parent
}

# report how many dups and how many actual refs
paste( "I found", count(matches),
       "dupes which leaves", count(refWorks) - count(matches),
       "real refs" )

# for( i in i:nrow(coreDSEworks) ) {
#   currentRefsList <- as.data.frame(str_split(coreDSEworks$CR[i],"; "))
#   colnames(currentRefsList) <- c("ref")
#   typoRefs <- match(currentRefsList$ref,cleanRefsLookup$dupe)
#   currentRefsList$ref[!is.na(typoRefs)] <-
#     cleanRefsLookup$main[na.omit(
#       match(currentRefsList$ref,cleanRefsLookup$dupe))]
#   coreDSEworks$CR[i] <- paste(currentRefsList$ref,collapse="; ")
#   print(paste(i,"Replaced"))
# }

# build co-citation network of DSE cited works
refMatrix <- biblioNetwork(coreDSEworks, analysis = "co-citation",
                           network = "references", sep = ";")

# Setting a cutoff to include only papers referenced 4 or more times in the
# network. The clusters hold steady with cutoffs down 2 or more times
cutoff = as.integer(count(refWorks %>% filter(Freq>3)))
refNet=networkPlot(refMatrix, n = cutoff,
                   Title = "Co-Citation Network of Top 100 Cited Papers",
                   size.cex=TRUE, size=15, remove.multiple=FALSE,
                   remove.isolates = TRUE, labelsize=.7, edgesize = 5,
                   edges.min=0, type = "fruchterman", cluster="louvain")
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
  summarize(authors = list(word(vertex))) #paste(word(vertex), collapse = "|"))

# Identify the top citers for each cluster of referenced works. Right now
# this is just looking at first author's name (duplicates are typically self-
# citations which are less useful to understand intellectual foundations).
cleanRecords$CL1 <- str_count(cleanRecords$CR,
                              regex(local_cluster_authors$vertex[1],
                                    ignore_case = T))
cleanRecords$CL2 <- str_count(cleanRecords$CR,
                              regex(local_cluster_authors$vertex[2],
                                    ignore_case = T))
cleanRecords$CL3 <- str_count(cleanRecords$CR,
                              regex(local_cluster_authors$vertex[3],
                                    ignore_case = T))
cleanRecords$CL4 <- str_count(cleanRecords$CR,
                              regex(local_cluster_authors$vertex[4],
                                    ignore_case = T))

print("TOP CITED PAPERS IN CLUSTER 1")
head(arrange(cleanRecords, desc(CL1)),10)[c("CL1")]
print("TOP CORE PAPERS CITING CLUSTER 2")
head(arrange(cleanRecords, desc(CL2), CL2),10)[c("CL2")]
print("TOP CORE PAPERS CITING CLUSTER 3")
head(arrange(cleanRecords, desc(CL3), CL3),10)[c("CL3")]
print("TOP CORE PAPERS CITING CLUSTER 4")
head(arrange(cleanRecords, desc(CL4), CL4),10)[c("CL4")]

cleanRecords$CLSFARD<- str_count(cleanRecords$CR,
                              regex('sfard',
                                    ignore_case = T))
cleanRecords$CLSBEIH<- str_count(cleanRecords$CR,
                                 regex('biehler',
                                       ignore_case = T))


# summary results of core DSE works
coreBibAnalysis <- biblioAnalysis(data.frame(coreDSEworks))
summary(coreBibAnalysis, max=10)
