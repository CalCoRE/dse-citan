# Sneak peek code at a bibliometric analysis exploring the emergent literature
# on data science education.
# Michelle Hoda Wilkerson
# Jun 15, 2024

library(agop)
library(bibliometrix)
library(dplyr)
library(stringdist) # for reference cleanup
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

# The reference list has some disasterous typos that cause several citations
# to be missed or mis-mapped by bibliometrix. I cleared them out using
# stringdist to build a lookup table of typos back to the most frequently
# cited version of each reference. Computing the pairwise string distances
# of an 8.5K matrix of refs takes hours on a typical computer, so here I'm
# using a saved csv.

matches <- read.csv("matches5.csv")

# Uncomment the lines below to recalculate the matrix (for example, if you
# have updated your coreDSEworks).

# matches <- as.data.frame(which(stringdist::stringdistmatrix(
#   refWorks[["CR"]], refWorks[["CR"]]) < 5, arr.ind=TRUE)) %>%
#   filter(row<col)
# colnames(matches) <- c("main","dupe")

cleanRefsLookup <- data.frame()

# Consolidate citations from dups to the main - get it done way
for( i in 1:nrow(matches) ) {
  refWorks$Freq[matches$main[i]] <-
    refWorks$Freq[matches$main[i]] +
    refWorks$Freq[matches$dupe[i]]
  print(paste("Adding",refWorks$Freq[matches$dupe[i]],"to",refWorks$CR[matches$main[i]]))
  refWorks$Freq[matches$dupe[i]] <- 0
  cleanRefsLookup <- rbind(cleanRefsLookup,
                           data.frame(refWorks$CR[matches$main[i]],
                                      refWorks$CR[matches$dupe[i]]))
}

colnames(cleanRefsLookup) <- c("main","dupe")

# report how many dups and how many actual refs
paste( "I found", count(matches),
       "dupes which leaves", count(refWorks) - count(matches),
       "real refs" )

# and re-index the refWorks dataframe using the fixed reference counts
refWorks <- refWorks %>% arrange(desc(Freq))

for( i in i:nrow(coreDSEworks) ) {
  currentRefsList <- as.data.frame(str_split(coreDSEworks$CR[i],"; "))
  colnames(currentRefsList) <- c("ref")
  typoRefs <- match(currentRefsList$ref,cleanRefsLookup$dupe)
  currentRefsList$ref[!is.na(typoRefs)] <-
    cleanRefsLookup$main[na.omit(
      match(currentRefsList$ref,cleanRefsLookup$dupe))]
  coreDSEworks$CR[i] <- paste(currentRefsList$ref,collapse="; ")
  print(paste(i,"Replaced"))
}
# #make a df from the refs
# # NOTE currentRefsList is the thing I'd construct in the loop
# currentRefsList <- as.data.frame(str_split(coreDSEworks$CR[2],"; "))
# colnames(currentRefsList) <- c("ref")
#
# # replace dupes with mains
# typoRefs <- match(currentRefsList$ref,cleanRefsLookup$dupe)
# currentRefsList$ref[!is.na(typoRefs)] <- cleanRefsLookup$main[na.omit(match(currentRefsList$ref,cleanRefsLookup$dupe))]
#
# # put it back together
# coreDSEworks$CR[2] <- paste(currentRefsList$ref,collapse="; ")

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
refWorks %>% filter(brokers %in% refWorks$CR)

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
