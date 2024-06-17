# Sneak peek code at a bibliometric analysis exploring the emergent literature
# on data science education.
# Michelle Hoda Wilkerson
# Jun 15, 2024

library(agop)
library(bibliometrix)
library(dplyr)
library(stringr)
library(textTools)

# Records last updated 6/14/24.
# Query inputted to Scopus and WoS were "data science education" in the
#   title, abstract, or keywords of a record.
# Removed corrections, retractions, notes. Kept early access records.

scopus <- convert2df(file = "./data/scopus.csv", dbsource = 'scopus',
                     format = "csv")
# 276 from Scopus
wos <- convert2df(file = "./data/wos.txt", dbsource = 'wos',
                  format = "plaintext")
# 170 from WoS

coreDSEworks <- mergeDbSources(scopus, wos)
rm(scopus, wos)
# cleaning and merging yields 293 records

# rm-list.txt identifies:
# records that are so incomplete that the text cannot be found;
# records that are duplicated (which breaks referenceMatrix construction); and
# records the represent full proceedings for conferences that were not
# primarily focused on data science education.
rmlist <- scan("rm-list.txt", what="", sep="\n")

# I identified rm-list members manually; if you want to check my work uncomment
# what's below and review the two CSV outputs. I could automate some of this
# with doi match but it's not reliable. Maybe build in as a step.
# write.csv(coreDSEworks[ coreDSEworks$UT %in% rmlist, ], "removedRefWorks.csv")
# write.csv(mergeDbSources(scopus, wos), "inspectWorks.csv")

coreDSEworks <- coreDSEworks[ ! coreDSEworks$UT %in% rmlist, ]
# 12 records removed; yields 281 records

# Some records (e.g. websites, reference works) do not have TI populated
# This duplicates the SO col to TI to make the record user-friendly
titlelist <- scan("title-list.txt", what="", sep="\n")
for(ut in titlelist) { coreDSEworks["TI"][coreDSEworks["UT"]==ut] <-
  coreDSEworks["SO"][coreDSEworks["UT"]==ut] }

rm(rmlist,titlelist,ut)

# summary results of core DSE works
coreBibAnalysis <- biblioAnalysis(data.frame(coreDSEworks))
summary(coreBibAnalysis, max=10)

# get a data frame of the references.
refWorks <- as.data.frame(
  citations(coreDSEworks, field = "article", sep = ";")$Cited)

# whoa nelly, the reference list has some disaster duplicates.
# we'll clear them out using bibliometix::duplicatedMatching.
# but to do that, we need a character vector. Let's keep both
# cols so we can check our work.
refWorks$charCR <- as.character(refWorks$CR)
# get the rows and columns of items that closely but not exactly match
matches <- as.data.frame(which(stringdist::stringdistmatrix(
  smallerTest[["CR"]], smallerTest[["CR"]]) < 5, arr.ind=TRUE)) %>%
  filter(row<col)

# the earlier the row, the more popular that version of the ref so let's call
# that main and all other versions dup
colnames(matches) <- c("main","dup")

# consolidate cites to the first instance and remove the later instance.
# maybe change names from row/col to main and dup?
for( item in 1:nrow(matches) ) {
  smallerTest$Freq[matches$main[item]] <-
    smallerTest$Freq[matches$main[item]] +
    smallerTest$Freq[matches$dup[item]]
}

# after the new frequency is calculated, you can get rid of the dup items

smallerTest$Freq[matches$main[item]]???


# make a matrix where nonzero values are very-near-matches
matches <- stringdist::stringdistmatrix(
  smallerTest[["CR"]],
  smallerTest[["CR"]])[stringdist::stringdistmatrix(
    smallerTest[["CR"]],
    smallerTest[["CR"]])<5]

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
