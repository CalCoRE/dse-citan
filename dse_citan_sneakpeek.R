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
# cleaning and merging yields 293 records

# rm-list.txt identifies: records that are so incomplete that the text cannot
# be found; records that are duplicated; and records the represent full
# proceedings documents for conferences that were not primarily focused on
# data science education. I identified these manually.
stoplist <- scan("rm-list.txt", what="", sep="\n")
coreDSEworks <- coreDSEworks[ ! coreDSEworks$UT %in% stoplist, ]
# 8 records removed; yields 285 records

# Some records (e.g. websites, reference works) do not have TI populated
# This duplicates the SO col to TI to make the record user-friendly
titlelist <- scan("title-list.txt", what="", sep="\n")
for(ut in titlelist) { coreDSEworks["TI"][coreDSEworks["UT"]==ut] <-
  coreDSEworks["SO"][coreDSEworks["UT"]==ut] }

# summary results of core DSE works
coreBibAnalysis <- biblioAnalysis(data.frame(coreDSEworks))
summary(coreBibAnalysis, max=10)

# get a data frame of the references.
referenceWorks <- as.data.frame(citations(coreDSEworks, field = "article", sep = ";")$Cited)

# build co-citation network of DSE cited works
referenceMatrix <- biblioNetwork(coreDSEworks, analysis = "co-citation",
                           network = "references", sep = ";")

# set a cut off of number of cites for a paper to qualify as refNet member
cutoff = as.integer(count(referenceWorks %>% filter(Freq>3)))
refNet=networkPlot(referenceMatrix, n = 71,
                   Title = "Co-Citation Network of Top 100 Cited Papers",
                   size.cex=TRUE, size=15, remove.multiple=FALSE,
                   remove.isolates = TRUE, labelsize=.7, edgesize = 5,
                   edges.min=0, type = "fruchterman", cluster = "louvain")
net2VOSviewer(refNet,".")

# NOTE: Broker papers get clustered differently depending on the algorithm. This
# makes sense. Any paper that is clustered differently across networkPlot
# and net2VOSviewer will not be considered an "identifying paper" for
# its corresponding foundational collection.

# Find the records with the most name matches in the ref list.
# This is incomplete; ideally I'd use labelShort() and removeDuplicatedlabels()
# in biblioNetwork to look for closer matches. For now, this will signal the
# papers to pull and examine further but does not ensure exact ref matches.

# get first author names of member authors for each cluster
referenceClusterAuthors <- net100[["cluster_res"]] %>%
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
