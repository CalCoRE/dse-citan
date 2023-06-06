# DATA CLEANING
## Look for names that need consolidating
## Look for changes in journal names (e.g. Journal of Statistics Education became the Journal of Statistics and Data Science Education)

# PROCESS
## Scopus I just did a search with quoted phrase. Did not seem to pick up on journal titles
## but I should go back to make sure.
## WoS did catch everything in JSDSE, so I then identified that the phrase should
## only be in the title, keywords, or abstract of the piece.

# Load Scopus and WOS database records

scopusPrimary <- convert2df(file = "scopusPrimary.csv", dbsource = 'scopus', format = "csv")
scopusSecondary <- convert2df(file = "scopusSecondary.csv", dbsource = 'scopus', format = "csv")
wos <- convert2df(file = "wos.txt", dbsource = 'wos', format = "plaintext")

# merge scopus records, removing dupes

mergedScopus <- unique.data.frame(rbind(scopusPrimary, scopusSecondary))
scopusBiblio <- biblioAnalysis(mergedScopus)

# output summary results

summary(scopusBiblio, max=5)
plot(scopusBiblio)

# Reference co-citation network analysis
# This represents the networks of papers most cited by the papers in our dataset. In other words,
# the network reflects the top papers cited by all papers in Scopus that are found on the "data science
# education" keyword.

ScopusRefNetMatrix <- biblioNetwork(mergedScopus, analysis = "co-citation", network = "references", sep = ";")
net20=networkPlot(ScopusNetMatrix, n = 20, Title = "Co-Citation Network",  size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 5, edges.min=2)
net50=networkPlot(ScopusNetMatrix, n = 50, Title = "Co-Citation Network",  size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 5, edges.min=2)
net75=networkPlot(ScopusNetMatrix, n = 75, Title = "Co-Citation Network",  size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 5, edges.min=2)

# Journal co-citation network analysis
# This represents the top 15 publications (e.g. journals, books, reports) cited by
# papers yielded in our Scopus search. Importantly, JLS is in the top 5.

extract=metaTagExtraction(mergedScopus,"CR_SO",sep=";")
ScopusSourceNetMatrix <- biblioNetwork(extract, analysis = "co-citation", network = "sources", sep = ";")
net=networkPlot(ScopusSourceNetMatrix, n = 15, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=1,edgesize = 5, edges.min=2)

# historical analysis - does not seem to yield anything too interesting

# Keyword analysis - does not seem to yield anything too interesting

#keywordMatrix <- biblioNetwork(mergedScopus, analysis = "co-occurrences", network = "keywords", sep = ";")
#net=networkPlot(keywordMatrix, normalize="association", n = 50, Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, labelsize=5,label.cex=TRUE,label.n=30,edges.min=2)

# Thematic Map and cluster info based on keywords
Map=thematicMap(mergedScopus, field = "ID", n = 250, minfreq = 4, stemming = FALSE, size = 0.7, n.labels=5, repel = TRUE)
plot(Map$map)
Clusters=Map$words[order(Map$words$Cluster,-Map$words$Occurrences),]
library(dplyr)
CL <- Clusters %>% group_by(.data$Cluster_Label) %>% top_n(5, .data$Occurrences)
print(CL,n=40)

# Author collaborations - not super useful because of large symposia

#NetMatrix <- biblioNetwork(mergedScopus, analysis = "collaboration",  network = "authors", sep = ";")
#net=networkPlot(NetMatrix,  n = 50, Title = "Author collaboration",type = "auto", size=10,size.cex=T,edgesize = 3,labelsize=1)
