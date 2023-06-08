### TODO: Would be great to dump all removed records into a trash
### dataframe for review/transparency.
### TODO: When narrowing focus to K-12, keywords include "higher education"
### "undergraduate"

# GET READY TO RUMBLE

rm(list = ls())
library(agop)
library(bibliometrix)
library(dplyr)

# PULL IN OUR DATA
## Scopus I just did a search with quoted phrase. Did not seem to pick up on journal titles
## but I should go back to make sure.
## WoS did initially catch everything in JSDSE, so I then identified that the phrase should
## only be in the title, keywords, or abstract of the piece.

# Load Scopus and WOS database records

scopusPrimary <- convert2df(file = "scopusPrimary.csv", dbsource = 'scopus', format = "csv")
scopusSecondary <- convert2df(file = "scopusSecondary.csv", dbsource = 'scopus', format = "csv")
wos <- convert2df(file = "wos.txt", dbsource = 'wos', format = "plaintext")

###### MERGE ALL RECORDS

# merging scopus is easy
mergedScopus <- rbind(scopusPrimary, scopusSecondary)

# figure out what columns in common for scopus and wos, then merge
commonCols <- intersect(names(mergedScopus), names(wos))
allRecords <- rbind(mergedScopus[c(commonCols)],wos[c(commonCols)])
#cleanRecords <- mergeDbSources(mergedScopus, wos)

# DATA CLEANING

### Remove wos version of duplicate records as determined by DOI
### When duplicates are found, retain Scopus records
cleanRecords <- allRecords %>% group_by(DI) %>% slice_max(factor(DB, c('SCOPUS','ISI')))

### Next, remove incorrect, incomplete, or dupe records that I
### identified manually. Also remove full proceedings refs (whole book)
### I have extensive notes on these decisions
stoplist <- scan("rm-list.txt", what="", sep="\n")
cleanRecords <- cleanRecords[ ! cleanRecords$UT %in% stoplist, ]

### Take all these reports and other pubs that say they don't have a
### title and duplicate the SO col to TI for interpretability
#titlelist <- scan("title-list.txt", what="", sep="\n")
#for(ut in titlelist) { cleanRecords["TI"][cleanRecords["UT"]==ut] <- cleanRecords["SO"][cleanRecords["UT"]==ut] }

# CLEAN UP

rm(mergedScopus, allRecords, stoplist)

# OUTPUT SUMMARY RESULTS

results <- biblioAnalysis(as.data.frame(cleanRecords))
summary(results, max=10)

# Reference co-citation network analysis
# Co-citation links papers A and B that are both cited by the same paper C.
# This represents the networks of papers most cited by the papers in our dataset. In other words,
# the network reflects the top papers cited by all papers in Scopus that are found on the "data science
# education" keyword.

netMatrix <- biblioNetwork(as.data.frame(cleanRecords), analysis = "co-citation", network = "references", sep = ";")
net20=networkPlot(netMatrix, n = 20, Title = "Co-Citation Network of Top 20 Cited Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
net50=networkPlot(netMatrix, n = 50, Title = "Co-Citation Network of Top 50 Cited Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
net200=networkPlot(netMatrix, n = 200, Title = "Co-Citation Network of Top 200 Cited Papers",  size.cex=TRUE, size=10, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.2, edgesize = 5, edges.min=0, type = "fruchterman")
net500=networkPlot(netMatrix, n = 500, Title = "Co-Citation Network of Top 500 Cited Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")

# Journal co-citation network analysis
# This represents the top 15 publications (e.g. journals, books, reports) cited by
# papers yielded in our Scopus search. Importantly, JLS is in the top 5.
# NOTE extracting seems to be really broken. moving on.

extract=metaTagExtraction(data.frame(cleanRecords),"CR_SO",sep=";")
sources <- biblioNetwork(extract, analysis = "co-citation", network = "sources", sep = ";")
net=networkPlot(sources, n = 15, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=1,edgesize = 5, edges.min=2)

conceptualStructure(data.frame(cleanRecords))

# historical analysis - does not seem to yield anything too interesting

# Keyword analysis - does not seem to yield anything too interesting

keywordMatrix <- biblioNetwork(mergedScopus, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(keywordMatrix, normalize="association", n = 50, Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, labelsize=5,label.cex=TRUE,label.n=30,edges.min=2)

# Author collaborations - not super useful because of large symposia

NetMatrix <- biblioNetwork(data.frame(cleanRecords), analysis = "collaboration",  network = "authors", sep = ";")
net=networkPlot(NetMatrix,  n = 50, Title = "Author collaboration",type = "auto", size=10,size.cex=T,edgesize = 3,labelsize=1)

histResults <- histNetwork(as.data.frame(cleanRecords), sep = ";")
plot(histResults)
