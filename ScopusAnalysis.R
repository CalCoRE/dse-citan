### TODO: Would be great to dump all removed records into a trash
### dataframe for review/transparency.
### TODO: When narrowing focus to K-12, keywords include "higher education"
### "undergraduate"

# cleanup

rm(list = ls())
library(agop)
library(bibliometrix)
library(dplyr)
library(stringr)

# 6/14 Queries inputted to Scopus and WoS were "data science education".
# Removed all corrections, retracted, notes. Kept early access.
# 276 from Scopus
# 170 from WoS

scopus <- convert2df(file = "./data/scopus.csv", dbsource = 'scopus', format = "csv")
wos <- convert2df(file = "./data/wos.txt", dbsource = 'wos', format = "plaintext")

# cleaning and merging yields 293 records
cleanRecords <- mergeDbSources(scopus, wos)

## DATA CLEANING

# ### Next, remove incorrect, incomplete, or dupe records that I
# ### identified manually. Also remove full proceedings refs (whole book)
# ### I have extensive notes on these decisions
stoplist <- scan("rm-list.txt", what="", sep="\n")
cleanRecords <- cleanRecords[ ! cleanRecords$UT %in% stoplist, ]

### Take all these reports and other pubs that say they don't have a
### title and duplicate the SO col to TI for interpretability
titlelist <- scan("title-list.txt", what="", sep="\n")
for(ut in titlelist) { cleanRecords["TI"][cleanRecords["UT"]==ut] <- cleanRecords["SO"][cleanRecords["UT"]==ut] }

rm(mergedScopus, allRecords, stoplist)

# OUTPUT SUMMARY RESULTS
results <- biblioAnalysis(data.frame(cleanRecords))
summary(results, max=10)

# Reference co-citation network analysis
# # Co-citation links papers A and B that are both cited by the same paper C.
# # We can think of it as referencing the literature the focal works
# # are "building FROM"
# # This represents the networks of papers most cited by the papers in our dataset. In other words,
# # the network reflects the top papers cited by all papers in Scopus that are found on the "data science
# # education" keyword.
#
netMatrix <- biblioNetwork(cleanRecords, analysis = "co-citation", network = "references", sep = ";")
#net20=networkPlot(netMatrix, n = 20, Title = "Co-Citation Network of Top 20 Cited Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
#net2VOSviewer(net20,".")
#net50=networkPlot(netMatrix, n = 50, Title = "Co-Citation Network of Top 50 Cited Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
#net2VOSviewer(net50,".")
net100=networkPlot(netMatrix, n = 100, Title = "Co-Citation Network of Top 100 Cited Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.7, edgesize = 5, edges.min=0, type = "fruchterman")
net2VOSviewer(net100,".")
#net500=networkPlot(netMatrix, n = 500, Title = "Co-Citation Network of Top 500 Cited Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
#net2VOSviewer(net500,".")

# look for records with at least
myRefs <- cleanRecords %>%
  filter(str_count(CR, regex('marchionini|ortiz-repiso|', ignore_case = T))>0)
myRefs$FRC <- str_count(CR, regex('marchionini|ortiz-repiso', ignore_case = T))

# # Reference coupling network analysis
# # Coupling links the networks of papers that are citing the focal
# # collection of work.
# # We can think of this analysis as referencing the work that is
# # "building ON" the focal work.

couplingMatrix <- biblioNetwork(cleanRecords, analysis = "coupling", network = "references", sep = ";")
coupling20=networkPlot(couplingMatrix, n = 20, Title = "Coupling Network of Top 20 Citing Papers",  size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
net2VOSviewer(coupling20,".")
coupling50=networkPlot(couplingMatrix, n = 50, Title = "Co-Citation Network of Top 50 Citing Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
net2VOSviewer(coupling50,".")
coupling200=networkPlot(couplingMatrix, n = 200, Title = "Co-Citation Network of Top 200 Citing Papers",  size.cex=TRUE, size=10, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.2, edgesize = 5, edges.min=0, type = "fruchterman")
net2VOSviewer(coupling200,".")

# # Keyword analysis
keywordMatrix <- biblioNetwork(cleanRecords, analysis = "co-occurrences", network = "keywords", sep = ";")
keyword50=networkPlot(keywordMatrix, n = 50, Title = "Co-Citation Network of Top 50 Citing Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
#
# # Journal co-citation network analysis
# # This represents the top 15 publications (e.g. journals, books, reports) cited by
# # papers yielded in our Scopus search. Importantly, JLS is in the top 5.
# # NOTE extracting seems to be really broken. moving on.
#
extract=metaTagExtraction(data.frame(cleanRecords),"CR_SO",sep=";")
sources <- biblioNetwork(extract, analysis = "co-citation", network = "sources", sep = ";")
net=networkPlot(sources, n = 15, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=1,edgesize = 5, edges.min=2)

conceptualStructure(data.frame(cleanRecords))
#
# # historical analysis - does not seem to yield anything too interesting
#
# # Keyword analysis - does not seem to yield anything too interesting
#
# #keywordMatrix <- biblioNetwork(mergedScopus, analysis = "co-occurrences", network = "keywords", sep = ";")
# #net=networkPlot(keywordMatrix, normalize="association", n = 50, Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, labelsize=5,label.cex=TRUE,label.n=30,edges.min=2)
#
# # Author collaborations - not super useful because of large symposia
#
# #NetMatrix <- biblioNetwork(data.frame(cleanRecords), analysis = "collaboration",  network = "authors", sep = ";")
# #net=networkPlot(NetMatrix,  n = 50, Title = "Author collaboration",type = "auto", size=10,size.cex=T,edgesize = 3,labelsize=1)
