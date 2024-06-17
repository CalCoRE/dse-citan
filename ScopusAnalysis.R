### TODO: Would be great to dump all removed records into a trash
### dataframe for review/transparency.
### TODO: When narrowing focus to K-12, keywords include "higher education"
### "undergraduate"

# cleanup

rm(list = ls())
library(agop)
library(bibliometrix)
library(dplyr)
library(here)
library(magrittr)
library(rscopus)
library(wosr)

## Use refresh_data this to update Scopus and WoS data.
## This will archive the current data files with today's date
## and generate new files with latest biblio info.

## You must have your own API keys. These are available through
## many exiting academic institutional lisences.
## Generate Scopus API key here: https://dev.elsevier.com/
## Generate WoS API key here: https://developer.clarivate.com/
## You may need to be on your institution's intranet when making the query.

  ## Thanks to https://aurelien-goutsmedt.com/post/extracting-biblio-data-1/
  # Note that you will need to be on your institution's network or see
  # instoken instructions

  ## Scopus API: You may, subject to the use restrictions set out below, for
  ## academic research purposes only:
  ## text and data mine the ScienceDirect dataset via the API and load and
  ## integrate the results (the TDM Output) on your internal system for access
  ## by authorized users; and
  ## share the TDM Output externally, subject to the following notice:
  ## © Some rights reserved. This work permits academic research purposes only,
  ## distribution, and reproduction in any medium, provided the original author
  ## and source are credited.

scopus_dse_papers <- data.frame()

refresh-scopus <- function(scopus_key) { # ,wos_key) {
  set_api_key("c4f594578d6489c29e1fb940d494e674")

  scopus_dse_query <- rscopus::scopus_search("TITLE-ABS-KEY(\"Data Science Education\")",
                                             view = "COMPLETE")
  scopus_dse_data_raw <- gen_entries_to_df(scopus_dse_query$entries)

  scopus_dse_papers <- scopus_dse_data_raw$df
  scopus_dse_affiliations <- scopus_dse_data_raw$affiliation
  scopus_dse_authors <- scopus_dse_data_raw$author

  ## Extract references. A loop because max 40 are extracted
  scopus_dse_paper_ids <- scopus_dse_papers$`dc:identifier` # extracting the IDs of our articles on dse
  scopus_citation_list <- list()

  for(i in 1:length(scopus_dse_paper_ids)){
    scopus_references <- abstract_retrieval(scopus_dse_paper_ids[i],
                                            identifier = "scopus_id",
                                            view = "REF")
    if(!is.null(scopus_references$content$`abstracts-retrieval-response`)){ # Checking if the article has some references before collecting them

      num_ref <- as.numeric(scopus_references$content$`abstracts-retrieval-response`$references$`@total-references`)
      scopus_citations <- gen_entries_to_df(scopus_citations_query$content$`abstracts-retrieval-response`$references$reference)$df

      if(num_ref > 40){ # The loop to collect all the references
        num_query_left_to_do <- floor((num_ref) / 40)
        cat("Number of requests left to do :", num_query_left_to_do, "\n")
        for (j in 1:nb_query_left_to_do){
          cat("Request n°", j , "\n")
          scopus_citations_query <- abstract_retrieval(citing_articles[i],
                                                       identifier = "scopus_id",
                                                       view = "REF",
                                                       startref = 40*j+1)
          scopus_citations_sup <- gen_entries_to_df(scopus_citations_query$content$`abstracts-retrieval-response`$references$reference)$df
          scopus_citations <- bind_rows(scopus_citations, scopus_citations_sup)
        }
      }

      citations <- citations %>%
        as_tibble(.name_repair = "unique") %>%
        select_if(~!all(is.na(.)))

      citation_list[[citing_articles[i]]] <- citations
    }
  }

  dse_references <- bind_rows(citation_list, .id = "citing_art")
}


#scopusPrimary <- convert2df(file = "scopusPrimary.csv", dbsource = 'scopus', format = "csv")
#scopusSecondary <- convert2df(file = "scopusSecondary.csv", dbsource = 'scopus', format = "csv")
#wos <- convert2df(file = "wos.txt", dbsource = 'wos', format = "plaintext")
  ## WoS did initially catch everything in JSDSE, so I then identified that the phrase should
  ## only be in the title, keywords, or abstract of the piece.

  # Load Scopus and WOS database records
###### MERGE ALL RECORDS

# # merging scopus is easy
# mergedScopus <- rbind(scopusPrimary, scopusSecondary)
#
# # figure out what columns in common for scopus and wos, then merge
# #commonCols <- intersect(names(mergedScopus), names(wos))
# #allRecords <- rbind(mergedScopus[c(commonCols)],wos[c(commonCols)])
# ### Remove wos version of duplicate records as determined by DOI
# ### When duplicates are found, retain Scopus records
# #cleanRecords <- allRecords %>% group_by(DI) %>% slice_max(factor(DB, c('SCOPUS','ISI')))
#
# # Or, have bibliometrix do it for me. (I checked, it's mostly fine
# # and in many ways better.)
# cleanRecords <- mergeDbSources(mergedScopus, wos)
#
# # DATA CLEANING
#
# ### Next, remove incorrect, incomplete, or dupe records that I
# ### identified manually. Also remove full proceedings refs (whole book)
# ### I have extensive notes on these decisions
# stoplist <- scan("rm-list.txt", what="", sep="\n")
# cleanRecords <- cleanRecords[ ! cleanRecords$UT %in% stoplist, ]
#
# ### Take all these reports and other pubs that say they don't have a
# ### title and duplicate the SO col to TI for interpretability
# #titlelist <- scan("title-list.txt", what="", sep="\n")
# #for(ut in titlelist) { cleanRecords["TI"][cleanRecords["UT"]==ut] <- cleanRecords["SO"][cleanRecords["UT"]==ut] }
#
# # CLEAN UP
#
# rm(mergedScopus, allRecords, stoplist)
#
# # OUTPUT SUMMARY RESULTS
#
# results <- biblioAnalysis(data.frame(cleanRecords))
# summary(results, max=10)
#
# # Reference co-citation network analysis
# # Co-citation links papers A and B that are both cited by the same paper C.
# # We can think of it as referencing the literature the focal works
# # are "building FROM"
# # This represents the networks of papers most cited by the papers in our dataset. In other words,
# # the network reflects the top papers cited by all papers in Scopus that are found on the "data science
# # education" keyword.
#
# netMatrix <- biblioNetwork(cleanRecords, analysis = "co-citation", network = "references", sep = ";")
# net20=networkPlot(netMatrix, n = 20, Title = "Co-Citation Network of Top 20 Cited Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
# net2VOSviewer(net20,".")
# net50=networkPlot(netMatrix, n = 50, Title = "Co-Citation Network of Top 50 Cited Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
# net2VOSviewer(net50,".")
# net200=networkPlot(netMatrix, n = 200, Title = "Co-Citation Network of Top 200 Cited Papers",  size.cex=TRUE, size=10, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.2, edgesize = 5, edges.min=0, type = "fruchterman")
# net2VOSviewer(net200,".")
# net500=networkPlot(netMatrix, n = 500, Title = "Co-Citation Network of Top 500 Cited Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
# net2VOSviewer(net500,".")
#
# # Reference coupling network analysis
# # Coupling links the networks of papers that are citing the focal
# # collection of work.
# # We can think of this analysis as referencing the work that is
# # "building ON" the focal work.
#
# couplingMatrix <- biblioNetwork(cleanRecords, analysis = "coupling", network = "references", sep = ";")
# coupling20=networkPlot(couplingMatrix, n = 20, Title = "Coupling Network of Top 20 Citing Papers",  size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
# net2VOSviewer(coupling20,".")
# coupling50=networkPlot(couplingMatrix, n = 50, Title = "Co-Citation Network of Top 50 Citing Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
# net2VOSviewer(coupling50,".")
# coupling200=networkPlot(couplingMatrix, n = 200, Title = "Co-Citation Network of Top 200 Citing Papers",  size.cex=TRUE, size=10, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.2, edgesize = 5, edges.min=0, type = "fruchterman")
# net2VOSviewer(coupling200,".")
#
# # Keyword analysis
# keywordMatrix <- biblioNetwork(cleanRecords, analysis = "co-occurrences", network = "keywords", sep = ";")
# keyword50=networkPlot(keywordMatrix, n = 50, Title = "Co-Citation Network of Top 50 Citing Papers",  size.cex=TRUE, size=15, remove.multiple=FALSE, remove.isolates = TRUE, labelsize=.5,edgesize = 5, edges.min=0, type = "fruchterman")
#
# # Journal co-citation network analysis
# # This represents the top 15 publications (e.g. journals, books, reports) cited by
# # papers yielded in our Scopus search. Importantly, JLS is in the top 5.
# # NOTE extracting seems to be really broken. moving on.
#
# #extract=metaTagExtraction(data.frame(cleanRecords),"CR_SO",sep=";")
# #sources <- biblioNetwork(extract, analysis = "co-citation", network = "sources", sep = ";")
# #net=networkPlot(sources, n = 15, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=1,edgesize = 5, edges.min=2)
#
# #conceptualStructure(data.frame(cleanRecords))
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
