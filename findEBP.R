library(RISmed)

# Use Systematic Review query logic
# http://www.nlm.nih.gov/bsd/pubmed_subsets/sysreviews_strategy.html

query <- "(bipolar AND depressive) 
          OR (depressive AND hypertension) 
          OR (bipolar AND hypertension) 
          AND systematic[sb] 
          AND comorbidity[mh]"

ngs_search <- EUtilsSummary(query, 
                            type="esearch",
                            db = "pubmed",
                            mindate=1980, maxdate=2015, 
                            retmax=30000)
QueryCount(ngs_search)
ngs_records <- EUtilsGet(ngs_search)

ngs_records <- as.data.frame(ngs_records)
class(ngs_records)

# store it
pubmed_data <- data.frame('PMID' = PMID(ngs_records),
                          'Title'=ArticleTitle(ngs_records),
                          'Abstract'=AbstractText(ngs_records))


pubmed_data$url <- paste("http://www.ncbi.nlm.nih.gov/pubmed/", pubmed_data$PMID, sep = "")
