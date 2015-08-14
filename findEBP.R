library(RISmed)

tst <- freqItemsets
tst$items <- gsub("{|}","",tst$items, perl=T) # remove brackets
spl <- strsplit(as.character(tst$items), split = ",")
n <- max(sapply(spl, length))
l <- lapply(spl, function(X) c(X, rep(NA, n - length(X))))
d <- data.frame(t(do.call(cbind, l)))
d$search1 <- tolower(d$X1)
d$search2 <- tolower(d$X2)
d$search3 <- tolower(d$X3)
d$search4 <- tolower(d$X4)

# Use Systematic Review query logic
# http://www.nlm.nih.gov/bsd/pubmed_subsets/sysreviews_strategy.html

query <- paste0("(",d$search1," AND ",d$search2,") ",
               ifelse(is.na(d$search3),
                      yes = "",
                      no = paste("OR (",d$search1," AND ",d$search3,") ")),
               ifelse(is.na(d$search3),
                      yes = "",
                      no = paste("OR (",d$search2," AND ",d$search3,") ")),
               "AND systematic[sb] ") # ,"AND comorbidity[mh]"

d <- cbind(d,query)

d$link <- paste0("http://www.ncbi.nlm.nih.gov/pubmed/?term=",
                 gsub(pattern = " ", replacement = "+", x = d$query))

d <- cbind(freqItemsets,d)

d$guidelines <- QueryCount(EUtilsSummary(d$query,mindate=1980, maxdate=2015))

d %>% 
  mutate(ref = paste0("<a href=",link,">",items,"</a>"),
         pct = round(support * 100, digits = 1)) %>%
  select(ref,pct) %>%
  DT::datatable(rownames = FALSE,
                colnames = c('Multimorbidity Pattern', '% of Patients'),
                escape = FALSE)


ngs_search <- EUtilsSummary(q, 
                            type="esearch",
                            db = "pubmed",
                            mindate=1980, maxdate=2015)
QueryCount(ngs_search)
ngs_records <- EUtilsGet(ngs_search)

ngs_records <- as.data.frame(ngs_records)
class(ngs_records)

# store it
pubmed_data <- data.frame('PMID' = PMID(ngs_records),
                          'Title'=ArticleTitle(ngs_records),
                          'Abstract'=AbstractText(ngs_records))


pubmed_data$url <- paste("http://www.ncbi.nlm.nih.gov/pubmed/", pubmed_data$PMID, sep = "")
