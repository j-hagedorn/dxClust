
# devtools::install_github("timelyportfolio/sunburstR")
library(magrittr)
library(sunburstR)

tst2 <- read.csv("https://gist.githubusercontent.com/kerryrodden/7090426/raw/34749be0e1d9dc31ce4ec5be95e61c7380925608/visit-sequences.csv",
                header = F)

tst <- freqItemsets
tst$items <- gsub("{|}| ","",tst$items, perl=T)
tst$items <- gsub(",","-",tst$items, perl=T)
tst$items <- tolower(tst$items)

tst$patients <- tst$support * pCount

tst %<>% select(items, patients)

freqItemsets %>% 
  
  sunburst(tst)

# remove brackets


# split itemsets
strsplit(as.character(freqItemsets$items), split = ",")
