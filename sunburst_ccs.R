
# devtools::install_github("timelyportfolio/sunburstR")
library(magrittr)
library(sunburstR)

freqItemsets %>% 
  
  sunburst()

# remove brackets


# split itemsets
strsplit(as.character(freqItemsets$items), split = ",")
