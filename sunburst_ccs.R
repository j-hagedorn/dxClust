
# sunburst(csvdata = read.csv(file = "https://gist.githubusercontent.com/mkajava/7515402/raw/9f80d28094dc9dfed7090f8fb3376ef1539f4fd2/comment-sequences.csv",
#                              header = FALSE ,stringsAsFactors = FALSE))

# tst2 <- read.csv("https://gist.githubusercontent.com/kerryrodden/7090426/raw/34749be0e1d9dc31ce4ec5be95e61c7380925608/visit-sequences.csv",
#                  header = F)

# devtools::install_github("timelyportfolio/sunburstR")
library(dplyr)
library(magrittr)
library(sunburstR)

# Clean and format data
  tst <- freqItemsets
  tst$items <- gsub("{|}| ","",tst$items, perl=T) # remove brackets
  spl <- strsplit(as.character(tst$items), split = ",")
  n <- max(sapply(spl, length))
  l <- lapply(spl, function(X) c(X, rep(NA, n - length(X))))
  d <- data.frame(t(do.call(cbind, l)))
  d$X1 <- stringr::str_sub(d$X1, start = 1, end = 13)
  d$X2 <- stringr::str_sub(d$X2, start = 1, end = 13)
  d$X3 <- stringr::str_sub(d$X3, start = 1, end = 13)
  d$X4 <- stringr::str_sub(d$X4, start = 1, end = 13)
  # Remove NAs from 3rd dx position before pasting
  d$paths <- ifelse(!is.na(d$X3), 
                    paste(d$X1,d$X2,d$X3,sep = "-"), 
                    paste(d$X1,d$X2,sep = "-"))
  # Remove NAs from 4th dx position before pasting
  d$paths <- ifelse(!is.na(d$X4), 
                    paste(d$paths,d$X4,sep = "-"), 
                    d$paths)
  tst <- cbind(freqItemsets,d)
  tst$paths <- tolower(tst$paths)
  tst$patients <- tst$support * pCount
  tst$patients <- as.integer(tst$patients)
  tst$paths <- as.factor(tst$paths)
  tst %<>% select(paths, counts = patients)

#write.table(tst, "tst.csv", quote = F, sep = ",", row.names = F)

#sunburst(csvdata = read.csv("tst.csv", header = F, stringsAsFactors = F))

sunburst(tst)



