
#rules <- apriori(myTrans, parameter=list(supp=.05,conf=.1,minlen=2))
oPut <- eclat(patientTrans, parameter=list(supp=.03,minlen=2,maxlen=25))
oPut <- sort(oPut, by="support", decreasing=TRUE)
freqItemsets <- as(oPut,"data.frame")
numItemsets = length(oPut)

indexer = data.frame()
iLen = 0
for (jj in (1:numItemsets)) {
  idxs <- unlist(strsplit(as.character(as(supportingTransactions(oPut[jj], patientTrans), "list")), '\", \n*\"'))
  idxsLen = length(idxs)
  iLen = iLen + 1
  indexer[iLen:(iLen+idxsLen-1), 1] = jj
  indexer[iLen, 2] = as.numeric(substr(idxs[1],6,nchar(idxs[1])))
  for (kk in 2:(idxsLen-1)) {
    iLen = iLen + 1
    indexer[iLen, 2] = as.numeric(substr(idxs[kk],3,nchar(idxs[kk])))
  }
  iLen = iLen + 1
  indexer[iLen, 2] = as.numeric(substr(idxs[idxsLen],3,nchar(idxs[idxsLen])-2))
}
colnames(indexer) = c("itemsetIdx", "patientIdx")

patientItemsets <- function(id=0, index=0) {
  if (id <= 0 && index <= 0) {
    print("Supply a patient id ('id' switch) or a patient index ('index' switch).")
    return
  }
  if (id > 0) {
    if (sum(pIDs==id) > 0) {
      thisPatIdx <- which(pIDs == id)
      patFreqItemsetIdxs <- subset(indexer, patientIdx == thisPatIdx)$itemsetIdx
      print(freqItemsets$items[patFreqItemsetIdxs])
    } else {
      print("No patient with this id appears to fit any frequent itemset.")
    }
  } else {
    if (index <= length(pIDs)) {
      patFreqItemsetIdxs <- subset(indexer, patientIdx == index)$itemsetIdx
      print(freqItemsets$items[patFreqItemsetIdxs])
    } else {
      print("There are not this many patients in the dataset.")
    }
  }
}

itemsetPatientIDs <- function(requestedItemsetIdx = 0) {
  if (requestedItemsetIdx <= 0) {
    print("You must supply an index for one of the frequent itemsets.")
    return ()
  } else if (requestedItemsetIdx > nrow(freqItemsets)) {
    print("There are not that many frequent itemsets.")
    return
  } else {
    pIdxsHavingItemset <- subset(indexer, itemsetIdx==requestedItemsetIdx)$patientIdx
    print(pIDs[pIdxsHavingItemset])
  }
}
