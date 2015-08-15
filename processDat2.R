# User settings
supportThreshold = 0.03
itemsetMinLength = 2
itemsetMaxLength = 20
ccsLevelToCross = 1       # set to 0, 1 or 2

# Code to do processing of data
oPut <- eclat(patientTrans, parameter=list(supp=supportThreshold, minlen=itemsetMinLength, maxlen=itemsetMaxLength))
oPut <- sort(oPut, by="support", decreasing=TRUE)
freqItemsets <- as(oPut,"data.frame")
numItemsets = length(oPut)

elaborateItemset <- function(itemsetIdx) {
  if (itemsetIdx>nrow(freqItemsets) || itemsetIdx<1){
    print("supply a valid frequent itemset index.")
    return
  }
  cconds <- unlist(as(strsplit(as(freqItemsets$items[itemsetIdx], "character"),","),"vector"))
  cconds[1] <- substr(cconds[1],2,nchar(cconds[1]))
  arrLen = length(cconds)
  cconds[arrLen] <- substr(cconds[arrLen], 1, nchar(cconds[arrLen])-1)
  isetConds <- data.frame(
    lev3.desc = ccs3$cats[as.numeric(cconds)],
    lev3.idx  = as.numeric(cconds),
    lev2.idx  = ccs3$lev2.idx[as.numeric(cconds)]
  )
  isetConds$lev1.idx <- ccs2$lev1.idx[isetConds$lev2.idx]
  return (isetConds)
}

if (ccsLevelToCross==1 || ccsLevelToCross==2) {
  keptIsets = c()
  for (ii in 1:numItemsets) {
    isetDetails = elaborateItemset(ii)
    if (ccsLevelToCross==1) {
      if (length(unique(isetDetails$lev1.idx))>1) {
        keptIsets = c(keptIsets, ii)
      }
    } else {
      if (length(unique(isetDetails$lev2.idx))>1) {
        keptIsets = c(keptIsets, ii)
      }
    }
  }
  if (length(keptIsets) > 0) {
    nonCrossoverItemsets <- freqItemsets[-keptIsets,]
    freqItemsets <- freqItemsets[keptIsets,]
    numItemsets <- length(keptIsets)
  } else {
    nonCrossoverItemsets <- data.frame()
    print("There are no frequent itemsets which cross CCS levels as desired.  Keeping all.")
    keptIsets = c(1:numItemsets)
  }
} else {
  keptIsets = c(1:numItemsets)
}

indexer = data.frame()
iLen = 0
for (jj in (1:numItemsets)) {
  idxs <- unlist(strsplit(as.character(as(supportingTransactions(oPut[keptIsets[jj]], patientTrans), "list")), '\", \n*\"'))
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


# supporting functions
patientItemsets <- function(id=0, index=0) {
  index = floor(index)
  if (id <= 0 && index <= 0) {
    print("Supply a patient id ('id' switch) or a patient index ('index' switch).")
    return
  }
  if (id > 0) {
    if (sum(pIDs==id) > 0) {
      index <- which(pIDs == id)
    } else {
      print("No patient with this id fits any frequent itemset.")
      return ()
    }
  } else if (index > length(pIDs)) {
    print("There are not this many patients in the dataset.")
    return ()
  }
  patFreqItemsetIdxs <- subset(indexer, patientIdx == index)$itemsetIdx
  if (length(patFreqItemsetIdxs) > 1) {
    print(paste("This patient is in", length(patFreqItemsetIdxs), "frequent itemsets with indices", paste(patFreqItemsetIdxs,collapse=", "), ":"))
  } else {
    print(paste("This patient is in the frequent itemset with index", patFreqItemsetIdxs, ":"))
  }
  for (ii in patFreqItemsetIdxs) {
    print(elaborateItemset(ii)$lev3.desc)
  }
  return (patFreqItemsetIdxs)
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
    return (pIDs[pIdxsHavingItemset])
  }
}
