for (ii in 1:nrow(indexer)) {
  if (!is.subset(oPut[indexer[ii,1]],patientTrans[indexer[ii,2]])) print(ii)
}