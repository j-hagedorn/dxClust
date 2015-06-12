require(arules)
tbdat <- read.csv("~/tbdatUpdated.csv")
pIDs = sort(unique(tbdat$Patient.ID))
pMorbidityList = list()
pCount = 0
for (currID in pIDs) {
  illnesses = names(table(droplevels(tbdat$CCS.Level.3[tbdat$Patient.ID==currID])))
  pCount = pCount + 1
  pMorbidityList[[pCount]] = illnesses
}
names(pMorbidityList) <- paste("Tr",c(1:length(pIDs)), sep="")
patientTrans <- as(pMorbidityList, "transactions")
