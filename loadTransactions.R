require(arules)
tbdat <- read.csv("tbdatUpdated.csv")

# Remove bracketed CCS ID #s
tbdat$CCS.Level.1 <- as.factor(gsub("\\[[^\\]]+\\]","",
                                    tbdat$CCS.Level.1, perl=T))
tbdat$CCS.Level.2 <- as.factor(gsub("\\[[^\\]]+\\]","",
                                    tbdat$CCS.Level.2, perl=T))
tbdat$CCS.Level.3 <- as.factor(gsub("\\[[^\\]]+\\]","",
                                    tbdat$CCS.Level.3, perl=T))

# Remove leading and trailing whitespace
tbdat$CCS.Level.1 <- as.factor(gsub("^\\s+|\\s+$","",
                                    tbdat$CCS.Level.1, perl=T))
tbdat$CCS.Level.2 <- as.factor(gsub("^\\s+|\\s+$","",
                                    tbdat$CCS.Level.2, perl=T))
tbdat$CCS.Level.3 <- as.factor(gsub("^\\s+|\\s+$","",
                                    tbdat$CCS.Level.3, perl=T))

library(dplyr)
tbdat <- tbdat %>% filter(Chronic.Condition == "Y") %>% droplevels()

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
