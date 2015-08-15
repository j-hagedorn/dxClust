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
ccs1 = data.frame(cats=names(table(tbdat$CCS.Level.1)))
ccs2 = data.frame(cats=names(table(tbdat$CCS.Level.2)))
ccs3 = data.frame(cats=names(table(tbdat$CCS.Level.3)))

new.CCS.Level.1 = c(1:length(ccs1$cats))
for (ii in 1:length(ccs1$cats)) {
  matches = (tbdat$CCS.Level.1==ccs1$cats[ii])
  new.CCS.Level.1[matches] = ii
}
tbdat$CCS.Level.1 = factor(new.CCS.Level.1)

new.CCS.Level.2 = c(1:length(ccs2$cats))
lev1 = c(1:length(ccs2$cats))
for (ii in 1:length(ccs2$cats)) {
  matches = (tbdat$CCS.Level.2==ccs2$cats[ii])
  new.CCS.Level.2[matches] = ii
  lev1[ii] = tbdat$CCS.Level.1[min(which(matches))]
}
tbdat$CCS.Level.2 = factor(new.CCS.Level.2)
ccs2$lev1.idx = as.numeric(lev1)

new.CCS.Level.3 = c(1:length(ccs3$cats))
lev2 = c(1:length(ccs3$cats))
for (ii in 1:length(ccs3$cats)) {
  matches = (tbdat$CCS.Level.3==ccs3$cats[ii])
  new.CCS.Level.3[matches] = ii
  lev2[ii] = tbdat$CCS.Level.2[min(which(matches))]
}
tbdat$CCS.Level.3 = factor(new.CCS.Level.3)
ccs3$lev2.idx = as.numeric(lev2)

pIDs = sort(unique(tbdat$Patient.ID))
pMorbidityList = list()
pCount = 0
for (currID in pIDs) {
  illnesses1 = names(table(droplevels(tbdat$CCS.Level.1[tbdat$Patient.ID==currID])))
  illnesses2 = names(table(droplevels(tbdat$CCS.Level.2[tbdat$Patient.ID==currID])))
  illnesses3 = names(table(droplevels(tbdat$CCS.Level.3[tbdat$Patient.ID==currID])))
  pCount = pCount + 1
  
  # Next line uses illnesses1 to flag patients by CCS.Level.1
  # Change to illnesses2 to flag patients by CCS.Level.2
  # Change to illnesses3 to flag patients by CCS.Level.3
  pMorbidityList[[pCount]] = illnesses3
}

names(pMorbidityList) <- paste("Tr",c(1:length(pIDs)), sep="")
patientTrans <- as(pMorbidityList, "transactions")
