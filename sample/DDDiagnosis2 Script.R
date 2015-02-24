#Loading packages
install.packages ("Hmisc")
library(Hmisc)
install.packages("foreign")
library(foreign)
install.packages("psych")
library(psych)
install.packages ("gmodels")
library(gmodels)

#Import/View data
DDDiagnosis2 <- read.delim(".DDDiagnosis2.txt", dec=",")
View(DDDiagnosis2)
attach(DDDiagnosis2)

#Structure of data set
str(DDDiagnosis2)

#Descriptives
#Number of individuals in the data set
nrow(DDDiagnosis2)

str(DDDiagnosis2$Age)
#Descriptives for quantitative variable Age
describe(DDDiagnosis2$Age)

#Age demographics -- creating AgeGroup variable
DDDiagnosis2$AgeGroup[DDDiagnosis2$Age <= 10] <- "10 years or younger"
DDDiagnosis2$AgeGroup[DDDiagnosis2$Age >= 11 & DDDiagnosis2$Age <= 20] <- "11 - 20 years"
DDDiagnosis2$AgeGroup[DDDiagnosis2$Age >= 21 & DDDiagnosis2$Age <= 30] <- "21 - 30 years"
DDDiagnosis2$AgeGroup[DDDiagnosis2$Age >= 31 & DDDiagnosis2$Age <= 40] <- "31 - 40 years"
DDDiagnosis2$AgeGroup[DDDiagnosis2$Age >= 41 & DDDiagnosis2$Age <= 50] <- "41 - 50 years"
DDDiagnosis2$AgeGroup[DDDiagnosis2$Age >= 51 & DDDiagnosis2$Age <= 60] <- "51 - 60 years"
DDDiagnosis2$AgeGroup[DDDiagnosis2$Age >= 61 & DDDiagnosis2$Age <= 70] <- "61 - 70 years"
DDDiagnosis2$AgeGroup[DDDiagnosis2$Age >= 71 & DDDiagnosis2$Age <= 80] <- "71 - 80 years"
DDDiagnosis2$AgeGroup[DDDiagnosis2$Age >= 81] <- "81 years or older"

AgeData <- data.frame(DDDiagnosis2$Age, DDDiagnosis2$AgeGroup)
View(AgeData)

table(DDDiagnosis2$AgeGroup)
AgeGrouptbl <- table(DDDiagnosis2$AgeGroup)
write.table(AgeGrouptbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/AgeGrouptbl2", sep="\t")

#Frequency of individuals within each program
Programtbl <- table(DDDiagnosis2$Program)
write.table(Programtbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/Programtbl2", sep="\t")
sum(is.na(DDDiagnosis2[,4]))

#Frequency of individuals within each service line
ServiceLinetbl <- table(DDDiagnosis2$ServiceLine)
write.table(ServiceLinetbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/ServiceLinetbl2", sep="\t")
sum(is.na(DDDiagnosis2[,6]))

#Finding how many unique Level Descriptions there are (note: LVL22 was last level with 2 unique categories)
unique(DDDiagnosis2$LVL1Desc_22)
unique(DDDiagnosis2$LVL2Desc_22)
unique(DDDiagnosis2$LVL3Desc_22)

#Restructuring data so that ICD9-Code, diagnosis, ProblemStatus, and LVL1Desc-LVL4Desc are stacked in a single column
#Creating data frames of each variable of interest [ICD9-Code, diagnosis, and LVL1Desc-LVL4Desc]
#Converting data frames above to matrices and then to vectors to be stacked
ICD9Code <- data.frame(ICD9Code_1,ICD9Code_2,ICD9Code_3,ICD9Code_4,ICD9Code_5,ICD9Code_6,ICD9Code_7,ICD9Code_8,ICD9Code_9,ICD9Code_10,ICD9Code_11,ICD9Code_12,ICD9Code_13,ICD9Code_14,ICD9Code_15,ICD9Code_16,ICD9Code_17,ICD9Code_18,ICD9Code_19,ICD9Code_20,ICD9Code_21,ICD9Code_22)
ICD9Code <- as.matrix(ICD9Code)
ICD9Code <- as.vector(ICD9Code)
#View(ICD9Code)

diagnosis <- data.frame(diagnosis_1,diagnosis_2,diagnosis_3,diagnosis_4,diagnosis_5,diagnosis_6,diagnosis_7,diagnosis_8,diagnosis_9,diagnosis_10,diagnosis_11,diagnosis_12,diagnosis_13,diagnosis_14,diagnosis_15,diagnosis_16,diagnosis_17,diagnosis_18,diagnosis_19,diagnosis_20,diagnosis_21,diagnosis_22)
diagnosis <- as.matrix(diagnosis)
diagnosis <- as.vector(diagnosis)
#View(diagnosis)

LVL1Desc <- data.frame(LVL1Desc_1,LVL1Desc_2,LVL1Desc_3,LVL1Desc_4,LVL1Desc_5,LVL1Desc_6,LVL1Desc_7,LVL1Desc_8,LVL1Desc_9,LVL1Desc_10,LVL1Desc_11,LVL1Desc_12,LVL1Desc_13,LVL1Desc_14,LVL1Desc_15,LVL1Desc_16,LVL1Desc_17,LVL1Desc_18,LVL1Desc_19,LVL1Desc_20,LVL1Desc_21,LVL1Desc_22)
LVL1Desc <- as.matrix(LVL1Desc)
LVL1Desc <- as.vector(LVL1Desc)
#View(LVL1Desc)

LVL2Desc <- data.frame(LVL2Desc_1,LVL2Desc_2,LVL2Desc_3,LVL2Desc_4,LVL2Desc_5,LVL2Desc_6,LVL2Desc_7,LVL2Desc_8,LVL2Desc_9,LVL2Desc_10,LVL2Desc_11,LVL2Desc_12,LVL2Desc_13,LVL2Desc_14,LVL2Desc_15,LVL2Desc_16,LVL2Desc_17,LVL2Desc_18,LVL2Desc_19,LVL2Desc_20,LVL2Desc_21,LVL2Desc_22)
LVL2Desc <- as.matrix(LVL2Desc)
LVL2Desc <- as.vector(LVL2Desc)
#View(LVL2Desc)

LVL3Desc <- data.frame(LVL3Desc_1,LVL3Desc_2,LVL3Desc_3,LVL3Desc_4,LVL3Desc_5,LVL3Desc_6,LVL3Desc_7,LVL3Desc_8,LVL3Desc_9,LVL3Desc_10,LVL3Desc_11,LVL3Desc_12,LVL3Desc_13,LVL3Desc_14,LVL3Desc_15,LVL3Desc_16,LVL3Desc_17,LVL3Desc_18,LVL3Desc_19,LVL3Desc_20,LVL3Desc_21,LVL3Desc_22)
LVL3Desc <- as.matrix(LVL3Desc)
LVL3Desc <- as.vector(LVL3Desc)
#View(LVL3Desc)

LVL4Desc <- data.frame(LVL4Desc_1,LVL4Desc_2,LVL4Desc_3,LVL4Desc_4,LVL4Desc_5,LVL4Desc_6,LVL4Desc_7,LVL4Desc_8,LVL4Desc_9,LVL4Desc_10,LVL4Desc_11,LVL4Desc_12,LVL4Desc_13,LVL4Desc_14,LVL4Desc_15,LVL4Desc_16,LVL4Desc_17,LVL4Desc_18,LVL4Desc_19,LVL4Desc_20,LVL4Desc_21,LVL4Desc_22)
LVL4Desc <- as.matrix(LVL4Desc)
LVL4Desc <- as.vector(LVL4Desc)
#View(LVL4Desc)

CCICatDesc <- data.frame(CCICatDesc_1,CCICatDesc_2,CCICatDesc_3,CCICatDesc_4,CCICatDesc_5,CCICatDesc_6,CCICatDesc_7,CCICatDesc_8,CCICatDesc_9,CCICatDesc_10,CCICatDesc_11,CCICatDesc_12,CCICatDesc_13,CCICatDesc_14,CCICatDesc_15,CCICatDesc_16,CCICatDesc_17,CCICatDesc_18,CCICatDesc_19,CCICatDesc_20,CCICatDesc_21,CCICatDesc_22)
CCICatDesc <- as.matrix(CCICatDesc)
CCICatDesc <- as.vector(CCICatDesc)

#Creating new data set "Stacked" by merging vectors created above
Stacked = data.frame(ServiceLine, ICD9Code, diagnosis, LVL1Desc, LVL2Desc, LVL3Desc, LVL4Desc, CCICatDesc)
str(Stacked)
View(Stacked)

#Finding how many unique levels there are within each LVL1-LVL4 description
unique(Stacked$LVL1Desc)
LVL1DescTbl <- table(Stacked$LVL1Desc)
write.table(LVL1DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/LVL1DescTbl2", sep="\t")

#Looking at subset of ONLY Mental Illness LVL1 Diagnoses
#MI <- subset(Stacked, LVL1Desc=="Mental Illness")
#View(MI)

#Additional subsets of LVL1 Diagnoses
Lvl1Diag <- subset(Stacked, LVL1Desc=="Diseases of the nervous system and sense organs")
View(Lvl1Diag)

unique(Stacked$LVL2Desc)
LVL2DescTbl <- table(Stacked$LVL2Desc)
write.table(LVL2DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/LVL2DescTbl2", sep="\t")

#Additional subsets of LVL2 Diagnoses
#Lvl2Diag <- subset(Stacked, LVL2Desc=="Epilepsy; convulsions [83.]")
#View(Lvl2Diag)

unique(Stacked$LVL3Desc)
LVL3DescTbl <- table(Stacked$LVL3Desc)
write.table(LVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/LVL3DescTbl2", sep="\t")

#unique(Stacked$LVL4Desc)
#LVL4DescTbl <- table(Stacked$LVL4Desc)
#write.table(LVL4DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/LVL4DescTbl2", sep="\t")

#Additional subsets of LVL4 Diagnoses
#Lvl4Diag <- subset(Stacked, LVL4Desc=="Other and unspecified genitourinary symptoms")
#View(Lvl4Diag)

#unique(Stacked$diagnosis)
#diagnosisTbl <- table(Stacked$diagnosis)
#write.table(diagnosisTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/diagnosisTbl2", sep="\t")

#CCICatDescTbl <- table(Stacked$CCICatDesc)
#write.table(CCICatDescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/CCICatDescTbl2", sep="\t")
#unique(Stacked$CCICatDescTbl)

#Creating subsets to look at ALL diagnosis within Service Line
DCS <- subset(Stacked, ServiceLine == "Developmental and Community Services")
View(DCS)
unique(DCS$LVL3Desc)
DCSLVL3DescTbl <- table(DCS$LVL3Desc)
write.table(DCSLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/DCSLVL3DescTbl2", sep="\t")

BHSE <- subset(Stacked, ServiceLine == "Behavioral Health Services (East)")
View(BHSE)
unique(BHSE$LVL3Desc)
BHSELVL3DescTbl <- table(BHSE$LVL3Desc)
write.table(BHSELVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/BHSELVL3DescTbl2", sep="\t")

Rehab <- subset(Stacked, ServiceLine == "Rehabilitation Services")
View(Rehab)
unique(Rehab$LVL3Desc)
RehabLVL3DescTbl <- table(Rehab$LVL3Desc)
write.table(RehabLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/RehabLVL3DescTbl2", sep="\t")

BHSW <- subset(Stacked, ServiceLine == "Behavioral Health Services (West)")
View(BHSW)
unique(BHSW$LVL3Desc)
BHSWLVL3DescTbl <- table(BHSW$LVL3Desc)
write.table(BHSWLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/BHSWLVL3DescTbl2", sep="\t")

#HNCorp <- subset(Stacked, ServiceLine == "HN Services Corporation")
#View(HNCorp)
#unique(HNCorp$LVL3Desc)
#HNCorpLVL3DescTbl <- table(HNCorp$LVL3Desc)
#write.table(HNCorpLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/HNCorpLVL3DescTbl2", sep="\t")

#Missing <- subset(Stacked, ServiceLine == "NULL")
#View(Missing)
#unique(Missing$LVL3Desc)
#MissingLVL3DescTbl <- table(Missing$LVL3Desc)
#write.table(MissingLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/MissingLVL3DescTbl2", sep="\t")

#Looking ONLY at Development Disability Diagnoses
#Creating three subsets defined by the inclusion criteria
DDOnly1 <- subset(Stacked, ICD9Code==343)
DDOnly2 <- subset(Stacked, LVL2Desc %in% c('Developmental disorders [654]', 'Disorders usually diagnosed in infancy childhood or adolescence [655]'))
View(DDOnly2)
DDOnly3 <- subset(Stacked, LVL3Desc=='Downs Syndrome')

#Creating one data set [DDOnly] from the three subsets above [DDOnly1, DDOnly2, DDOnly3]
DDOnly <- rbind(DDOnly1, DDOnly2, DDOnly3)
View(DDOnly)

#Looking at LVL3Desc within ONLY DD diagnoses
unique(DDOnly$LVL3Desc)
DDOnlyLVL3DescTbl <- table(DDOnly$LVL3Desc)
write.table(DDOnlyLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/DDOnlyLVL3DescTbl2", sep="\t")

#Creating subsets to look at ONLY DD diagnosis within Service Line
DCS_DD <- subset(DDOnly, ServiceLine == "Developmental and Community Services")
View(DCS_DD)
unique(DCS_DD$LVL3Desc)
DCS_DDLVL3DescTbl <- table(DCS_DD$LVL3Desc)
write.table(DCS_DDLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/DCS_DDLVL3DescTbl2", sep="\t")

BHSE_DD <- subset(DDOnly, ServiceLine == "Behavioral Health Services (East)")
View(BHSE_DD)
unique(BHSE_DD$LVL3Desc)
BHSE_DDLVL3DescTbl <- table(BHSE_DD$LVL3Desc)
write.table(BHSE_DDLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/BHSE_DDLVL3DescTbl2", sep="\t")

Rehab_DD <- subset(DDOnly, ServiceLine == "Rehabilitation Services")
View(Rehab_DD)
unique(Rehab_DD$LVL3Desc)
Rehab_DDLVL3DescTbl <- table(Rehab_DD$LVL3Desc)
write.table(Rehab_DDLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/Rehab_DDLVL3DescTbl2", sep="\t")

BHSW_DD <- subset(DDOnly, ServiceLine == "Behavioral Health Services (West)")
View(BHSW_DD)
unique(BHSW_DD$LVL3Desc)
BHSW_DDLVL3DescTbl <- table(BHSW_DD$LVL3Desc)
write.table(BHSW_DDLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/BHSW_DDLVL3DescTbl2", sep="\t")


#Looking at comorbid diagnosis EXCLUDING DD diagnoses
ExcludeDD1 <- subset(Stacked,(!(LVL2Desc%in%c('Developmental disorders [654]','Disorders usually diagnosed in infancy childhood or adolescence [655]'))))
ExcludeDD2 <- subset(ExcludeDD1, ICD9Code!=343)
ExcludeDD <- subset(ExcludeDD2, LVL3Desc!='Downs Syndrome')

#Looking at LVL3Desc within comorbid diagnosis EXCLUDING DD diagnoses
unique(ExcludeDD$LVL3Desc)
ExcludeDDLVL3DescTbl <- table(ExcludeDD$LVL3Desc)
write.table(ExcludeDDLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/ExcludeDDLVL3DescTbl2", sep="\t")

#Creating subsets to look at ONLY Comorbid diagnosis within Service Line
DCS_CO <- subset(ExcludeDD, ServiceLine == "Developmental and Community Services")
View(DCS_CO)
unique(DCS_CO$LVL3Desc)
DCS_COLVL3DescTbl <- table(DCS_CO$LVL3Desc)
write.table(DCS_COLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/DCS_COLVL3DescTbl2", sep="\t")

BHSE_CO <- subset(ExcludeDD, ServiceLine == "Behavioral Health Services (East)")
View(BHSE_CO)
unique(BHSE_CO$LVL3Desc)
BHSE_COLVL3DescTbl <- table(BHSE_CO$LVL3Desc)
write.table(BHSE_COLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/BHSE_COLVL3DescTbl2", sep="\t")

Rehab_CO <- subset(ExcludeDD, ServiceLine == "Rehabilitation Services")
View(Rehab_CO)
unique(Rehab_CO$LVL3Desc)
Rehab_COLVL3DescTbl <- table(Rehab_CO$LVL3Desc)
write.table(Rehab_COLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/Rehab_COLVL3DescTbl2", sep="\t")

BHSW_CO <- subset(ExcludeDD, ServiceLine == "Behavioral Health Services (West)")
View(BHSW_CO)
unique(BHSW_CO$LVL3Desc)
BHSW_COLVL3DescTbl <- table(BHSW_CO$LVL3Desc)
write.table(BHSW_COLVL3DescTbl, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/BHSW_COLVL3DescTbl2", sep="\t")


#Creating new data set with just CCI codes
CCIcode <- data.frame(DDDiagnosis2$AgeGroup,CCICatDesc_1,CCICatDesc_2,CCICatDesc_3,CCICatDesc_4,CCICatDesc_5,CCICatDesc_6,CCICatDesc_7,CCICatDesc_8,CCICatDesc_9,CCICatDesc_10,CCICatDesc_11,CCICatDesc_12,CCICatDesc_13,CCICatDesc_14,CCICatDesc_15,CCICatDesc_16,CCICatDesc_17,CCICatDesc_18,CCICatDesc_19,CCICatDesc_20,CCICatDesc_21,CCICatDesc_22)
View(CCIcode)
str(CCIcode)

#Creating new variable that counts number of chronic conditions per person
CCIcode$CCI1 <- rowSums(CCIcode[c(1:22)]==1, na.rm=TRUE)

#Creating new variable that counts number of non chronic conditions per person
CCIcode$CCI0 <- rowSums(CCIcode[c(1:22)]==0, na.rm=TRUE)
View(CCIcode)

#Creating new variable that counts number of both chronic and non chronic condidtions per person
#as.numeric(as.character(CCIcode$CCI1))
#str(CCIcode$CCI1)
#as.numeric(as.character(CCIcode$CCI0))
#str(CCIcode$CCI0)
CCIcode$CCI10 <- rowSums(CCIcode[c(24:25)], na.rm=TRUE)
View(CCIcode)

#Number of total chronic and non-chronic conditions for DD Population
sum(CCIcode$CCI1)
sum(CCIcode$CCI0)
sum(CCIcode$CCI10)

describe(CCIcode$CCI1)
describe(CCIcode$CCI0)
describe(CCIcode$CCI10)

#How many individuals within the population have greater than two, three, four, five, six chronic conditions?
sum(CCIcode$CCI1>2)
sum(CCIcode$CCI1>3)
sum(CCIcode$CCI1>4)
sum(CCIcode$CCI1>5)
sum(CCIcode$CCI1>6)

#Average number of chronic conditions per age group
str(CCIcode$DDDiagnosis2.AgeGroup)
AgeGroupL <- list(CCIcode$DDDiagnosis2.AgeGroup)
View(AgeGroupL)
aggregate(CCIcode$CCI1, by=(AgeGroupL), mean, na.rm=TRUE)

#Average number of non chronic conditions per age group
aggregate(CCIcode$CCI0, by=(AgeGroupL), mean, na.rm=TRUE)

detach(DDDiagnosis2)