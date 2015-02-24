#########################
## INSTALLING PACKAGES ##
#########################

#install.packages("plyr") ## For ddply
library("plyr")
#install.packages("psych") ## For describe function
library("psych")
#install.packages("ggplot2") ## For graphics
library("ggplot2")

####################
## IMPORTING DATA ##
####################

# TBI includes all diagnoses for consumers who had AT LEAST one of the following diagnoses:
# Acute cerebrovascular disease [109.] (LVL3)
# Late effects of cerebrovascular disease [113.] (LVL3)
# Intracranial injury [233.] (LVL2)
# Spinal cord injury [227.] (LVL2)
# with a problemStatus = 'Active' Note: co-morbid diagnoses may have a different problemStatus

TBI <- read.delim(".Data/TBI Diagnoses.txt", strip.white=TRUE)


unique(TBI$Client_ID) # (n=1,144)

#######################################
## DEMOGRAPHICS/GENERAL DESCRIPTIVES ##
#######################################

## Age (quantitative)
sum(is.na(TBI$Age)) # (n=0)
describe(TBI$Age)

## Age (categorical)
TBI$AgeGroup[TBI$Age <= 10] <- "10 years or younger"
TBI$AgeGroup[TBI$Age >= 11 & TBI$Age <= 20] <- "11 - 20 years"
TBI$AgeGroup[TBI$Age >= 21 & TBI$Age <= 30] <- "21 - 30 years"
TBI$AgeGroup[TBI$Age >= 31 & TBI$Age <= 40] <- "31 - 40 years"
TBI$AgeGroup[TBI$Age >= 41 & TBI$Age <= 50] <- "41 - 50 years"
TBI$AgeGroup[TBI$Age >= 51 & TBI$Age <= 60] <- "51 - 60 years"
TBI$AgeGroup[TBI$Age >= 61 & TBI$Age <= 70] <- "61 - 70 years"
TBI$AgeGroup[TBI$Age >= 71 & TBI$Age <= 80] <- "71 - 80 years"
TBI$AgeGroup[TBI$Age >= 81] <- "81 years or older"


AgeGroup <- data.frame(table(TBI$AgeGroup))
AgeGroup$Percent <- (AgeGroup$Freq/((sum(AgeGroup$Freq))+(sum(is.na(TBI$AgeGroup)))))*100
AgeGroup

## Company
sum(is.na(TBI$Company)) # (n=0)
Company <- data.frame(table(TBI$Company))
Company$Percent <- (Company$Freq/((sum(Company$Freq))+(sum(is.na(TBI$Company)))))*100
Company <- Company[order(Company$Freq),] # Frequencies in ascending order
Company

## Service Line - All Rehabilitation Services
sum(is.na(TBI$ServiceLine)) # (n=0)
ServiceLine <- data.frame(table(TBI$ServiceLine))
ServiceLine$Percent <- (ServiceLine$Freq/((sum(ServiceLine$Freq))+(sum(is.na(TBI$ServiceLine)))))*100
ServiceLine <- ServiceLine[order(ServiceLine$Freq),] # Frequencies in ascending order
ServiceLine

## Program
sum(is.na(TBI$Program)) # (n=0)
Program <- data.frame(table(TBI$Program))
Program$Percent <- (Program$Freq/((sum(Program$Freq))+(sum(is.na(TBI$Program)))))*100
Program <- Program[order(Program$Freq),] # Frequencies in ascending order
Program

############################
## DIAGNOSIS DESCRIPTIVES ##
############################

unique(TBI$ICD9Code_33) ## The most diagnoses for a single consumer
unique(TBI$diagnosis_33)

TBI <- subset(TBI[,1:303]) # Subsetting TBI to remove diagnosis fieldsin positions 34 and greater (all NA)

ICD9Code <- data.frame(TBI$ICD9Code_1,TBI$ICD9Code_2,TBI$ICD9Code_3,TBI$ICD9Code_4,TBI$ICD9Code_5,TBI$ICD9Code_6,TBI$ICD9Code_7,TBI$ICD9Code_8,
                      TBI$ICD9Code_9,TBI$ICD9Code_10,TBI$ICD9Code_11,TBI$ICD9Code_12,TBI$ICD9Code_13,TBI$ICD9Code_14,TBI$ICD9Code_15,
                      TBI$ICD9Code_16,TBI$ICD9Code_17,TBI$ICD9Code_18,TBI$ICD9Code_19,TBI$ICD9Code_20,TBI$ICD9Code_21,TBI$ICD9Code_22,TBI$ICD9Code_23,
                      TBI$ICD9Code_24,TBI$ICD9Code_25,TBI$ICD9Code_26,TBI$ICD9Code_27,TBI$ICD9Code_28,TBI$ICD9Code_29,TBI$ICD9Code_30,TBI$ICD9Code_31,
                      TBI$ICD9Code_32,TBI$ICD9Code_33)
ICD9Code <- as.matrix(ICD9Code)
ICD9Code <- as.vector(ICD9Code)
write.table(ICD9Code, "T:/Projects/Active/TBI Population Analysis/R Output/ICD9Code", sep="\t")

diagnosis <- data.frame(TBI$diagnosis_1,TBI$diagnosis_2,TBI$diagnosis_3,TBI$diagnosis_4,TBI$diagnosis_5,TBI$diagnosis_6,TBI$diagnosis_7,TBI$diagnosis_8,
                        TBI$diagnosis_9,TBI$diagnosis_10,TBI$diagnosis_11,TBI$diagnosis_12,TBI$diagnosis_13,TBI$diagnosis_14,
                        TBI$diagnosis_15,TBI$diagnosis_16,TBI$diagnosis_17,TBI$diagnosis_18,TBI$diagnosis_19,TBI$diagnosis_20,TBI$diagnosis_21,
                        TBI$diagnosis_22,TBI$diagnosis_23,TBI$diagnosis_24,TBI$diagnosis_25,TBI$diagnosis_26,TBI$diagnosis_27,TBI$diagnosis_28,
                        TBI$diagnosis_29,TBI$diagnosis_30,TBI$diagnosis_31,TBI$diagnosis_32,TBI$diagnosis_33)
diagnosis <- as.matrix(diagnosis)
diagnosis <- as.vector(diagnosis)

problemStatus <- data.frame(TBI$problemStatus_1,TBI$problemStatus_2,TBI$problemStatus_3,TBI$problemStatus_4,TBI$problemStatus_5,TBI$problemStatus_6,
                            TBI$problemStatus_7,TBI$problemStatus_8,TBI$problemStatus_9,TBI$problemStatus_10,TBI$problemStatus_11,TBI$problemStatus_12,
                            TBI$problemStatus_13,TBI$problemStatus_14,TBI$problemStatus_15,TBI$problemStatus_16,
                            TBI$problemStatus_17,TBI$problemStatus_18,TBI$problemStatus_19,TBI$problemStatus_20,TBI$problemStatus_21,
                            TBI$problemStatus_22,TBI$problemStatus_23,TBI$problemStatus_24,TBI$problemStatus_25,TBI$problemStatus_26,
                            TBI$problemStatus_27,TBI$problemStatus_28,TBI$problemStatus_29,TBI$problemStatus_30,TBI$problemStatus_31,
                            TBI$problemStatus_32,TBI$problemStatus_33)
problemStatus <- as.matrix(problemStatus)
problemStatus <- as.vector(problemStatus)

LVL1Desc <- data.frame(TBI$LVL1Desc_1,TBI$LVL1Desc_2,TBI$LVL1Desc_3,TBI$LVL1Desc_4,TBI$LVL1Desc_5,TBI$LVL1Desc_6,
                         TBI$LVL1Desc_7,TBI$LVL1Desc_8,TBI$LVL1Desc_9,TBI$LVL1Desc_10,TBI$LVL1Desc_11,TBI$LVL1Desc_12,
                         TBI$LVL1Desc_13,TBI$LVL1Desc_14,TBI$LVL1Desc_15,TBI$LVL1Desc_16,
                         TBI$LVL1Desc_17,TBI$LVL1Desc_18,TBI$LVL1Desc_19,TBI$LVL1Desc_20,TBI$LVL1Desc_21,
                         TBI$LVL1Desc_22,TBI$LVL1Desc_23,TBI$LVL1Desc_24,TBI$LVL1Desc_25,TBI$LVL1Desc_26,
                         TBI$LVL1Desc_27,TBI$LVL1Desc_28,TBI$LVL1Desc_29,TBI$LVL1Desc_30,TBI$LVL1Desc_31,
                         TBI$LVL1Desc_32,TBI$LVL1Desc_33)
LVL1Desc <- as.matrix(LVL1Desc)
LVL1Desc <- as.vector(LVL1Desc)

LVL2Desc <- data.frame(TBI$LVL2Desc_1,TBI$LVL2Desc_2,TBI$LVL2Desc_3,TBI$LVL2Desc_4,TBI$LVL2Desc_5,TBI$LVL2Desc_6,
                          TBI$LVL2Desc_7,TBI$LVL2Desc_8,TBI$LVL2Desc_9,TBI$LVL2Desc_10,TBI$LVL2Desc_11,TBI$LVL2Desc_12,
                          TBI$LVL2Desc_13,TBI$LVL2Desc_14,TBI$LVL2Desc_15,TBI$LVL2Desc_16,
                          TBI$LVL2Desc_17,TBI$LVL2Desc_18,TBI$LVL2Desc_19,TBI$LVL2Desc_20,TBI$LVL2Desc_21,
                          TBI$LVL2Desc_22,TBI$LVL2Desc_23,TBI$LVL2Desc_24,TBI$LVL2Desc_25,TBI$LVL2Desc_26,
                          TBI$LVL2Desc_27,TBI$LVL2Desc_28,TBI$LVL2Desc_29,TBI$LVL2Desc_30,TBI$LVL2Desc_31,
                          TBI$LVL2Desc_32,TBI$LVL2Desc_33)
LVL2Desc <- as.matrix(LVL2Desc)
LVL2Desc <- as.vector(LVL2Desc)

LVL3Desc <- data.frame(TBI$LVL3Desc_1,TBI$LVL3Desc_2,TBI$LVL3Desc_3,TBI$LVL3Desc_4,TBI$LVL3Desc_5,TBI$LVL3Desc_6,
                       TBI$LVL3Desc_7,TBI$LVL3Desc_8,TBI$LVL3Desc_9,TBI$LVL3Desc_10,TBI$LVL3Desc_11,TBI$LVL3Desc_12,
                       TBI$LVL3Desc_13,TBI$LVL3Desc_14,TBI$LVL3Desc_15,TBI$LVL3Desc_16,
                       TBI$LVL3Desc_17,TBI$LVL3Desc_18,TBI$LVL3Desc_19,TBI$LVL3Desc_20,TBI$LVL3Desc_21,
                       TBI$LVL3Desc_22,TBI$LVL3Desc_23,TBI$LVL3Desc_24,TBI$LVL3Desc_25,TBI$LVL3Desc_26,
                       TBI$LVL3Desc_27,TBI$LVL3Desc_28,TBI$LVL3Desc_29,TBI$LVL3Desc_30,TBI$LVL3Desc_31,
                       TBI$LVL3Desc_32,TBI$LVL3Desc_33)
LVL3Desc <- as.matrix(LVL3Desc)
LVL3Desc <- as.vector(LVL3Desc)


CCI <- data.frame(TBI$CCICatDesc_1,TBI$CCICatDesc_2,TBI$CCICatDesc_3,TBI$CCICatDesc_4,TBI$CCICatDesc_5,TBI$CCICatDesc_6,
                         TBI$CCICatDesc_7,TBI$CCICatDesc_8,TBI$CCICatDesc_9,TBI$CCICatDesc_10,TBI$CCICatDesc_11,TBI$CCICatDesc_12,
                         TBI$CCICatDesc_13,TBI$CCICatDesc_14,TBI$CCICatDesc_15,TBI$CCICatDesc_16,
                         TBI$CCICatDesc_17,TBI$CCICatDesc_18,TBI$CCICatDesc_19,TBI$CCICatDesc_20,TBI$CCICatDesc_21,
                         TBI$CCICatDesc_22,TBI$CCICatDesc_23,TBI$CCICatDesc_24,TBI$CCICatDesc_25,TBI$CCICatDesc_26,
                         TBI$CCICatDesc_27,TBI$CCICatDesc_28,TBI$CCICatDesc_29,TBI$CCICatDesc_30,TBI$CCICatDesc_31,
                         TBI$CCICatDesc_32,TBI$CCICatDesc_33)
CCI <- as.matrix(CCI)
CCI <- as.vector(CCI)

# vectors have length 37,752 (33 diagnosis fields * 1,144 consumers)

TBI_new <- data.frame(cbind(ICD9Code, diagnosis, problemStatus, LVL1Desc, LVL2Desc, LVL3Desc, CCI))
write.table(TBI_new, "T:/Projects/Active/TBI Population Analysis/R Output/TBI_New", sep="\t")

TBIdiagnoses <- subset(TBI_new[which(TBI_new$LVL3Desc=="Acute cerebrovascular disease [109.]" |
                                       TBI_new$LVL3Desc=="Late effects of cerebrovascular disease [113.]"|
                                       TBI_new$LVL2Desc=="Intracranial injury [233.]" | 
                                       TBI_new$LVL2Desc=="Spinal cord injury [227.]"),])
TBItable <- data.frame(table(TBIdiagnoses$diagnosis))
TBItable <- subset(TBItable[which(TBItable$Freq!=0),])
TBItable <- TBItable[order(TBItable$Freq),]
TBItable$Percent <- (TBItable$Freq/((sum(TBItable$Freq))))*100

TBIcomorbid <- subset(TBI_new[which(TBI_new$LVL3Desc!="Acute cerebrovascular disease [109.]" &
                                       TBI_new$LVL3Desc!="Late effects of cerebrovascular disease [113.]"&
                                       TBI_new$LVL2Desc!="Intracranial injury [233.]" &
                                       TBI_new$LVL2Desc!="Spinal cord injury [227.]"),])
Comorbidtable <- data.frame(table(TBIcomorbid$diagnosis))
Comorbidtable <- subset(Comorbidtable[which(Comorbidtable$Freq!=0),])
Comorbidtable <- Comorbidtable[order(Comorbidtable$Freq),]
Comorbidtable$Percent <- (Comorbidtable$Freq/((sum(Comorbidtable$Freq))))*100
sum(Comorbidtable$Freq)

TBILVL3 <- subset(TBI, select=c(Name,Age,Program,Company,ServiceLine,LVL3Desc_1,LVL3Desc_2,LVL3Desc_3,
                                LVL3Desc_4,LVL3Desc_5,LVL3Desc_6,LVL3Desc_7,LVL3Desc_8,LVL3Desc_9,LVL3Desc_10,
                                LVL3Desc_11,LVL3Desc_12,LVL3Desc_13,LVL3Desc_14,LVL3Desc_15,LVL3Desc_16,LVL3Desc_17,
                                LVL3Desc_18,LVL3Desc_19,LVL3Desc_20,LVL3Desc_21,LVL3Desc_22,LVL3Desc_23,LVL3Desc_24,
                                LVL3Desc_25,LVL3Desc_26,LVL3Desc_27,LVL3Desc_28,LVL3Desc_29,LVL3Desc_30,LVL3Desc_31,
                                LVL3Desc_32))

New <- TBILVL3


New <- data.frame(lapply(New, as.character), stringsAsFactors=FALSE)

New$sum1 <- rowSums(New[c(38:404)]==1, na.rm=TRUE)
describe(New$sum1)

###########################################################################################################################

## Creating new data set that includes only the newly created diagnosis variables for cluster analysis
## ClusterData: includes all 366 diagnosis variables
ClusterData <- data.frame(New[c(38:404)])

ClusterData_ColSums <- colSums(ClusterData[,1:366]) # Summing the total number of diagnoses for each diagnosis column
ClusterData_ColSums <- data.frame(ClusterData_ColSums) # Formatting to data frame
describe(ClusterData_ColSums)
colnames(ClusterData_ColSums) <- c("Frequency") # Adding column label
ClusterData_ColSums2 <- subset(ClusterData_ColSums, ClusterData_ColSums$Frequency > 5) # Subsetting to find all diagnoses with n > 5

## Subsetting the original ClusterData data set to include only diagnoses with n > 5
DiagnosesKeep <- c("AcuteCerebDis","AdminSocAdmiss","ADDADHD",
                   "Bipolar","Blindness","Concussion","CondDizz","ConductDis","Convulsions",
                   "DepressDis","DisPeripNerv","Dysphagia","Epilepsy","FractLowLimb","FractNeckFem",
                   "Hemiplegia","LateEffCerebroDis","MentalDisGen","OppDefiDis","OthAcquireDeform",
                   "OthBackProb","OthCentNerveDis","OthEyeDis","OthHeadache","OthIntracInj",
                   "OthNervSysSympDis","OthParalysis","SkullFaceFract","SleepDis","SomatoDis")


ClusterData2 <- ClusterData[DiagnosesKeep] # Creating new data set ClusterData2 to include only the diagnoses identified as have n > 5

## Transposing ClusterData2 so that diagnosis variables are rows
ClusterData3 <- t(ClusterData2)

## Cluster Analysis
dist <- dist(ClusterData3, method='binary') # Calculating distance matrix
#dist2new <- as.matrix(dist2)
#write.table(dist2new, "T:/Reports/DD Population Analysis/Characteristics of the DD Population/Final Report/Tables/Distance Matrix", sep="\t")
#par(mar=c(6,6,2,0))
hclust <- hclust(dist, method='complete') # Hierarchical cluster analysis
#plot(hclust(dist,method='single')) # Single-linkage method
## Labels 
lab <- c("Acute cerebrovascular disease",
         "Administrative/social admission",
         "Attention deficit disorder and Attention deficit hyperactivity disorder",
         "Bipolar disorders",
         "Blindness and vision defects",
         "Concussion",
         "Conditions associated with dizziness or vertigo",
         "Conduct disorder",
         "Convulsions",
         "Depressive disorders",
         "Disorders of the peripheral nervous system",
         "Dysphagia",
         "Epilepsy",
         "Fracture of lower limb",
         "Fracture of neck of femur (hip)",
         "Hemiplegia",
         "Late effects of cerebrovascular disease",
         "Mental disorders due to general medical conditions not elsewhere classified",
         "Oppositional defiant disorder",
         "Other acquired deformities",
         "Other back problems",
         "Other central nervous system disorders",
         "Other eye disorders",
         "Other headache",
         "Other intracranial injury",
         "Other nervous system symptoms and disorders",
         "Other paralysis",
         "Skull and face fractures",
         "Sleep disorders",
         "Somatoform disorders")

lab.col <- c("gray1",
             "orange4",
             "magenta4",
             "magenta4",
             "navyblue",
             "firebrick3",
             "darkorange",
             "dimgrey",
             "orange4",
             "magenta4",
             "darkorchid4",
             "blue4",
             "darkorchid4",
             "chartreuse4",
             "chartreuse4",
             "blue4",
             "gray1",
             "magenta4",
             "darkorange",
             "blue4",
             "magenta4",
             "navyblue",
             "navyblue",
             "darkorange",
             "magenta4",
             "magenta4",
             "dimgrey",
             "navyblue",
             "magenta4",
             "magenta4")
y <- rep(hclust$height,2)
x <- as.numeric(hclust$merge)
y <- y[which(x<0)]
x <- x[which(x<0)]
x <- abs(x)
y <- y[order(x)]
x <- x[order(x)]
hang <- 0.06
plot(hclust, labels=FALSE)
text(x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, cex=0.7, font=2)
################################################################################################################

table(ClusterData2$FractLowLimb, ClusterData2$FractNeckFem)

table(ClusterData2$Dysphagia, ClusterData2$Hemiplegia)
t81 <- xtabs(~ClusterData2$Allergic+ClusterData2$OthAcquireDeform+ClusterData2$OthMetabolic)
xtabs(~ClusterData2$Dysphagia+ClusterData2$Hemiplegia+ClusterData2$OthAcquireDeform)

table(ClusterData2$DisPeripNerv, ClusterData2$Epilepsy)

table(ClusterData2$ConductDis, ClusterData2$OthParalysis)

table(ClusterData2$CondDizz, ClusterData2$OthHeadache)
xtabs(~ClusterData2$CondDizz+ClusterData2$OthHeadache+ClusterData2$OppDefiDis)

table(ClusterData2$Blindness, ClusterData2$SkullFaceFract)
xtabs(~ClusterData2$Blindness+ClusterData2$SkullFaceFract+ClusterData2$OthCentNerveDis)
xtabs(~ClusterData2$Blindness+ClusterData2$SkullFaceFract+ClusterData2$OthCentNerveDis+ClusterData2$OthEyeDis)

table(ClusterData2$DepressDis ,ClusterData2$OthIntracInj)
xtabs(~ClusterData2$DepressDis+ClusterData2$OthIntracInj+ClusterData2$OthNervSysSympDis)
xtabs(~ClusterData2$DepressDis+ClusterData2$OthIntracInj+ClusterData2$OthNervSysSympDis+ClusterData2$MentalDisGen)
xtabs(~ClusterData2$DepressDis+ClusterData2$OthIntracInj+ClusterData2$OthNervSysSympDis+ClusterData2$MentalDisGen+ClusterData2$OthBackProb)
xtabs(~ClusterData2$DepressDis+ClusterData2$OthIntracInj+ClusterData2$OthNervSysSympDis+ClusterData2$MentalDisGen+ClusterData2$OthBackProb+ClusterData2$ADDADHD)
xtabs(~ClusterData2$DepressDis+ClusterData2$OthIntracInj+ClusterData2$OthNervSysSympDis+ClusterData2$MentalDisGen+ClusterData2$OthBackProb+ClusterData2$ADDADHD+ClusterData2$SleepDis)

table(ClusterData2$Bipolar, ClusterData2$SomatoDis)

table(ClusterData2$AcuteCerebDis, ClusterData2$LateEffCerebroDis)

table(ClusterData2$AdminSocAdmiss, ClusterData2$Convulsions)



sum(ClusterData2$AcuteCerebDis==1)
sum(ClusterData2$AdminSocAdmiss==1)
sum(ClusterData2$ADDADHD==1)
sum(ClusterData2$Bipolar==1)
sum(ClusterData2$Blindness==1)
sum(ClusterData2$Concussion==1)
sum(ClusterData2$CondDizz==1)
sum(ClusterData2$ConductDis==1)
sum(ClusterData2$Convulsions==1)
sum(ClusterData2$DepressDis==1)
sum(ClusterData2$DisPeripNerv==1)
sum(ClusterData2$Dysphagia==1)
sum(ClusterData2$Epilepsy==1)
sum(ClusterData2$FractLowLimb==1)
sum(ClusterData2$FractNeckFem==1)
sum(ClusterData2$Hemiplegia==1)
sum(ClusterData2$LateEffCerebroDis==1)
sum(ClusterData2$MentalDisGen==1)
sum(ClusterData2$OppDefiDis==1)
sum(ClusterData2$OthAcquireDeform==1)
sum(ClusterData2$OthBackProb==1)
sum(ClusterData2$OthCentNerveDis==1)
sum(ClusterData2$OthEyeDis==1)
sum(ClusterData2$OthHeadache==1)
sum(ClusterData2$OthIntracInj==1)
sum(ClusterData2$OthNervSysSympDis==1)
sum(ClusterData2$OthParalysis==1)
sum(ClusterData2$SkullFaceFract==1)
sum(ClusterData2$SleepDis==1)
sum(ClusterData2$SomatoDis==1)


################################################################################################################
## This section was used for preliminary report
TBI_Active <- subset(TBI_new[which(TBI_new$problemStatus=="Active"),]) 

ICD9_Active <- data.frame(table(TBI_Active$ICD9Code))
write.table(ICD9_Active, "T:/Projects/Active/TBI Population Analysis/R Output/ICD9_Active", sep="\t")

diagnosis_Active <- data.frame(table(TBI_Active$diagnosis))
write.table(diagnosis_Active, "T:/Projects/Active/TBI Population Analysis/R Output/diagnosis_Active", sep="\t") # This is what was used in preliminary report (Basic Descriptives and Demographics)

LVL1Desc_Active <- data.frame(table(TBI_Active$LVL1Desc))
write.table(LVL1Desc_Active, "T:/Projects/Active/TBI Population Analysis/R Output/LVL1Desc_Active", sep="\t")

LVL2Desc_Active <- data.frame(table(TBI_Active$LVL2Desc))
write.table(LVL2Desc_Active, "T:/Projects/Active/TBI Population Analysis/R Output/LVL2Desc_Active", sep="\t")

LVL3Desc_Active <- data.frame(table(TBI_Active$LVL3Desc))
write.table(LVL3Desc_Active, "T:/Projects/Active/TBI Population Analysis/R Output/LVL3Desc_Active", sep="\t") 
 
################################################################################################################


