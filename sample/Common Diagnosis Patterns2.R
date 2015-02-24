#Installing packages
install.packages("MASS")
library(MASS)
install.packages("pvclust")
library(pvclust)
#example(pvclust)
install.packages("cluster")
library(cluster)

#Import/View data
Diagnoses <- read.delim(".DD Population Analysis/DataDDDiagnosis2.txt", dec=",")
#View(Diagnoses)
attach(Diagnoses)


#How many unique level three descriptions?
unique(Diagnoses$LVL3Desc_22)

#Creating new data set with only level three descriptions through the largest unique level three description
DiagnosesLvl3 <- subset(Diagnoses, select=c(CaseNumber,Age,LVL3Desc_1,LVL3Desc_2,LVL3Desc_3,LVL3Desc_4,LVL3Desc_5,LVL3Desc_6,LVL3Desc_7,LVL3Desc_8,LVL3Desc_9,LVL3Desc_10,LVL3Desc_11,LVL3Desc_12,LVL3Desc_13,LVL3Desc_14,LVL3Desc_15,LVL3Desc_16,LVL3Desc_17,LVL3Desc_18,LVL3Desc_19,LVL3Desc_20,LVL3Desc_21,LVL3Desc_22))
#View(DiagnosesLvl3)
detach(Diagnoses)
#Did NA's from SQL read in correctly?
is.na(DiagnosesLvl3$LVL3Desc_19)
str(DiagnosesLvl3)

#Set values to missing
DiagnosesLvl3[DiagnosesLvl3==""] <- NA
DiagnosesLvl3[DiagnosesLvl3==" "] <- NA
#View(DiagnosesLvl3)
#Checking if values were successfully set to missing
is.na(DiagnosesLvl3$LVL3Desc_1)

#Concatenating columns 
#your_data_frame$new_name = apply(your_data_frame, 1, function(x){paste( x[!is.na(x)], collapse="")}
DiagnosesLvl3$LVL3 = apply(DiagnosesLvl3[c(3:24)], 1, function(x){paste(x[!is.na(x)], collapse=", ")})
#View(DiagnosesLvl3)

#Creating new data set with CaseNumber, Age and LVL3
New <- data.frame(Diagnoses$CaseNumber, Diagnoses$Age, DiagnosesLvl3$LVL3)
colnames(New) <- c("CaseNumber", "Age", "Diagnoses")
#View(New)
str(New$Diagnoses)
unique(New$Diagnoses)
#write.table(New, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/New", sep="\t")

DiagCat <- table(New$Diagnoses)
#write.table(DiagCat, "T:/SQL Query Library/Reports/DD Population/Diagnosis/Tables/DiagCat", sep="\t")

#Splitting the new variable New$DiagnosesLvl3.LVL3 into multiple columns
#Note: There are a total of 14 unique level three descriptions
New$Diagnoses <- as.character(New$Diagnoses)
str(New$Diagnoses)
New$Diagnosis1 <- sapply(strsplit(New$Diagnoses,", "),"[",1)
New$Diagnosis2 <- sapply(strsplit(New$Diagnoses,", "), "[",2)
New$Diagnosis3 <- sapply(strsplit(New$Diagnoses,", "), "[",3)
New$Diagnosis4 <- sapply(strsplit(New$Diagnoses,", "), "[",4)
New$Diagnosis5 <- sapply(strsplit(New$Diagnoses,", "), "[",5)
New$Diagnosis6 <- sapply(strsplit(New$Diagnoses,", "), "[",6)
New$Diagnosis7 <- sapply(strsplit(New$Diagnoses,", "), "[",7)
New$Diagnosis8 <- sapply(strsplit(New$Diagnoses,", "), "[",8)
New$Diagnosis9 <- sapply(strsplit(New$Diagnoses,", "), "[",9)
New$Diagnosis10 <- sapply(strsplit(New$Diagnoses,", "), "[",10)
New$Diagnosis11 <- sapply(strsplit(New$Diagnoses,", "), "[",11)
New$Diagnosis12 <- sapply(strsplit(New$Diagnoses,", "), "[",12)
New$Diagnosis13 <- sapply(strsplit(New$Diagnoses,", "), "[",13)
New$Diagnosis14 <- sapply(strsplit(New$Diagnoses,", "), "[",14)

View(New)
unique(New$Diagnosis14)
str(New)

#Recoding all possible level three diagnoses to numeric values
New[c(4:17)][is.na(New[c(4:17)])] <- 0


#Creating new variables for all observed diagnoses, coding each variable as:
#1 if present in any of the 14 diagnosis columns and 0 if absent in all 14 diagnosis
#Acquired foot deformities [208.]
AquireFootDeform <- for (i in 1:2189){
  if("Acquired foot deformities [208.]" %in% New[i, 4:17])
  {New$AquireFootDeform[i]=1}
  else{New$AquireFootDeform[i]=0}
}
table(New$AquireFootDeform)

#Acute and unspecified renal failure [157.]
AcuteRenalFail <- for (i in 1:2189){
  if("Acute and unspecified renal failure [157.]" %in% New[i, 4:17])
  {New$AcuteRenalFail[i]=1}
  else{New$AcuteRenalFail[i]=0}
}
table(New$AcuteRenalFail)

#Acute cerebrovascular disease [109.]
AcuteCerebDis <- for (i in 1:2189){
  if("Acute cerebrovascular disease [109.]" %in% New[i, 4:17])
  {New$AcuteCerebDis[i]=1}
  else{New$AcuteCerebDis[i]=0}
}
table(New$AcuteCerebDis)

#Administrative/social admission [255.]
AdminSocAdmiss <- for (i in 1:2189){
  if("Administrative/social admission [255.]" %in% New[i, 4:17])
  {New$AdminSocAdmiss[i]=1}
  else{New$AdminSocAdmiss[i]=0}
}
table(New$AdminSocAdmiss)

#All other congenital anomalies
AllOthCogAnom <- for (i in 1:2189){
  if("All other congenital anomalies" %in% New[i, 4:17])
  {New$AllOthCogAnom[i]=1}
  else{New$AllOthCogAnom[i]=0}
}
table(New$AllOthCogAnom)

#Allergic reactions [253.]
Allergic <- for (i in 1:2189){
  if("Allergic reactions [253.]" %in% New[i, 4:17])
  {New$Allergic[i]=1}
  else{New$Allergic[i]=0}
}
table(New$Allergic)

#Aortic valve stenosis
Aortic <- for (i in 1:2189){
  if("Aortic valve stenosis" %in% New[i, 4:17])
  {New$Aortic[i]=1}
  else{New$Aortic[i]=0}
}
table(New$Aortic)

#Appendicitis and other appendiceal conditions [142.]
Appendicitis <- for (i in 1:2189){
  if("Appendicitis and other appendiceal conditions [142.]" %in% New[i, 4:17])
  {New$Appendicitis[i]=1}
  else{New$Appendicitis[i]=0}
}
table(New$Appendicitis)

#Attention deficit disorder and Attention deficit hyperactivity disorder [6523]
ADDADHD <- for (i in 1:2189){
  if("Attention deficit disorder and Attention deficit hyperactivity disorder [6523]" %in% New[i, 4:17])
  {New$ADDADHD[i]=1}
  else{New$ADDADHD[i]=0}
}
table(New$ADDADHD)

#Bipolar disorders [6571]
Bipolar <- for (i in 1:2189){
  if("Bipolar disorders [6571]" %in% New[i, 4:17])
  {New$Bipolar[i]=1}
  else{New$Bipolar[i]=0}
}
table(New$Bipolar)

#Blindness and vision defects [89.]
Blindness <- for (i in 1:2189){
  if("Blindness and vision defects [89.]" %in% New[i, 4:17])
  {New$Blindness[i]=1}
  else{New$Blindness[i]=0}
}
table(New$Blindness)

#Cancer of colon [14.]
ColonCancer <- for (i in 1:2189){
  if("Cancer of colon [14.]" %in% New[i, 4:17])
  {New$ColonCancer[i]=1}
  else{New$ColonCancer[i]=0}
}
table(New$ColonCancer)

#Cardiac dysrhythmias [106.]
CardiacDysrhyth <- for (i in 1:2189){
  if("Cardiac dysrhythmias [106.]" %in% New[i, 4:17])
  {New$CardiacDysrhyth[i]=1}
  else{New$CardiacDysrhyth[i]=0}
}
table(New$CardiacDysrhyth)

#Cellulitis and abscess
Cellulitis <- for (i in 1:2189){
  if("Cellulitis and abscess" %in% New[i, 4:17])
  {New$Cellulitis[i]=1}
  else{New$Cellulitis[i]=0}
}
table(New$Cellulitis)

#Chronic airway obstruction; not otherwise specified
ChronicAirway <- for (i in 1:2189){
  if("Chronic airway obstruction; not otherwise specified" %in% New[i, 4:17])
  {New$ChronicAirway[i]=1}
  else{New$ChronicAirway[i]=0}
}
table(New$ChronicAirway)

#Chronic kidney disease [158.]
ChronicKidney <- for (i in 1:2189){
  if("Chronic kidney disease [158.]" %in% New[i, 4:17])
  {New$ChronicKidney[i]=1}
  else{New$ChronicKidney[i]=0}
}
table(New$ChronicKidney)

#Cleft palate without cleft lip
CleftPal <- for (i in 1:2189){
  if("Cleft palate without cleft lip" %in% New[i, 4:17])
  {New$CleftPal[i]=1}
  else{New$CleftPal[i]=0}
}
table(New$CleftPal)

#Coagulation defects
CoagDefect <- for (i in 1:2189){
  if("Coagulation defects" %in% New[i, 4:17])
  {New$CoagDefect[i]=1}
  else{New$CoagDefect[i]=0}
}
table(New$CoagDefect)

#Codes related to mental health disorders [6631]
CodesMHDis <- for (i in 1:2189){
  if("Codes related to mental health disorders [6631]" %in% New[i, 4:17])
  {New$CodesMHDis[i]=1}
  else{New$CodesMHDis[i]=0}
}
table(New$CodesMHDis)

#Codes related to substance-related disorders [6632]
CodesSRDis <- for (i in 1:2189){
  if("Codes related to substance-related disorders [6632]" %in% New[i, 4:17])
  {New$CodesSRDis[i]=1}
  else{New$CodesSRDis[i]=0}
}
table(New$CodesSRDis)

#Communication disorders [6541]
CommDis <- for (i in 1:2189){
  if("Communication disorders [6541]" %in% New[i, 4:17])
  {New$CommDis[i]=1}
  else{New$CommDis[i]=0}
}
table(New$CommDis)

#Complications of surgical procedures or medical care [238.]
CompSurg <- for (i in 1:2189){
  if("Complications of surgical procedures or medical care [238.]" %in% New[i, 4:17])
  {New$CompSurg[i]=1}
  else{New$CompSurg[i]=0}
}
table(New$CompSurg)

#Concussion
Concussion <- for (i in 1:2189){
  if("Concussion" %in% New[i, 4:17])
  {New$Concussion[i]=1}
  else{New$Concussion[i]=0}
}
table(New$Concussion)

#Conditions associated with dizziness or vertigo [93.]
CondDizz <- for (i in 1:2189){
  if("Conditions associated with dizziness or vertigo [93.]" %in% New[i, 4:17])
  {New$CondDizz[i]=1}
  else{New$CondDizz[i]=0}
}
table(New$CondDizz)

#Conduct disorder [6521]
ConductDis <- for (i in 1:2189){
  if("Conduct disorder [6521]" %in% New[i, 4:17])
  {New$ConductDis[i]=1}
  else{New$ConductDis[i]=0}
}
table(New$ConductDis)

#Conduction disorders [105.]
ConductionDis <- for (i in 1:2189){
  if("Conduction disorders [105.]" %in% New[i, 4:17])
  {New$ConductionDis[i]=1}
  else{New$ConductionDis[i]=0}
}
table(New$ConductionDis)

#Congenital anomalies of larynx trachea and bronchus
CongenAnomTrachea <- for (i in 1:2189){
  if("Congenital anomalies of larynx trachea and bronchus" %in% New[i, 4:17])
  {New$CongenAnomTrachea[i]=1}
  else{New$CongenAnomTrachea[i]=0}
}
table(New$CongenAnomTrachea)

#Congenital anomalies of skull and facial bones
CongenAnomSkull <- for (i in 1:2189){
  if("Congenital anomalies of skull and facial bones" %in% New[i, 4:17])
  {New$CongenAnomSkull[i]=1}
  else{New$CongenAnomSkull[i]=0}
}
table(New$CongenAnomSkull)

#Congenital hip dislocation
CongenHip <- for (i in 1:2189){
  if("Congenital hip dislocation" %in% New[i, 4:17])
  {New$CongenHip[i]=1}
  else{New$CongenHip[i]=0}
}
table(New$CongenHip)

#Congenital hydrocephalus
CongenHydro <- for (i in 1:2189){
  if("Congenital hydrocephalus" %in% New[i, 4:17])
  {New$CongenHydro[i]=1}
  else{New$CongenHydro[i]=0}
}
table(New$CongenHydro)

#Congestive heart failure; nonhypertensive [108.]
CongenHeartFail <- for (i in 1:2189){
  if("Congestive heart failure; nonhypertensive [108.]" %in% New[i, 4:17])
  {New$CongenHeartFail[i]=1}
  else{New$CongenHeartFail[i]=0}
}
table(New$CongenHeartFail)

#Constipation
Constipation <- for (i in 1:2189){
  if("Constipation" %in% New[i, 4:17])
  {New$Constipation[i]=1}
  else{New$Constipation[i]=0}
}
table(New$Constipation)

#Convulsions
Convulsions <- for (i in 1:2189){
  if("Convulsions" %in% New[i, 4:17])
  {New$Convulsions[i]=1}
  else{New$Convulsions[i]=0}
}
table(New$Convulsions)

#Coronary atherosclerosis and other heart disease [101.]
CoronaryAthero <- for (i in 1:2189){
  if("Coronary atherosclerosis and other heart disease [101.]" %in% New[i, 4:17])
  {New$CoronaryAthero[i]=1}
  else{New$CoronaryAthero[i]=0}
}
table(New$CoronaryAthero)

#Deficiency and other anemia [59.]
DeficientAnemia <- for (i in 1:2189){
  if("Deficiency and other anemia [59.]" %in% New[i, 4:17])
  {New$DeficientAnemia[i]=1}
  else{New$DeficientAnemia[i]=0}
}
table(New$DeficientAnemia)

#Depressive disorders [6572]
DepressDis <- for (i in 1:2189){
  if("Depressive disorders [6572]" %in% New[i, 4:17])
  {New$DepressDis[i]=1}
  else{New$DepressDis[i]=0}
}
table(New$DepressDis)

#Developmental disabilities [6542]
DevelopDis <- for (i in 1:2189){
  if("Developmental disabilities [6542]" %in% New[i, 4:17])
  {New$DevelopDis[i]=1}
  else{New$DevelopDis[i]=0}
}
table(New$DevelopDis)

#Diabetes with circulatory manifestations
DiabCirc <- for (i in 1:2189){
  if("Diabetes with circulatory manifestations" %in% New[i, 4:17])
  {New$DiabCirc[i]=1}
  else{New$DiabCirc[i]=0}
}
table(New$DiabCirc)

#Diabetes with neurological manifestations
DiabNeuro <- for (i in 1:2189){
  if("Diabetes with neurological manifestations" %in% New[i, 4:17])
  {New$DiabNeuro[i]=1}
  else{New$DiabNeuro[i]=0}
}
table(New$DiabNeuro)

#Diabetes with ophthalmic manifestations
DiabOptha <- for (i in 1:2189){
  if("Diabetes with ophthalmic manifestations" %in% New[i, 4:17])
  {New$DiabOptha[i]=1}
  else{New$DiabOptha[i]=0}
}
table(New$DiabOptha)

#Diabetes with other manifestations
DiabOther <- for (i in 1:2189){
  if("Diabetes with other manifestations" %in% New[i, 4:17])
  {New$DiabOther[i]=1}
  else{New$DiabOther[i]=0}
}
table(New$DiabOther)

#Diaphragmatic hernia
DiaphHernia <- for (i in 1:2189){
  if("Diaphragmatic hernia" %in% New[i, 4:17])
  {New$DiaphHernia[i]=1}
  else{New$DiaphHernia[i]=0}
}
table(New$DiaphHernia)

#Disorders of mineral metabolism
DisMinMeta <- for (i in 1:2189){
  if("Disorders of mineral metabolism" %in% New[i, 4:17])
  {New$DisMinMeta[i]=1}
  else{New$DisMinMeta[i]=0}
}
table(New$DisMinMeta)

#Disorders of the peripheral nervous system
DisPeripNerv <- for (i in 1:2189){
  if("Disorders of the peripheral nervous system" %in% New[i, 4:17])
  {New$DisPeripNerv[i]=1}
  else{New$DisPeripNerv[i]=0}
}
table(New$DisPeripNerv)

#Diverticulosis and diverticulitis [146.]
Divert <- for (i in 1:2189){
  if("Diverticulosis and diverticulitis [146.]" %in% New[i, 4:17])
  {New$Divert[i]=1}
  else{New$Divert[i]=0}
}
table(New$Divert)

#Double outlet right ventricle
DoubleOutRVent <- for (i in 1:2189){
  if("Double outlet right ventricle" %in% New[i, 4:17])
  {New$DoubleOutRVent[i]=1}
  else{New$DoubleOutRVent[i]=0}
}
table(New$DoubleOutRVent)

#Downs Syndrome
DownsSyn <- for (i in 1:2189){
  if("Downs Syndrome" %in% New[i, 4:17])
  {New$DownsSyn[i]=1}
  else{New$DownsSyn[i]=0}
}
table(New$DownsSyn)

#Dysphagia
Dysphagia <- for (i in 1:2189){
  if("Dysphagia" %in% New[i, 4:17])
  {New$Dysphagia[i]=1}
  else{New$Dysphagia[i]=0}
}
table(New$Dysphagia)

#Eating disorders [6702]
EatingDis <- for (i in 1:2189){
  if("Eating disorders [6702]" %in% New[i, 4:17])
  {New$EatingDis[i]=1}
  else{New$EatingDis[i]=0}
}
table(New$EatingDis)

#Elimination disorders [6551]
ElimDis <- for (i in 1:2189){
  if("Elimination disorders [6551]" %in% New[i, 4:17])
  {New$ElimDis[i]=1}
  else{New$ElimDis[i]=0}
}
table(New$ElimDis)

#Endometriosis [169.]
Endometriosis <- for (i in 1:2189){
  if("Endometriosis [169.]" %in% New[i, 4:17])
  {New$Endometriosis[i]=1}
  else{New$Endometriosis[i]=0}
}
table(New$Endometriosis)

#Epilepsy
Epilepsy <- for (i in 1:2189){
  if("Epilepsy" %in% New[i, 4:17])
  {New$Epilepsy[i]=1}
  else{New$Epilepsy[i]=0}
}
table(New$Epilepsy)

#Esophageal disorders [138.]
EsophDis <- for (i in 1:2189){
  if("Esophageal disorders [138.]" %in% New[i, 4:17])
  {New$EsophDis[i]=1}
  else{New$EsophDis[i]=0}
}
table(New$EsophDis)

#Essential hypertension [98.]
EssenHyper <- for (i in 1:2189){
  if("Essential hypertension [98.]" %in% New[i, 4:17])
  {New$EssenHyper[i]=1}
  else{New$EssenHyper[i]=0}
}
table(New$EssenHyper)

#Factitious disorders [6703]
FactDis <- for (i in 1:2189){
  if("Factitious disorders [6703]" %in% New[i, 4:17])
  {New$FactDis[i]=1}
  else{New$FactDis[i]=0}
}
table(New$FactDis)

#Fracture of lower limb [230.]
FractLowLimb <- for (i in 1:2189){
  if("Fracture of lower limb [230.]" %in% New[i, 4:17])
  {New$FractLowLimb[i]=1}
  else{New$FractLowLimb[i]=0}
}
table(New$FractLowLimb)

#Fracture of neck of femur (hip) [226.]
FractNeckFem <- for (i in 1:2189){
  if("Fracture of neck of femur (hip) [226.]" %in% New[i, 4:17])
  {New$FractNeckFem[i]=1}
  else{New$FractNeckFem[i]=0}
}
table(New$FractNeckFem)

#Gastritis and duodenitis [140.]
GastDuoden <- for (i in 1:2189){
  if("Gastritis and duodenitis [140.]" %in% New[i, 4:17])
  {New$GastDuoden[i]=1}
  else{New$GastDuoden[i]=0}
}
table(New$GastDuoden)

#Gastroesophageal laceration syndrome
GastLacerSyn <- for (i in 1:2189){
  if("Gastroesophageal laceration syndrome" %in% New[i, 4:17])
  {New$GastLacerSyn[i]=1}
  else{New$GastLacerSyn[i]=0}
}
table(New$GastLacerSyn)

#Genitourinary symptoms and ill-defined conditions [163.]
GenitSymp <- for (i in 1:2189){
  if("Genitourinary symptoms and ill-defined conditions [163.]" %in% New[i, 4:17])
  {New$GenitSymp[i]=1}
  else{New$GenitSymp[i]=0}
}
table(New$GenitSymp)

#Glaucoma [88.]
Glaucoma <- for (i in 1:2189){
  if("Glaucoma [88.]" %in% New[i, 4:17])
  {New$Glaucoma[i]=1}
  else{New$Glaucoma[i]=0}
}
table(New$Glaucoma)

#Heart valve disorders [96.]
HeartValveDis <- for (i in 1:2189){
  if("Heart valve disorders [96.]" %in% New[i, 4:17])
  {New$HeartValveDis[i]=1}
  else{New$HeartValveDis[i]=0}
}
table(New$HeartValveDis)

#Hemiplegia
Hemiplegia <- for (i in 1:2189){
  if("Hemiplegia" %in% New[i, 4:17])
  {New$Hemiplegia[i]=1}
  else{New$Hemiplegia[i]=0}
}
table(New$Hemiplegia)

#Hemorrhoids [120.]
Hemorrhoids <- for (i in 1:2189){
  if("Hemorrhoids [120.]" %in% New[i, 4:17])
  {New$Hemorrhoids[i]=1}
  else{New$Hemorrhoids[i]=0}
}
table(New$Hemorrhoids)

#Hodgkins disease [37.]
HodgkinsDis <- for (i in 1:2189){
  if("Hodgkins disease [37.]" %in% New[i, 4:17])
  {New$HodgkinsDis[i]=1}
  else{New$HodgkinsDis[i]=0}
}
table(New$HodgkinsDis)

#Hypertension with complications and secondary hypertension [99.]
HypertensionComp <- for (i in 1:2189){
  if("Hypertension with complications and secondary hypertension [99.]" %in% New[i, 4:17])
  {New$HypertensionComp[i]=1}
  else{New$HypertensionComp[i]=0}
}
table(New$HypertensionComp)

#Hypopotassemia
Hypopotassemia <- for (i in 1:2189){
  if("Hypopotassemia" %in% New[i, 4:17])
  {New$Hypopotassemia[i]=1}
  else{New$Hypopotassemia[i]=0}
}
table(New$Hypopotassemia)

#Inflammation; infection of eye (except that caused by TB or STD) [90.]
Inflammation <- for (i in 1:2189){
  if("Inflammation; infection of eye (except that caused by TB or STD) [90.]" %in% New[i, 4:17])
  {New$Inflammation[i]=1}
  else{New$Inflammation[i]=0}
}
table(New$Inflammation)

#Intellectual disabilities [6543]
IntellDis <- for (i in 1:2189){
  if("Intellectual disabilities [6543]" %in% New[i, 4:17])
          {New$IntellDis[i]=1}
  else{New$IntellDis[i]=0}
}
table(New$IntellDis)

#Intestinal obstruction without hernia [145.]
IntesObstruct <- for (i in 1:2189){
  if("Intestinal obstruction without hernia [145.]" %in% New[i, 4:17])
  {New$IntesObstruct[i]=1}
  else{New$IntesObstruct[i]=0}
}
table(New$IntesObstruct)

#Late effects of cerebrovascular disease [113.]
LateEffCerebroDis <- for (i in 1:2189){
  if("Late effects of cerebrovascular disease [113.]" %in% New[i, 4:17])
  {New$LateEffCerebroDis[i]=1}
  else{New$LateEffCerebroDis[i]=0}
}
table(New$LateEffCerebroDis)

#Learning disorders [6544]
LearnDis <- for (i in 1:2189){
  if("Learning disorders [6544]" %in% New[i, 4:17])
  {New$LearnDis[i]=1}
  else{New$LearnDis[i]=0}
}
table(New$LearnDis)

#Medical examination/evaluation [256.]
MedExam <- for (i in 1:2189){
  if("Medical examination/evaluation [256.]" %in% New[i, 4:17])
  {New$MedExam[i]=1}
  else{New$MedExam[i]=0}
}
table(New$MedExam)

#Menopausal disorders [173.]
MenopausDis <- for (i in 1:2189){
  if("Menopausal disorders [173.]" %in% New[i, 4:17])
  {New$MenopausDis[i]=1}
  else{New$MenopausDis[i]=0}
}
table(New$MenopausDis)

#Menstrual disorders [171.]
MenstDis <- for (i in 1:2189){
  if("Menstrual disorders [171.]" %in% New[i, 4:17])
  {New$MenstDis[i]=1}
  else{New$MenstDis[i]=0}
}
table(New$MenstDis)

#Mental disorders due to general medical conditions not elsewhere classified [6708]
MentalDisGen <- for (i in 1:2189){
  if("Mental disorders due to general medical conditions not elsewhere classified [6708]" %in% New[i, 4:17])
  {New$MentalDisGen[i]=1}
  else{New$MentalDisGen[i]=0}
}
table(New$MentalDisGen)

#Microcephalus
Microcephalus <- for (i in 1:2189){
  if("Microcephalus" %in% New[i, 4:17])
  {New$Microcephalus[i]=1}
  else{New$Microcephalus[i]=0}
}
table(New$Microcephalus)

#Migraine
Migraine <- for (i in 1:2189){
  if("Migraine" %in% New[i, 4:17])
  {New$Migraine[i]=1}
  else{New$Migraine[i]=0}
}
table(New$Migraine)

#Motor skill disorders [6545]
MotorSkillDis <- for (i in 1:2189){
  if("Motor skill disorders [6545]" %in% New[i, 4:17])
  {New$MotorSkillDis[i]=1}
  else{New$MotorSkillDis[i]=0}
}
table(New$MotorSkillDis)

#Multiple sclerosis [80.]
MultSclerosis <- for (i in 1:2189){
  if("Multiple sclerosis [80.]" %in% New[i, 4:17])
  {New$MultSclerosis[i]=1}
  else{New$MultSclerosis[i]=0}
}
table(New$MultSclerosis)

#Nephritis; nephrosis; renal sclerosis [156.]
Nephritis <- for (i in 1:2189){
  if("Nephritis; nephrosis; renal sclerosis [156.]" %in% New[i, 4:17])
  {New$Nephritis[i]=1}
  else{New$Nephritis[i]=0}
}
table(New$Nephritis)

#Nonmalignant breast conditions [167.]
NonmaligBreastCon <- for (i in 1:2189){
  if("Nonmalignant breast conditions [167.]" %in% New[i, 4:17])
  {New$NonmaligBreastCon[i]=1}
  else{New$NonmaligBreastCon[i]=0}
}
table(New$NonmaligBreastCon)

#Obesity
Obesity <- for (i in 1:2189){
  if("Obesity" %in% New[i, 4:17])
  {New$Obesity[i]=1}
  else{New$Obesity[i]=0}
}
table(New$Obesity)

#Obstructive chronic bronchitis
ObsChronBronch <- for (i in 1:2189){
  if("Obstructive chronic bronchitis" %in% New[i, 4:17])
  {New$ObsChronBronch[i]=1}
  else{New$ObsChronBronch[i]=0}
}
table(New$ObsChronBronch)

#Oppositional defiant disorder [6522]
OppDefiDis <- for (i in 1:2189){
  if("Oppositional defiant disorder [6522]" %in% New[i, 4:17])
  {New$OppDefiDis[i]=1}
  else{New$OppDefiDis[i]=0}
}
table(New$OppDefiDis)

#Osteoarthritis [203.]
Osteoarthritis <- for (i in 1:2189){
  if("Osteoarthritis [203.]" %in% New[i, 4:17])
  {New$Osteoarthritis[i]=1}
  else{New$Osteoarthritis[i]=0}
}
table(New$Osteoarthritis)

#Other acquired deformities [209.]
OthAcquireDeform <- for (i in 1:2189){
  if("Other acquired deformities [209.]" %in% New[i, 4:17])
  {New$OthAcquireDeform[i]=1}
  else{New$OthAcquireDeform[i]=0}
}
table(New$OthAcquireDeform)

#Other and unspecified asthma
OthAsthma <- for (i in 1:2189){
  if("Other and unspecified asthma" %in% New[i, 4:17])
  {New$OthAsthma[i]=1}
  else{New$OthAsthma[i]=0}
}
table(New$OthAsthma)

#Other and unspecified benign neoplasm [47.]
OthBenignNeo <- for (i in 1:2189){
  if("Other and unspecified benign neoplasm [47.]" %in% New[i, 4:17])
  {New$OthBenignNeo[i]=1}
  else{New$OthBenignNeo[i]=0}
}
table(New$OthBenignNeo)

#Other and unspecified complications of birth; puerperium affecting management of moth
OthCompBirth <- for (i in 1:2189){
  if("Other and unspecified complications of birth; puerperium affecting management of moth" %in% New[i, 4:17])
  {New$OthCompBirth[i]=1}
  else{New$OthCompBirth[i]=0}
}
table(New$OthCompBirth)

#Other and unspecified gastrointestinal disorders
OthGastroDis <- for (i in 1:2189){
  if("Other and unspecified gastrointestinal disorders" %in% New[i, 4:17])
  {New$OthGastroDis[i]=1}
  else{New$OthGastroDis[i]=0}
}
table(New$OthGastroDis)

#Other and unspecified lower respiratory disease
OthLowRespDis <- for (i in 1:2189){
  if("Other and unspecified lower respiratory disease" %in% New[i, 4:17])
  {New$OthLowRespDis[i]=1}
  else{New$OthLowRespDis[i]=0}
}
table(New$OthLowRespDis)

#Other and unspecified metabolic; nutritional; and endocrine disorders
OthMetabolic <- for (i in 1:2189){
  if("Other and unspecified metabolic; nutritional; and endocrine disorders" %in% New[i, 4:17])
  {New$OthMetabolic[i]=1}
  else{New$OthMetabolic[i]=0}
}
table(New$OthMetabolic)

#Other anomalies of bulbus cordis and cardiac septal closure
OthBulbCordis <- for (i in 1:2189){
  if("Other anomalies of bulbus cordis and cardiac septal closure" %in% New[i, 4:17])
  {New$OthBulbCordis[i]=1}
  else{New$OthBulbCordis[i]=0}
}
table(New$OthBulbCordis)

#Other back problems
OthBackProb <- for (i in 1:2189){
  if("Other back problems" %in% New[i, 4:17])
  {New$OthBackProb[i]=1}
  else{New$OthBackProb[i]=0}
}
table(New$OthBackProb)

#Other bacterial infections [3.]
OthBactInfect <- for (i in 1:2189){
  if("Other bacterial infections [3.]" %in% New[i, 4:17])
  {New$OthBactInfect[i]=1}
  else{New$OthBactInfect[i]=0}
}
table(New$OthBactInfect)

#Other central nervous system disorders
OthCentNerveDis <- for (i in 1:2189){
  if("Other central nervous system disorders" %in% New[i, 4:17])
  {New$OthCentNerveDis[i]=1}
  else{New$OthCentNerveDis[i]=0}
}
table(New$OthCentNerveDis)

#Other chronic pulmonary disease
OthChronPulmDis <- for (i in 1:2189){
  if("Other chronic pulmonary disease" %in% New[i, 4:17])
  {New$OthChronPulmDis[i]=1}
  else{New$OthChronPulmDis[i]=0}
}
table(New$OthChronPulmDis)

#Other circulatory disease [117.]
OthCircDis <- for (i in 1:2189){
  if("Other circulatory disease [117.]" %in% New[i, 4:17])
  {New$OthCircDis[i]=1}
  else{New$OthCircDis[i]=0}
}
table(New$OthCircDis)

#Other complications of pregnancy [181.]
OtherCompPreg <- for (i in 1:2189){
  if("Other complications of pregnancy [181.]" %in% New[i, 4:17])
  {New$OtherCompPreg[i]=1}
  else{New$OtherCompPreg[i]=0}
}
table(New$OtherCompPreg)

#Other congenital anomalies of integument
OthCongenAnomInteg <- for (i in 1:2189){
  if("Other congenital anomalies of integument" %in% New[i, 4:17])
  {New$OthCongenAnomInteg[i]=1}
  else{New$OthCongenAnomInteg[i]=0}
}
table(New$OthCongenAnomInteg)

#Other congenital anomalies of urinary system
OthCongenAnomUrinary <- for (i in 1:2189){
  if("Other congenital anomalies of urinary system" %in% New[i, 4:17])
  {New$OthCongenAnomUrinary[i]=1}
  else{New$OthCongenAnomUrinary[i]=0}
}
table(New$OthCongenAnomUrinary)

#Other diseases of bladder and urethra [162.]
OthDisBladder <- for (i in 1:2189){
  if("Other diseases of bladder and urethra [162.]" %in% New[i, 4:17])
  {New$OthDisBladder[i]=1}
  else{New$OthDisBladder[i]=0}
}
table(New$OthDisBladder)

#Other diseases of veins and lymphatics [121.]
OthDisVeins <- for (i in 1:2189){
  if("Other diseases of veins and lymphatics [121.]" %in% New[i, 4:17])
  {New$OthDisVeins[i]=1}
  else{New$OthDisVeins[i]=0}
}
table(New$OthDisVeins)

#Other disorders of infancy  childhood  or adolescence [6552]
OthDisInfancy <- for (i in 1:2189){
  if("Other disorders of infancy  childhood  or adolescence [6552]" %in% New[i, 4:17])
  {New$OthDisInfancy[i]=1}
  else{New$OthDisInfancy[i]=0}
}
table(New$OthDisInfancy )

#Other disorders of stomach and duodenum [141.]
OthDisStomach <- for (i in 1:2189){
  if("Other disorders of stomach and duodenum [141.]" %in% New[i, 4:17])
  {New$OthDisStomach[i]=1}
  else{New$OthDisStomach[i]=0}
}
table(New$OthDisStomach)

#Other ear and sense organ disorders [94.]
OthEarDis <- for (i in 1:2189){
  if("Other ear and sense organ disorders [94.]" %in% New[i, 4:17])
  {New$OthEarDis[i]=1}
  else{New$OthEarDis[i]=0}
}
table(New$OthEarDis)

#Other eye disorders [91.]
OthEyeDis <- for (i in 1:2189){
  if("Other eye disorders [91.]" %in% New[i, 4:17])
  {New$OthEyeDis[i]=1}
  else{New$OthEyeDis[i]=0}
}
table(New$OthEyeDis)

#Other female genital disorders [175.]
OthFemGenDis <- for (i in 1:2189){
  if("Other female genital disorders [175.]" %in% New[i, 4:17])
  {New$OthFemGenDis[i]=1}
  else{New$OthFemGenDis[i]=0}
}
table(New$OthFemGenDis)

#Other headache
OthHeadache <- for (i in 1:2189){
  if("Other headache" %in% New[i, 4:17])
  {New$OthHeadache[i]=1}
  else{New$OthHeadache[i]=0}
}
table(New$OthHeadache)

#Other heart valve congenital anomalies
OthHeartValveAnom <- for (i in 1:2189){
  if("Other heart valve congenital anomalies" %in% New[i, 4:17])
  {New$OthHeartValveAnom[i]=1}
  else{New$OthHeartValveAnom[i]=0}
}
table(New$OthHeartValveAnom)

#Other hereditary and degenerative nervous system conditions [81.]
OthHeredNervSysCond <- for (i in 1:2189){
  if("Other hereditary and degenerative nervous system conditions [81.]" %in% New[i, 4:17])
  {New$OthHeredNervSysCond[i]=1}
  else{New$OthHeredNervSysCond[i]=0}
}
table(New$OthHeredNervSysCond)

#Other intracranial injury
OthIntracInj <- for (i in 1:2189){
  if("Other intracranial injury" %in% New[i, 4:17])
  {New$OthIntracInj[i]=1}
  else{New$OthIntracInj[i]=0}
}
table(New$OthIntracInj)

#Other malnutrition
OthMalnut <- for (i in 1:2189){
  if("Other malnutrition" %in% New[i, 4:17])
  {New$OthMalnut[i]=1}
  else{New$OthMalnut[i]=0}
}
table(New$OthMalnut)

#Other mycoses
OthMycoses <- for (i in 1:2189){
  if("Other mycoses" %in% New[i, 4:17])
  {New$OthMycoses[i]=1}
  else{New$OthMycoses[i]=0}
}
table(New$OthMycoses)

#Other nervous system symptoms and disorders
OthNervSysSympDis <- for (i in 1:2189){
  if("Other nervous system symptoms and disorders" %in% New[i, 4:17])
  {New$OthNervSysSympDis[i]=1}
  else{New$OthNervSysSympDis[i]=0}
}
table(New$OthNervSysSympDis)

#Other non-traumatic joint disorders [204.]
OthNonTraumaJointDis <- for (i in 1:2189){
  if("Other non-traumatic joint disorders [204.]" %in% New[i, 4:17])
  {New$OthNonTraumaJointDis[i]=1}
  else{New$OthNonTraumaJointDis[i]=0}
}
table(New$OthNonTraumaJointDis)

#Other paralysis
OthParalysis <- for (i in 1:2189){
  if("Other paralysis" %in% New[i, 4:17])
  {New$OthParalysis[i]=1}
  else{New$OthParalysis[i]=0}
}
table(New$OthParalysis)

#Other respiratory congenital anomalies
OthRespCongenAnom <- for (i in 1:2189){
  if("Other respiratory congenital anomalies" %in% New[i, 4:17])
  {New$OthRespCongenAnom[i]=1}
  else{New$OthRespCongenAnom[i]=0}
}
table(New$OthRespCongenAnom)

#Other spinal congenital anomalies
OthSpinalCongenAnom <- for (i in 1:2189){
  if("Other spinal congenital anomalies" %in% New[i, 4:17])
  {New$OthSpinalCongenAnom[i]=1}
  else{New$OthSpinalCongenAnom[i]=0}
}
table(New$OthSpinalCongenAnom)

#Other thyroid disorders
OthThyroidDis <- for (i in 1:2189){
  if("Other thyroid disorders" %in% New[i, 4:17])
  {New$OthThyroidDis[i]=1}
  else{New$OthThyroidDis[i]=0}
}
table(New$OthThyroidDis)

#Other upper respiratory infections [126.]
OthUpRespInfect <- for (i in 1:2189){
  if("Other upper respiratory infections [126.]" %in% New[i, 4:17])
  {New$OthUpRespInfect[i]=1}
  else{New$OthUpRespInfect[i]=0}
}
table(New$OthUpRespInfect)

#Other viral infections [7.]
OthViralInfect <- for (i in 1:2189){
  if("Other viral infections [7.]" %in% New[i, 4:17])
  {New$OthViralInfect[i]=1}
  else{New$OthViralInfect[i]=0}
}
table(New$OthViralInfect)

#Pervasive developmental disorders [6553]
PervaDevelDis <- for (i in 1:2189){
  if("Pervasive developmental disorders [6553]" %in% New[i, 4:17])
  {New$PervaDevelDis[i]=1}
  else{New$PervaDevelDis[i]=0}
}
table(New$PervaDevelDis)

#Poisoning by other medications and drugs [242.]
PoisonOther <- for (i in 1:2189){
  if("Poisoning by other medications and drugs [242.]" %in% New[i, 4:17])
  {New$PoisonOther[i]=1}
  else{New$PoisonOther[i]=0}
}
table(New$PoisonOther)

#Pulmonary heart disease [103.]
PulmHeartDis <- for (i in 1:2189){
  if("Pulmonary heart disease [103.]" %in% New[i, 4:17])
  {New$PulmHeartDis[i]=1}
  else{New$PulmHeartDis[i]=0}
}
table(New$PulmHeartDis)

#Pulmonary valve atresia and stenosis
PulmValveAtresia <- for (i in 1:2189){
  if("Pulmonary valve atresia and stenosis" %in% New[i, 4:17])
  {New$PulmValveAtresia[i]=1}
  else{New$PulmValveAtresia[i]=0}
}
table(New$PulmValveAtresia)

#Regional enteritis and ulcerative colitis [144.]
RegEnterCol <- for (i in 1:2189){
  if("Regional enteritis and ulcerative colitis [144.]" %in% New[i, 4:17])
  {New$RegEnterCol[i]=1}
  else{New$RegEnterCol[i]=0}
}
table(New$RegEnterCol)

#Retinal detachments; defects; vascular occlusion; and retinopathy [87.]
RetDetach <- for (i in 1:2189){
  if("Retinal detachments; defects; vascular occlusion; and retinopathy [87.]" %in% New[i, 4:17])
  {New$RetDetach[i]=1}
  else{New$RetDetach[i]=0}
}
table(New$RetDetach)

#Rheumatoid arthritis and related disease [202.]
RheuArthritis <- for (i in 1:2189){
  if("Rheumatoid arthritis and related disease [202.]" %in% New[i, 4:17])
  {New$RheuArthritis[i]=1}
  else{New$RheuArthritis[i]=0}
}
table(New$RheuArthritis)

#Sexual and gender identity disorders [6705]
SexGenIdentDis <- for (i in 1:2189){
  if("Sexual and gender identity disorders [6705]" %in% New[i, 4:17])
  {New$SexGenIdentDis[i]=1}
  else{New$SexGenIdentDis[i]=0}
}
table(New$SexGenIdentDis)

#Sickle cell anemia [61.]
SickleCell <- for (i in 1:2189){
  if("Sickle cell anemia [61.]" %in% New[i, 4:17])
  {New$SickleCell[i]=1}
  else{New$SickleCell[i]=0}
}
table(New$SickleCell)

#Skull and face fractures [228.]
SkullFaceFract <- for (i in 1:2189){
  if("Skull and face fractures [228.]" %in% New[i, 4:17])
  {New$SkullFaceFract[i]=1}
  else{New$SkullFaceFract[i]=0}
}
table(New$SkullFaceFract)

#Sleep disorders [6706]
SleepDis <- for (i in 1:2189){
  if("Sleep disorders [6706]" %in% New[i, 4:17])
  {New$SleepDis[i]=1}
  else{New$SleepDis[i]=0}
}
table(New$SleepDis)

#Somatoform disorders [6707]
SomatoDis <- for (i in 1:2189){
  if("Somatoform disorders [6707]" %in% New[i, 4:17])
  {New$SomatoDis[i]=1}
  else{New$SomatoDis[i]=0}
}
table(New$SomatoDis)

#Spina bifida
SpinaBifida <- for (i in 1:2189){
  if("Spina bifida" %in% New[i, 4:17])
  {New$SpinaBifida[i]=1}
  else{New$SpinaBifida[i]=0}
}
table(New$SpinaBifida)

#Tetralogy of Fallot
TetraFallot <- for (i in 1:2189){
  if("Tetralogy of Fallot" %in% New[i, 4:17])
  {New$TetraFallot[i]=1}
  else{New$TetraFallot[i]=0}
}
table(New$TetraFallot)

#Thrombocytopenia
Thrombocytopenia <- for (i in 1:2189){
  if("Thrombocytopenia" %in% New[i, 4:17])
  {New$Thrombocytopenia[i]=1}
  else{New$Thrombocytopenia[i]=0}
}
table(New$Thrombocytopenia)

#Thyrotoxicosis with or without goiter
Thyrotoxicosis <- for (i in 1:2189){
  if("Thyrotoxicosis with or without goiter" %in% New[i, 4:17])
  {New$Thyrotoxicosis[i]=1}
  else{New$Thyrotoxicosis[i]=0}
}
table(New$Thyrotoxicosis)

#Tic disorders [6554]
TicDis <- for (i in 1:2189){
  if("Tic disorders [6554]" %in% New[i, 4:17])
  {New$TicDis[i]=1}
  else{New$TicDis[i]=0}
}
table(New$TicDis)

#Transposition of great vessels
TranspGreatVess <- for (i in 1:2189){
  if("Transposition of great vessels" %in% New[i, 4:17])
  {New$TranspGreatVess[i]=1}
  else{New$TranspGreatVess[i]=0}
}
table(New$TranspGreatVess)

#Tuberculosis [1.]
Tuberculosis <- for (i in 1:2189){
  if("Tuberculosis [1.]" %in% New[i, 4:17])
  {New$Tuberculosis[i]=1}
  else{New$Tuberculosis[i]=0}
}
table(New$Tuberculosis)

#Undescended testicle
UndescTesticle <- for (i in 1:2189){
  if("Undescended testicle" %in% New[i, 4:17])
  {New$UndescTesticle[i]=1}
  else{New$UndescTesticle[i]=0}
}
table(New$UndescTesticle)

#Urinary tract infections [159.]
UTI <- for (i in 1:2189){
  if("Urinary tract infections [159.]" %in% New[i, 4:17])
  {New$UTI[i]=1}
  else{New$UTI[i]=0}
}
table(New$UTI)

#Varicose veins of lower extremity [119.]
VaricoseVeins <- for (i in 1:2189){
  if("Varicose veins of lower extremity [119.]" %in% New[i, 4:17])
  {New$VaricoseVeins[i]=1}
  else{New$VaricoseVeins[i]=0}
}
table(New$VaricoseVeins)

#Ventricular septal defect
VentSeptDefect <- for (i in 1:2189){
  if("Ventricular septal defect" %in% New[i, 4:17])
  {New$VentSeptDefect[i]=1}
  else{New$VentSeptDefect[i]=0}
}
table(New$VentSeptDefect)

View(New)

#Creating new data set that includes only the newly created diagnosis variables for cluster analysis
#ClusterData: includes all 146 diagnosis variables
ClusterData <- data.frame(New[c(18:163)])
View(ClusterData)
str(ClusterData)


#Transposing ClusterData so that diagnosis variables are rows and id's are columns
ClusterData2 <- t(ClusterData)
View(ClusterData2)

attach(New)

#ClusterData: includes all diagnosis variables with 10 or more individuals diagnosed with that disorder
ClusterData <- data.frame(New$Constipation, New$GenitSymp, New$HeartValveDis, New$Migraine, New$ElimDis, New$OthNervSysSympDis,
        New$OthEyeDis, New$AdminSocAdmiss, New$OthHeredNervSysCond, New$SexGenIdentDis, New$TicDis,
        New$OthDisInfancy, New$Convulsions, New$OthIntracInj, New$Dysphagia, New$OthNonTraumaJointDis,
        New$OthAcquireDeform, New$Allergic, New$ConductDis, New$OppDefiDis, New$OthMetabolic, 
        New$EsophDis, New$OthAsthma, New$AllOthCogAnom, New$Blindness, New$DevelopDis, New$OthEarDis,
        New$EssenHyper, New$Obesity, New$OthThyroidDis, New$CommDis, New$CodesMHDis, New$DownsSyn,
        New$OthNervSysSympDis, New$Epilepsy, New$ADDADHD, New$OthParalysis, New$PervaDevelDis,
        New$LearnDis, New$DepressDis, New$Bipolar, New$IntellDis)

#Cluster Analysis
dist.items <- dist(ClusterData2, method='binary')
items.complete.link <- hclust(dist.items, method='complete')
plot(items.complete.link)