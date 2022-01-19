suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))

source("manyColsToDummy.R")

withdrawn<-read.csv("w48818_20210809.csv", header = FALSE)

QCids<-read.table("bd_QC-keep.txt",header=TRUE)

###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#Load data=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

#Load UK Biobank datasets-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
source('load_UKBphenotables.R') #20 min

#Phenotypes  ------------------------------------------------------------------------------
#Covariates 
#Model 1: sex, age, age squared, genotyping array, and assessment center indicators (sites of recruitment); 
#Cholesterol;  


pheno<-bd%>%select(f.eid, f.21003.0.0, f.31.0.0, 
                   f.189.0.0,
                   f.54.0.0, f.22000.0.0
                    )

colnames(pheno)<-c("IID", "Age", "Sex",  
                   "Townsend",
                   "Assessment_center", "Geno_batch"
                    )

pheno2<-bd_join4%>%select(f.eid,
                          f.21001.0.0, f.30690.0.0, f.30780.0.0,
			  f.30760.0.0, f.30870.0.0
                          )

colnames(pheno2)<-c("IID",
                    "BMI", "TotalCholesterol", "LDLCholesterol",
		    "HDLCholesterol", "Triglycerides"
		   )

new<-left_join(pheno, pheno2, by="IID")
new<-as_tibble(new)

#Remove withdrawn participants------------------------------------
new<-new[!(new$IID %in% withdrawn$V1), ]

#QC participants via output of UKB_participantQC.R----------------

new<-new[!(new$IID %in% QCids$V1),]

#Age squared----------------------------
new$Age2<-new$Age^2

#Make dummy 0/1 cols for each assessment center----------------------
#table(pheno$Assessment_center)
centers<-unique(new$Assessment_center)
centercols<-paste("center", 1:22, sep="")
new[centercols]<-0

for (i in 1:length(centers)){
    new[new$Assessment_center==centers[i],][centercols[i]]<-1
}

new<-new%>%select(-Assessment_center)
new

#Genotype batch
new$Geno_batch1<-0
new$Geno_batch1[new$Geno_batch>0]<-1
#sum(pheno$Geno_batch1) #[1] 438313
new$Geno_batch<-new$Geno_batch1
new<-new%>%select(-Geno_batch1)
#table(new$Geno_batch) #it worked


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

#Switch sex values to numeric
new$Sex<-mapvalues(as.character(new$Sex), 
                     c("Male", "Female"), c(0,1))




#Statins
statincols<-c(sprintf("f.20003.0.%s", 0:47))
statincodes<-c(1141146234,1141192414,1140910632,1140888594,1140864592,
	1141146138,1140861970,1140888648,1141192410,
	1141188146,1140861958,1140881748,1141200040)

manyColsToDummy(statincodes, bd_join4[,statincols], "statinoutput")
statinoutput$statins<-rowSums(statinoutput) 
statinoutput$statins[statinoutput$statins>1]<-1

statinoutput$IID<-bd_join4$f.eid

statinoutput<-statinoutput%>%select(IID, statins)

new<-left_join(new, statinoutput, by="IID")

participants1<-new%>%select(IID)
participants1$FID<-participants1$IID
participants1<-participants1%>%select(FID, IID)

###=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
###WRITE OUTPUT=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
###=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

outdir="/scratch/as58810/FallProject/pheno"

#Model 1
write.table(participants1, 
	paste(outdir, "/GWAS_phenoQC_IDS_M1.txt",sep=""), 
	row.names=FALSE, quote=FALSE)

write.table(new, 
	paste(outdir, "/GWAS_pheno_M1.txt", sep=""),
	row.names=FALSE, quote=FALSE)
