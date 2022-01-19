library(tidyverse)

setwd("/scratch/as58810/Fall2021Practice/Project")

source("pheno.R")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

#Add vegetarian columns
vegqc3<-read.delim("vegQC2_04032021.txt", header=TRUE, sep="\t")

#Inner join
new2<-as_tibble(inner_join(new, vegqc3, by="IID"))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

#PCs
pan<-read_tsv("/scratch/as58810/FallProject/all_pops_non_eur_pruned_within_pop_pc_covs.tsv")
pan<-as_tibble(pan)
pan$s<-as.integer(pan$s)
table(pan$pop, useNA = "always")

bridge<-read.table("/scratch/as58810/FallProject/ukb48818bridge31063.txt")
bridge<-as_tibble(bridge)
colnames(bridge)<-c("IID", "panID")

pan2<-pan%>%select(s, pop, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10)%>%
    left_join(bridge, by=c("s"="panID"))

#Inner join
new3<-as_tibble(inner_join(new2, pan2, by="IID"))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

#Complete cases
new4<-new3[complete.cases(new3),]
#removes 26,173
#163,712

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

participants <- new4 %>% select("FID", "IID")
participants$FID<-participants$IID
participants<-participants%>%select(FID, IID)

#Output
outdir="/scratch/as58810/FallProject/pheno"

write.table(participants, 
	paste(outdir, "/GWAS_phenoQC_IDS_M1_Veg.txt", sep=""), 
	row.names=FALSE, quote=FALSE)

write.table(new4, 
	paste(outdir, "/GWAS_pheno_M1_Veg.txt", sep=""),
	row.names=FALSE, quote=FALSE)

write.csv(new4, "GWAS_pheno_M1_Veg.csv", quote=FALSE, row.names=FALSE)
