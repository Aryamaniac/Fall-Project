setwd("/scratch/as58810/FallProject")

### This is a script for generating lists of UK Biobank participants which meet QC/filtering criteria.
source("/scratch/as58810/FallProject/load_UKBphenotables.R")

pan<-read_tsv("/scratch/as58810/FallProject/all_pops_non_eur_pruned_within_pop_pc_covs.tsv")
pan<-as_tibble(pan)
pan$s<-as.integer(pan$s)
table(pan$pop, useNA = "always")

bridge<-read.table("/scratch/as58810/FallProject/ukb48818bridge31063.txt")
bridge<-as_tibble(bridge)
colnames(bridge)<-c("IID", "panID")

pan2<-pan%>%select(s, pop)%>%left_join(bridge, by=c("s"="panID"))

##===============================PART A=================================================
#Generate a list of participants who pass the following QC criteria:
#1. Genetic ethnicity = Caucasian VIA PAN UKBB
#2. Not an outlier for heterogeneity and missing genotype rate (poor quality genotype)
#3. No Sex chromosome aneuploidy
#4. Self-reported sex matches genetic sex
#5. Do not have high degree of genetic kinship (Ten or more third-degree relatives identified)
#6. Does not appear in "maximum_set_of_unrelated_individuals.MF.pl"

bd_QC<- bd %>% select(f.eid, f.31.0.0, f.22001.0.0, f.21000.0.0, f.22027.0.0, f.22019.0.0, f.22021.0.0)

colnames(bd_QC)<-c("IID", "Sex", "Genetic_Sex", "Race","Outliers_for_het_or_missing", "SexchrAneuploidy","Genetic_kinship")

#Join UKB cols with with Pan UKBB
bd_QC<-as_tibble(bd_QC) #502,527
#nrow(bd_QC) #[1] 502527
bd_QC<-bd_QC%>%inner_join(pan2, by="IID")

#Filter by Genetic ethnicity = Caucasian VIA PAN UKBB
bd_QC<-bd_QC[bd_QC$pop=="EUR",]
#nrow(bd_QC) #[1] 426881

bd_QC<-bd_QC%>%filter(is.na(Outliers_for_het_or_missing) | Outliers_for_het_or_missing !="Yes")
#nrow(bd_QC) #[1] 426433

bd_QC<-bd_QC%>%filter(is.na(SexchrAneuploidy) | SexchrAneuploidy != "Yes")
#nrow(bd_QC) #[1] 425854


bd_QC<- bd_QC%>%filter(is.na(Genetic_kinship) | Genetic_kinship != "Ten or more third-degree relatives identified")

#If Sex does not equal genetic sex, exclude participant
bd_QC<-bd_QC[bd_QC$Sex == bd_QC$Genetic_Sex,]
#nrow(bd_QC) #[1] 425683

#Filter related file by those in QC
relatives<-read.table("ukb48818_rel_s488282.dat", header=T)


#From maximum_set_of_unrelated_individuals.MF.pl output:
max_unrelated<-read.table("ukb48818_rel_s488282_output.dat")
max_unrelated<-as.integer(unlist(max_unrelated))
bd_QC<-bd_QC%>%filter(!IID %in% max_unrelated)

table(bd_QC$Race)

QCkeepparticipants<-bd_QC%>%select(IID)

write.table(QCkeepparticipants, file= "/scratch/as58810/FallProject/bd_QC-keep.txt", row.names = FALSE, quote = FALSE)
