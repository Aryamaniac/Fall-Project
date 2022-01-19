#This script starts with the input of VegetarianQC1.R
#and does another round of QC where specific answers
# from both initial food survey and 24HR are checked to make sure
# no meat was reported as eaten recently.

library(plyr)
library(dplyr)
library(tidyverse)

vegqc1<-read.delim("vegQC1_04032021.txt", header=TRUE, sep="\t")
vegqc1<-as_tibble(vegqc1)

table(vegqc1$Consistent_Self.Reported_Vegetarian_across_all_24hr) 
#starting= 5738/ consistent self-reported vegetarians across 24hr
nrow(vegqc1) #211018 --all people took the 24hr

#Load UK Biobank datasets-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
source('ukb34137_loaddata.r') #15 min
bd <- as_tibble(bd)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=
#First check the 24hr columns for "Meat consumer" and "fish consumer"
#Across all instances where the 24hr was taken.
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=

#UKB data for people who took the 24hour recall survey
#From UKB_getparticipants_24hourrecall.R
bd24<-as_tibble(read.table("UKB_24hourRecall-ParticipantInstancesTaken.txt",
                           header=TRUE, stringsAsFactors = FALSE))

bd24

Meatfish24<-bd%>%select(f.eid, 
                        #Category 100106: Meat/fish yesterday. 24HR.
                        f.103000.0.0, f.103000.1.0, f.103000.2.0, 
                        f.103000.3.0, f.103000.4.0, 
                        
                        f.103140.0.0, f.103140.1.0, f.103140.2.0,
                        f.103140.3.0, f.103140.4.0)


colnames(Meatfish24)<-c("IID", "Meat0", "Meat1","Meat2","Meat3","Meat4",
                        "Fish0","Fish1","Fish2","Fish3","Fish4")

TooksurveyAndMeat<- left_join(Meatfish24, bd24,  by=c("IID"))
TooksurveyAndMeat<-as_tibble(TooksurveyAndMeat)
TooksurveyAndMeat<-TooksurveyAndMeat[TooksurveyAndMeat$ever_took_24HR==1,]
TooksurveyAndMeat<-TooksurveyAndMeat%>%select(-ever_took_24HR)

TooksurveyAndMeat[,2:11]<-sapply(TooksurveyAndMeat[,2:11], 
                                 mapvalues, c("No", "Yes"), c(0, 1))

TooksurveyAndMeat$Meatfish0<-0 #column 12
TooksurveyAndMeat$Meatfish1<-0 #column 13
TooksurveyAndMeat$Meatfish2<-0 #column 14
TooksurveyAndMeat$Meatfish3<-0 #column 15
TooksurveyAndMeat$Meatfish4<-0 #column 16

#There are 3 conditions here:
#1 = took survey, didn't say they eat meat or fish
#-100 = took survey, did say they eat meat or fish
#0 didn't take survey in this instance


#This loop takes between 10 and 20 minutes start 5:14
for(instance in 12:16){ #columns 12 through 16 are the "took" columns
    for(row in 1:nrow(TooksurveyAndMeat)){ #loop through every row in the table
        if(TooksurveyAndMeat[row, instance] == 1){ #if they took the survey in this instance
            if((TooksurveyAndMeat[row, instance-10]==0) & (TooksurveyAndMeat[row, instance-5]==0))
                { #if they had 0 in the same instance of Meat AND in Fish
                TooksurveyAndMeat[row, instance+5]<-1
                }
            else TooksurveyAndMeat[row, instance+5]<-(-100)
        }
        else(TooksurveyAndMeat[row, instance+5]<-0)
    }
}

Diet<-TooksurveyAndMeat%>%select(IID, Meatfish0, Meatfish1, Meatfish2, 
                                 Meatfish3, Meatfish4)

Diet$row_sum = rowSums(Diet[,c(2,3,4,5,6)]) #sum of the rows
Diet$Meatfish[Diet$row_sum > 0]<-0 #didn't eat meat or fish
Diet$Meatfish[Diet$row_sum < 0]<-1 #did eat meat or fish

#negative number = ate meat or fish in these columns.
MF<-Diet%>%select(IID, Meatfish)
vegqc2<-left_join(vegqc1, MF, by="IID")
vegqc2[vegqc2$Consistent_Self.Reported_Vegetarian_across_all_24hr==1 & vegqc2$Meatfish==1,] #563 ppl

#Get number of people identified by this QC check:: 563


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=
#Second check with data from initial assessment for those who ate meat
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=

#selecting data to tibble
new<-bd%>%select(f.eid, 
                    #Category 100052: Diet. Initial assessment
                    f.1329.0.0, f.1339.0.0, 
                    f.1349.0.0, f.1359.0.0, 
                    f.1369.0.0, f.1379.0.0, 
                    f.1389.0.0
                    )

#column names for tibble
colnames(new)<- c("IID", 
                  #Category 100052: Diet. Initial assessment
                  "Oily_fish_intake", "Non_oily_fish_intake",
                  "Processed_meat_intake","Poultry_intake",
                  "Beef_intake","Lamb.mutton_intake",
                  "Pork_intake"
)


#Category 100052: keep only those who answered "No"
#Category 100106: keep only those who answered "No"

#Combine diet data with result from VegetarianQC1.R
(new<-left_join(vegqc1, new, by="IID"))

new[,3:9]<-sapply(new[,3:9], as.character)
nrow(new) #[1] 211018
sapply(new, function(x) sum(is.na(x)))
#85 people did not answer to the Initial assessment diet questions.
#Remove them here:
new<-new[!is.na(new$Pork_intake),]
nrow(new) #[1] 210933
new$strict_initial<-0

#QC for diet data
new$strict_initial[(new$Oily_fish_intake =="Never") &
    (new$Non_oily_fish_intake =="Never") &
    (new$Processed_meat_intake =="Never") &
    (new$Poultry_intake =="Never") &
    (new$Beef_intake=="Never") &
    (new$Lamb.mutton_intake =="Never") &
    (new$Pork_intake =="Never")] <-1

sum(new$strict_initial) #[1] 4749
strictinitaltable<-new%>%select(IID, strict_initial)
colnames(vegqc2)[colnames(vegqc2)=="Meatfish"]<-"Meatfish24"
vegqc2<-inner_join(vegqc2, strictinitaltable, by="IID")
vegqc2$strict_initial_and24<-0
vegqc2$strict_initial_and24[(vegqc2$Consistent_Self.Reported_Vegetarian_across_all_24hr==1)&(vegqc2$strict_initial==1)&(vegqc2$Meatfish24==0)]<-1

sum(vegqc2$strict_initial_and24)#3784

vegqc3<-vegqc2%>%select(IID, Consistent_Self.Reported_Vegetarian_across_all_24hr, strict_initial_and24)


colnames(vegqc3)<-c("IID", "Consistent_Self_Reported_Vegetarian_across_all_24hr", "Self_Reported_Vegetarian_plus_strict_initial_and24")
table(vegqc3$Consistent_Self_Reported_Vegetarian_across_all_24hr, useNA = "always")
#      0      1   <NA> 
#   205200   5733      0

table(vegqc3$Self_Reported_Vegetarian_plus_strict_initial_and24, useNA = "always")
#     0      1   <NA> 
#   207149   3784      0 


write.table(vegqc3, file = "vegQC2_04032021.txt", sep = "\t", col.names = TRUE, quote = FALSE, row.names = FALSE)
