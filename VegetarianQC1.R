library(plyr)
library(dplyr)
library(tidyverse)

setwd("/scratch/as58810/FallProject")

#The key of this script is solving the problem that if someone has an
#NA in field 20086 (24hr-recall: special diet followed), it could be 
#because they did not answer this question, or because they didn't 
#take the 24h recall. This parses out the answer to that question.

#Load UK Biobank datasets-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
source('/scratch/as58810/FallProject/ukb34137_loaddata.r') #15 min
bd <- as_tibble(bd)

#UKB data for people who took the 24hour recall survey
bd24<-as_tibble(read.table("UKB_24hourRecall-ParticipantInstancesTaken.txt", header=TRUE, stringsAsFactors = FALSE))
#sum(bd24$ever_took_24HR)#[1] 211018

#24hr special diet columns
SpecialDiet<-bd%>%select(f.eid, f.20086.0.0, f.20086.1.0, f.20086.2.0, f.20086.3.0, f.20086.4.0)

#Column names
colnames(SpecialDiet)<- c("IID", "Veg0","Veg1", "Veg2", "Veg3", "Veg4")
table(SpecialDiet$Veg0) 


#***Use sapply here to perform repeated functions over columns***
#Code columns so vegetarian or vegan = 1, other = 0
SpecialDiet[,2:6]<-sapply(SpecialDiet[,2:6], as.character)
SpecialDiet[,2:6]<-sapply(SpecialDiet[,2:6], mapvalues, c(NA, "Low calorie", "Gluten-free", "Lactose-free", "Other", "Vegetarian", "Vegan"), c(0,0,0,0,0,1,1))

###This section creates a table that has both answers to special diet
###columns (Veg0-4) and columns indicating whether people took the
###24hr survey (took0-4)

TooksurveyAndVeg <- left_join(SpecialDiet, bd24,  by=c("IID"))
TooksurveyAndVeg<-as_tibble(TooksurveyAndVeg)
TooksurveyAndVeg<-TooksurveyAndVeg[TooksurveyAndVeg$ever_took_24HR==1,]
TooksurveyAndVeg<-TooksurveyAndVeg%>%select(-ever_took_24HR)

colnames(TooksurveyAndVeg)[7:11]<-c("took0", "took1", "took2", "took3", "took4")
#nrow(TooksurveyAndVeg) #[1] 211018

#TooksurveyAndVeg[!complete.cases(TooksurveyAndVeg),] #0

#initialize answer columns
TooksurveyAndVeg$Answer0<-"d" #column 12
TooksurveyAndVeg$Answer1<-"d" #column 13
TooksurveyAndVeg$Answer2<-"d" #column 14
TooksurveyAndVeg$Answer3<-"d" #column 15
TooksurveyAndVeg$Answer4<-"d" #column 16

#There are 4 conditions here:
#a) took survey, was vegetarian
#b) took survey, wasn't vegetarian
#c) didn't take survey in this instance
#d) code failed to update answer

#This loop takes between 10 and 20 minutes

for(instance in 7:11){ #columns 7 through 11 are the "took" columns
    for(row in 1:nrow(TooksurveyAndVeg)){ #loop through every row in the table
        if(TooksurveyAndVeg[row, instance] == 1){ #if they took the survey in this instance
            if(TooksurveyAndVeg[row, instance-5]==1){ #if they had 1 in the same instance of veg
                TooksurveyAndVeg[row, instance+5]<-"a" 
            }
            else TooksurveyAndVeg[row, instance+5]<-"b"
        }
        else(TooksurveyAndVeg[row, instance+5]<-"c")
    }
}


TooksurveyAndVeg
#adding column for diet 
TooksurveyAndVeg$Diet0<-"0"
TooksurveyAndVeg$Diet1<-"0"
TooksurveyAndVeg$Diet2<-"0"
TooksurveyAndVeg$Diet3<-"0"
TooksurveyAndVeg$Diet4<-"0"

#********************************************************************
#set b's to -100 because they once answered that they were not 
#vegetarian in the survey; we are only looking to designate people 
#with 100% of answers as yes vegetarian

TooksurveyAndVeg


TooksurveyAndVeg$Diet0[TooksurveyAndVeg$Answer0=="a"]<-1
TooksurveyAndVeg$Diet0[TooksurveyAndVeg$Answer0=="b"]<-(-100)
TooksurveyAndVeg$Diet0[TooksurveyAndVeg$Answer0=="c"]<-0
TooksurveyAndVeg$Diet1[TooksurveyAndVeg$Answer1=="a"]<-1
TooksurveyAndVeg$Diet1[TooksurveyAndVeg$Answer1=="b"]<-(-100)
TooksurveyAndVeg$Diet1[TooksurveyAndVeg$Answer1=="c"]<-0
TooksurveyAndVeg$Diet2[TooksurveyAndVeg$Answer2=="a"]<-1
TooksurveyAndVeg$Diet2[TooksurveyAndVeg$Answer2=="b"]<-(-100)
TooksurveyAndVeg$Diet2[TooksurveyAndVeg$Answer2=="c"]<-0
TooksurveyAndVeg$Diet3[TooksurveyAndVeg$Answer3=="a"]<-1
TooksurveyAndVeg$Diet3[TooksurveyAndVeg$Answer3=="b"]<-(-100)
TooksurveyAndVeg$Diet3[TooksurveyAndVeg$Answer3=="c"]<-0
TooksurveyAndVeg$Diet4[TooksurveyAndVeg$Answer4=="a"]<-1
TooksurveyAndVeg$Diet4[TooksurveyAndVeg$Answer4=="b"]<-(-100)
TooksurveyAndVeg$Diet4[TooksurveyAndVeg$Answer4=="c"]<-0



#********************************************************************

Diet<-TooksurveyAndVeg%>%select(IID, Diet0, Diet1, Diet2, Diet3, Diet4) #subsetting information


Diet<-Diet%>%mutate_if(is.character, as.numeric) #changing Diet to numeric 
#this above line was necessary because these rows were initialized 
#with "o"'s (a character) instead of "0"'s (a number)

Diet$row_sum = rowSums(Diet[,c(2,3,4,5,6)]) #sum of the rows

Diet%>%filter(row_sum== 0) #nobody. good.

Diet$Veg<-"a" #creating column for final answer


Diet$Veg[Diet$row_sum < 0]<-"Non-vegetarian"
Diet$Veg[Diet$row_sum > 0]<-"Vegetarian"

Diet%>%filter(Veg== "Vegetarian") #5,738
Diet%>%filter(Veg== "Non-vegetarian") #205,280
5738/205280#[1] 0.02795207


Diet$Vegb<-0 #creating column for final answer

Diet$Vegb[Diet$Veg == "Non-vegetarian" ]<-0
Diet$Vegb[Diet$Veg ==  "Vegetarian" ]<-1

sum(Diet$Vegb) #[1] 3321

veg<-Diet%>%select("IID", "Vegb")

colnames(veg)<-c("IID", "Consistent_Self-Reported_Vegetarian_across_all_24hr")

write.table(veg, file = "/scratch/as58810/FallProject/vegQC1_04032021.txt", sep = "\t", col.names = TRUE, quote = FALSE, row.names = FALSE)
