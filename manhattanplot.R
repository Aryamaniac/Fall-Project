library(tidyverse)
library(qqman)
library(manhattan)

outdir = "GMPlots/" 

var1 = 'TotalCholesterol'
#var1 = 'LDLCholesterol'
#var1 = 'HDLCholesterol'
#var1 = 'Triglycerides'
#var2 = 'Self_Reported_Vegetarian_plus_strict_initial_and24-chr' 
var2 = 'Consistent_Self_Reported_Vegetarian_across_all_24hr-chr'


for (i in 1:11) {
raw<-as_tibble(read.table(paste("GEM", var1, "/", var1, "x", var2, i, sep=""), sep="/"), header=TRUE, stringsAsFactors=FALSE)

mandata<-infile %>% select(c('RSID', 'CHR', 'POS', 'robust_P_Value_Joint'))
colnames(mandata)<-c('rsid', 'chrom', 'pos', 'pval')
mandata %>% mutate(y = -log10(pval))

if (i ==1) {
finalman <- mandata
} else {
finalman<-rbind(finalman, mandata)

}

#find significant SNPS
finalman <- filter(finalman, color = ifelse(pval<=5*10^-8, 'red', NA)
finalman <- filter(finalman, label = ifelse(pval<=5*10^-8, rsid, NA)

manhattan(finalman, build='hg19', color1='skyblue', color2='navyblue')+geom_hline(yintercept=-log10(5e-8),color='red')+geom_text(aes(label=label),hjust=-0.1)

outputname<-paste(outdir, var1, "x", var2, "ManPlot.png", sep = "")



