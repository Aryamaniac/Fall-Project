library(tidyverse)

pheno <- read.csv("/scratch/as58810/FallProject/phenoComplete/LipidsxVeg_pheno.csv", header=TRUE)

pheno<-as_tibble(pheno)

outdir = "/scratch/as58810/Fall2021Practice/Project/genoQC2/PCA/phenos"

for (i in 1:22) {
PCA <- as_tibble(read.table(paste("/scratch/as58810/Fall2021Practice/Project/genoQC2/PCA/chr", i, "PCA.eigenvec", sep=""), header=FALSE))

colnames(eigenPCA) <- c("FID", "IID", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")

phenocombined <- left_join(pheno, PCA,  by=c("IID"))

write.csv(phenocombined, paste(outdir, "/GWAS_pheno_M1_Veg_eigen", i, ".csv", sep=""),row.names=FALSE, quote=FALSE)
}
  
