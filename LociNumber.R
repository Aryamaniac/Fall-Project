dirs = list.dirs(dir, full.names = T)[-1]
tables<-list()

# Read in FUMA and label --------------------------------------------------


for (d in 1:length(dirs)) {
  pheno <- str_split(dirs[d], "/", simplify = T)[,7]
  #print(paste(dir, "/", pheno, "/GenomicRiskLock.txt", sep=""))
  
  tables[[d]] = as_tibble(read.table(paste(dir, "/", pheno, "/GenomicRiskLoci.txt", sep=""), header = T))
  tables[[d]]$Phenotype = pheno
  tables[[d]]=tables[[d]]%>%select(Phenotype, everything())
}

combines = bind_rows(tables)

#Loci designnation function -----------------------------------------------
sorted = as_tibble(combines[with(combines, order(chr, start)),])
sorted$LociNumber = 1
i = 1
window = 250 * 1000
for (d in 2:length(sorted$chr)) {
  if (sorted[d, 5] != sorted[d-1, 5]) {
    i = i + 1
    sorted[d, 16] = i
  } else {
    if ((sorted[d, 8] > sorted[d-1, 8] & sorted[d,8] < sorted[d-1, 9]) | sorted[d,8] - sorted[d-1, 9] < window) {
      #print(paste("true", d))
    #if (sorted[d,8] > sorted[d-1, 8] & sorted[d,8] < sorted[d-1,9]) {
      sorted[d,16] = i
    } else {
      i = i + 1
      sorted[d, 16] = i
    }
  }
}
