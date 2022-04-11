#installing curatedMetagenomicData
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("curatedMetagenomicData")

library(curatedMetagenomicData)
#documentation
browseVignettes("curatedMetagenomicData")
#getting the data, can replace "LeChatelierE_2013" with "NielsenHB_2014" or "FengQ_2015" to look at the other datasets
data <- curatedMetagenomicData("LeChatelierE_2013.metaphlan_bugs_list.stool", dryrun = FALSE)
data.eset <- data[[1]]
#looking for a specific species in the data set
grep("obeum", rownames(data.eset), value=TRUE)
#gives the correlation value of the abundance of all the species of bacteriain the dataset against BMI
sink("LeChatelierE_2013.csv")
print("species, pearson correlation to BMI, P-value for pearson, Spearman correlation to BMI, P-value for spearman correlation")
for  (i in rownames(data.eset))
{
  x = exprs( data.eset )[i, ]
  
  y = as.data.frame(x)
  
  df <- data.frame(data.eset$subjectID, data.eset$BMI)
  
  graph = merge(y, df, by.x="row.names", by.y="data.eset.subjectID")
  #spearman correlation
  tst = cor.test(x=graph$data.eset.BMI, y=graph$x, method = 'spearman', exact = FALSE)
  #pearson correlation
  res <- cor.test(graph$data.eset.BMI, graph$x, method = "pearson")
  
  my_data = paste(i, res$statistic, res$p.value, tst$estimate, tst$p.value, sep = ",")
  
  print(my_data)
}
sink()
#used to look at one species in particular and plot the graph of it, can replace "t__Ruminococcus_obeum_unclassified" with any entry of species in the dataset
x = exprs( data.eset )[grep("t__Ruminococcus_obeum_unclassified$", rownames(data.eset)), ]

y = as.data.frame(x)

df <- data.frame(data.eset$subjectID, data.eset$BMI)

dff <- data.frame(y, df)

graph = merge(y, df, by.x="row.names", by.y="data.eset.subjectID")
#generates graph
plot(graph$data.eset.BMI, graph$x, xlab = "BMI", ylab = "abundance of species")
#pearson correlation
print(cor.test(graph$data.eset.BMI, graph$x, method = "pearson"))
#spearman correlation
pinrt(cor.test(x=graph$data.eset.BMI, y=graph$x, method = 'spearman', exact = FALSE))
#linear regression
print(lm(x~data.eset.BMI , data = dff))
