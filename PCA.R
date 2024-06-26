library(readxl)
PCA_stock <- read_excel("3.2/Applied Analytics/PCA_stock.xlsx")
View(PCA_stock)
pca<-read_excel("3.2/Applied Analytics/PCA_stock.xlsx")
colnames(pca) = c("AnnualReturn", "ExcessReturn", "SystematicRisk", "TotalRisk", "AbsWinRate", "RelWinRate")
head(pca)
tail(pca)

#creation of a correlation matrix
#pca correlation below
pcacor<-cor(pca)
pcacor
#variance covarinace matrix 
pcacov<-cov(pca)
pcacov
#checking for statistical significance of the bivariate correlations
library(psych)
statSig <- corr.p(pcacor, 252, alpha=.05)
print(statSig, short = FALSE) 
# short=FALSE prints confidence intervals

#verification of assumptions
#first test for Sphericity and Sample adequacy using paf()function
#it performs both Bartlett and KMO tests

# Bartlett's test for sphericity
bartlett_test <- cortest.bartlett(pcacor, n = nrow(pca))
print("Bartlett's Test for Sphericity:")
print(bartlett_test)

# Kaiser-Meyer-Olkin (KMO) test for sampling adequacy
kmo_result <- KMO(pca)
print("Kaiser-Meyer-Olkin (KMO) Test:")
print(kmo_result)

#bartlett significance test using correlation matrix
cortest.bartlett(pcacor, n=252)

#low p-vaues proves statistical sig,indicating that correlations arent near zero
#we can therefore calculate matrix determinant to ensure its positive
# Verify that the determinant is positive
det(pcacor)
#positive det concludes the test allowing us to proceed with pca modelling

#COMPLETE PCA
#Access the components using the principal function
pca1Component <- principal(pcacor, rotate="none")
# default with 1 component and no scores
#.....notes

pca1Component
pca6Components <- principal(pcacor, nfactors=6, rotate="none")
#we calculate all 6 components
pca6Components
#....notes
alpha(pcacor) 
#do something

#result visualization
fa.parallel(pca,n.obs=252, fm="pa", fa="pc")
# Parallel analysis suggests that the number of factors =  NA  and the number of components =  2
fa.diagram(pca6Components)
#....some notes here

#factor representations
# Use data instead of correlation matrix
pca.results <- principal(pca,nfactors=6,rotate='none')
pca.results$scores
#now correlate the scores to check for independence
cor(pca.results$scores) 
#these should be orthogonal
round(cor(pca.results$scores)) 
#round off for better visualization

#end of code
