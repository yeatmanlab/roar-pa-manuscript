library(psych)
library(readxl)
library(dplyr)

# conduct exploratory factor analysis to assess the unidemnsionality of OPAA

roarpa = roarpa_data_overview # tab results_T_del.partic.
cor_matrix <- cor(select(roarpa,-Subject,-Age,-Gender,-Type), use = "complete.obs")


one.factor.efa<-fa(cor_matrix,nfactors=1, rotate="promax", scores="regression", residuals=FALSE, 
                   SMC=FALSE, covar=FALSE,
                   missing=FALSE,impute="median",
                   min.err = 0.001,max.iter = 50,
                   symmetric=TRUE, warnings=TRUE, 
                   fm="pa",use="pairwise",cor="cor")

two.factor.efa<-fa(cor_matrix,nfactors=2, rotate="promax", scores="regression", residuals=FALSE, 
                   SMC=FALSE, covar=FALSE,
                   missing=FALSE,impute="median",
                   min.err = 0.001,max.iter = 50,
                   symmetric=TRUE, warnings=TRUE, 
                   fm="pa",use="pairwise",cor="cor")

three.factor.efa<-fa(cor_matrix,nfactors=3, rotate="promax", scores="regression", residuals=FALSE, 
                   SMC=FALSE, covar=FALSE,
                   missing=FALSE,impute="median",
                   min.err = 0.001,max.iter = 50,
                   symmetric=TRUE, warnings=TRUE, 
                   fm="pa",use="pairwise",cor="cor")


four.factor.efa<-fa(cor_matrix,nfactors=4, rotate="promax", scores="regression", residuals=FALSE, 
                     SMC=FALSE, covar=FALSE,
                     missing=FALSE,impute="median",
                     min.err = 0.001,max.iter = 50,
                     symmetric=TRUE, warnings=TRUE, 
                     fm="pa",use="pairwise",cor="cor")


one <- loadings(one.factor.efa)
two <- loadings(two.factor.efa)
three <- loadings(three.factor.efa)

all <- cbind(one, two, three)

all <- as.data.frame(all)

colnames(all) <- c("m1.f1", "m2.f1", "m2.f2", "m3.f2", "m3.f1", "m3.f3")
all <- all %>% dplyr::select(m1.f1, m2.f1, m2.f2, m3.f1, m3.f2, m3.f3)


# scree plot
set.seed(1234)  # set the seed so that you can reproduce your parallel analysis exactly
fa.parallel(cor_matrix, n.obs = nrow(opaa), fm="minres", fa="both", 
            main = "Parallel Analysis Scree Plots",
            n.iter=100, error.bars=FALSE, SMC=FALSE,
            ylabel=NULL, show.legend=TRUE)
