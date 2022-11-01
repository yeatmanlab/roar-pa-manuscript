# load libraries
library(mirt)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(corrplot)
library(RColorBrewer)
library(stringr)
library(psych)

# Read data
   # 3 subtests
df= # read excel: roarpa_data_overview --> tab results_T_del.partic.
   # 5 subtests
df = # read excel: roarpa_data_overview --> tab results_T_5sub_delparc
   # metadata
metadata = # read excel: CTOPP_scores

###CUTOFF 0.1###
# Correlation between items and percent correct
item.cors = as.data.frame(cor(select(df,-Subject,-Age,-Gender,-Type),rowSums(select(df,-Subject,-Age,-Gender,-Type),dim=1)/(dim(df)[2]-1)))
item.cors <- rename(item.cors,rvaltest=V1)
item.cors$items <- row.names(item.cors)

# Item correlation with CTOPP
# match data types
ctopp.data <- left_join(df,metadata)
# Compute item correlation with CTOPP and put it table
item.cors$rvalctopp <-cor(select(df,-Subject,-Age,-Gender,-Type),ctopp.data$CTOPP_PA_raw, use='pairwise.complete.obs')

# Add threshold 0.1
cor.thresh.pc <- 0.1 # remove items that have low correlation with percent correct
cor.thresh.ctopp <- 0.1 # remove items that have low correlation with ctopp
item.cors$thresh <- as.numeric(item.cors$rvaltest>cor.thresh.pc &
                                 item.cors$rvalctopp>cor.thresh.ctopp |
                                 is.na(item.cors$rvalctopp))
# add subtest names _cutoff 0.1
item.cors$subtest <- row.names(item.cors)
#only keep first 3 letters
item.cors$subtest <- substr(item.cors$subtest, 0, 3)
#Combine with threshold
item.cors$subtest.thresh = interaction(item.cors$subtest,item.cors$thresh)

# Scatter plot of corr values cutoff0.1
gt1 = ggplot(item.cors,aes(x=rvaltest,y=rvalctopp,label=items,colour=subtest.thresh)) +
  geom_text(size=2,alpha=0.7)+
  theme(legend.position='none') +
  scale_color_manual(values = c('red','red','red','darkgreen',"orange","purple","blue","black"))+
  scale_x_continuous('Item correlation - ROAR-PA (5 subtests) proportion correct',c(-.2,0,.2,.4,.6,.8),limits=c(0,0.7))+
  scale_y_continuous('Item correlation - CTOPP-2 (raw scores)',c(-.2,0,.2,.4,.6),limits=c(0,.7)) +
  geom_vline(xintercept=cor.thresh.pc,color='red',linetype ='dashed',alpha =0.5) +
  geom_hline(yintercept=cor.thresh.ctopp,color='red',linetype ='dashed',alpha=0.5)
gt1
ggsave('itemcors0.1.pdf',gt1,width=5,height=5)
ggsave('itemcors0.1.png',gt1,width=5,height=5)


# infit and outfit thresholds (https://www.rasch.org/rmt/rmt83b.htm and https://www.rasch.org/rmt/rmt34e.htm)
##Cutoff 0.1
fit.thresh = c(.6, 1.4)
outliers <- TRUE
maxiter <- 20
iteration <- 0
df_goodRasch <- select(df, all_of(c('Subject',item.cors$items[item.cors$thresh==1])))

##cutoff 0.1
# fit 1PL model and look at fit stats 
while (outliers > 0 & iteration<maxiter){
  iteration <- iteration + 1;
  start.dim <- dim(df_goodRasch)[2]-1
  mR <- mirt(select(df_goodRasch,-Subject), model = 1, itemtype = 'Rasch', guess=0.333) #3AFC. Guess Rate = 0.333
  mR.itemfit = itemfit(mR, fit_stats = 'infit')
  good.items = as.character(mR.itemfit$item[mR.itemfit$infit>fit.thresh[1] & 
                                              mR.itemfit$outfit>fit.thresh[1] &
                                              mR.itemfit$infit<fit.thresh[2] &
                                              mR.itemfit$outfit<fit.thresh[2]])
  outliers <- dim(df_goodRasch)[2] - length(good.items) -1 #-1 because subject was removed in fitting
  # Remove items with low or extreme slope and refit
  df_goodRasch <- select(df_goodRasch, all_of(c('Subject',good.items)))
  end.dim <- dim(df_goodRasch)[2]-1
  print(sprintf('1PL ITERATION %d. STARTED WITH %d ITEMS. %d OUTLIERS REMOVED. %d ITEMS RETAINED.',iteration,start.dim,outliers,end.dim))
}
# Fit final model
mR <- mirt(select(df_goodRasch,-Subject), model = 1, itemtype = 'Rasch', guess=0.333) # 3AFC. Guess Rate = 0.333
mR.coef <- coef(mR,simplify=TRUE, IRTpars = TRUE) # Get coefficients
mR.coef <- arrange(tibble::rownames_to_column(as.data.frame(mR.coef$items),'itemnames'),b)
write.csv(mR.coef,'RaschModelItemnames.csv')

#  Fit 2 parameter IRT model with guess rate of 0.33
outliers <- TRUE
maxiter <- 20
iteration <- 0
df_good <- df_goodRasch
aminmax <- c(.7, Inf)
umin <- 0.9
# Model priors
mm = (
  'F = 1
PRIOR = (1-%d, a1, lnorm, .2, 1)'
)
while (outliers > 0 & iteration<maxiter){
  iteration <- iteration + 1;
  start.dim <- dim(df_good)[2]-1
  mm=1
  m <- mirt(select(df_good,-Subject), model = mm,itemtype = '2PL',guess=0.33,technical=list(NCYCLES=5000)) # 3AFC. Guess Rate = 0.33
  co <- coef(m,simplify=TRUE, IRTpars = TRUE) # Get coefficients
  co <- tibble::rownames_to_column(as.data.frame(co$items),'itemnames')
  ggplot(co, aes(a, b)) + geom_point(size=3)
  ggsave(sprintf('2PL-ModelParams_%d.png',iteration))
  # Remove items with low or extreme slope and refit
  df_good <- select(df_good,c(1, which(co$a>aminmax[1] & co$a<aminmax[2] & co$u>umin)+1)) # +1 to account for subj not being in fitting
  end.dim <- dim(df_good)[2]-1
  outliers <- sum(!(co$a>aminmax[1] & co$a<aminmax[2] & co$u>umin))
  print(sprintf('2PL ITERATION %d. STARTED WITH %d ITEMS. %d OUTLIERS REMOVED. %d ITEMS RETAINED.',iteration,start.dim,outliers,end.dim))
}

# Sort by difficulty
co <- arrange(co,b)
write.csv(co,'RaschModel_2PLWords.csv')
## co <- left_join(co, rename(item.cors,words = items))

# figure out the items that were removed at each stage of IRT
df_preIRT <- select(df, all_of(c('Subject',item.cors$items[item.cors$thresh==1])))
m.2PL <- mirt(select(df_preIRT,-Subject), model = 1, itemtype = '2PL',guess=0.33, technical=list(NCYCLES=500)) 
m.2PL.coef <- coef(m.2PL,simplify=TRUE, IRTpars = TRUE) # Get coefficients
m.2PL.coef <- tibble::rownames_to_column(as.data.frame(m.2PL.coef$items),'itemnames')
# Mark which items from df_preIRT were removed during 1PL and 2PL cleaning
m.2PL.coef$kept <- m.2PL.coef$itemnames %in% co$itemnames
write.csv(m.2PL.coef,'keep.deleteitems.csv')

# Join subtest with kept
# create subtest column
m.2PL.coef$subtest <- substr(m.2PL.coef$itemnames, 0, 3)

#Combine with kept
m.2PL.coef$subtest.kept = interaction(m.2PL.coef$subtest,m.2PL.coef$kept)

# Plot out parameters of 2PL model
gt2 = ggplot(m.2PL.coef,aes(x=b,y=a,label=itemnames,color=subtest.kept)) +
  geom_text(size=2,alpha=0.7)+
  scale_color_manual(values = c('red','red','red','red','red','darkgreen',"orange","purple","blue","black"))+
  theme(legend.position='none') +scale_y_log10()+xlab('Item difficulty')+ylab('Item discriminability')

gt2
ggsave('itemparams2PL.pdf',gt2,width=5,height=5)
ggsave('itemparams2PL.png',gt2,width=5,height=5)
gt <- arrangeGrob(gt1,gt2,nrow=1) 
ggsave('itemanalysis.png',gt,width=10,height=5)
ggsave('itemanalysis.pdf',gt,width=10,height=5)

#  Fit 1 parameter IRT model with guess rate of 0.33 to these same data
m1 <- mirt(select(df_good,-Subject), model = 1,itemtype = 'Rasch', guess=0.33) # 3AFC. Guess Rate = 0.33
co.m1 <- coef(m1,simplify=TRUE, IRTpars = TRUE) # Get coefficients
co.m1 <- arrange(tibble::rownames_to_column(as.data.frame(co.m1$items),'itemnames'),b)
write.csv(co.m1,'RaschModel_2PLItemnamesRasch.csv')

# Histogram of slopes and difficulties
h1 <- ggplot(co, aes(x=a)) + 
  geom_histogram(color="black", fill="white")
h2 <- ggplot(co, aes(x=b)) + 
  geom_histogram(color="black", fill="white")
grid.arrange(h1, h2, nrow = 1)
g <- arrangeGrob(h1,h2, nrow=1) 
ggsave('2PL-ModelParamsHist.png',g)

##plots, see 'type' argument here, https://rdrr.io/cran/mirt/man/plot-method.html
p1 = plot(m,type="info") #test information
p2 = plot(m,type="trace",which.items=c(1,10,20,30)) #test information
p3 = plot(m,type="infotrace",which.items=c(1,10,20)) #test information
grid.arrange(p1,p2,p3, nrow = 1)
g <- arrangeGrob(p1,p2,p3, nrow=1) 
ggsave('2PL-testplots_randitems.png',g)

# Plot item functions
png("itemfunctions.png")
plot(NULL,xlim=c(-4,4),ylim=c(.33,1),xlab='Theta',ylab='P(correct)')
cc<-coef(m)
th<-seq(-4,4,length.out=1000)
for (i in 1:(length(cc)-1)) {
  cc[[i]]->a
  k<-a[1]*(th-a[2])
  ek<-exp(k)
  yv<-a[3]+(1-a[3])*ek/(1+ek)
  lines(th,yv)
}
dev.off()

# Diagnostic plots
png("infoSE.png")
plot(m,type="infoSE") #test information and standard error
dev.off()

png("Precision.png")
plot(m,type="rxx") #test information and standard error
dev.off()

# Put theta estimates for each subject into a dataframe
sub.data <- select(df,Subject)
# Compute and add percent correct
sub.data$pcor <- rowSums(select(df,-Subject,-Age,-Gender,-Type),dim=1)/(dim(df)[2]-1)
# Get subject thetas
sub.data$f1PL <- fscores(mR)
sub.data$f2PL1PL <- fscores(m1)
sub.data$f2PL <- fscores(m)
write.csv(sub.data,'SubjectThetaEstimates.csv')

############################################################################################################
