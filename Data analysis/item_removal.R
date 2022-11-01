# set libraries
library(kableExtra)
library(CTT)
library(psych)
library(ggplot2)
library(dplyr)
library(dplyr)
library(corrplot)
library(WrightMap)
library(mirt)
library(kableExtra)
library(readxl)
library(stringr)

#First Sound Matching
roarpa <- roarpa_data_overview #tab res_trans_FSM
ctopp <- ctopp_scores # tab raw scores

roarpa <- subset(roarpa, roarpa$Age <= 9.50)

fsm = subset(roarpa, select=-c(Gender,Type,Age))

ia <- function(df, offset)
  # return item analysis report
{
  ia <- itemAnalysis(as.data.frame(df[offset]))
  
  return(ia)
}

t <- ia(fsm, -c(1))

ncor <- rowSums(fsm[-c(1)])
ncor <- cbind.data.frame("Subject" = fsm$Subject, "fsm_total" = ncor)
c_o <- merge(ctopp, ncor, by = "Subject", all.b = T)
c1 <- cor(c_o$CTOPP_PA_raw, c_o$fsm_total)


par(mfrow = c(2, 3)) 

total <- barplot(table(ncor$fsm_total), xlab = "Total score",col="purple")
text(x = 5, y = 40, labels = paste("α = ", round(t$alpha, 2)), xpd=T)
text(x = 5, y = 45, labels = paste("cor(FSM, CTOPP-2 (raw)) =  ", round(c1, 2)), xpd=T)
hist(t$itemReport$itemMean, main = "All Items", xlab = "% correct",col="purple") # distribution of p.values
hist(t$itemReport$pBis, main = "", xlab = "Point-biserial",col="purple")

fsm_updated = subset(fsm, select=-c(FSM_2,FSM_3,FSM_9,FSM_13,FSM_19,FSM_20))



f <- ia(fsm_updated, -c(1))
ncor2 <- rowSums(fsm_updated[-c(1)])
ncor2 <- cbind.data.frame("Subject" = fsm_updated$Subject, "fsm_total" = ncor2)
c_o_2 <- merge(ctopp, ncor2, by = "Subject", all.b = T)
c2 <- cor(c_o_2$CTOPP_PA_raw, c_o_2$fsm_total)


barplot(table(ncor2$fsm_total), xlab = "Total score",col="darkmagenta")
text(x = 5, y = 60, labels = paste("cor(FSM, CTOPP-2(raw)) =  ", round(c2, 2)), xpd=T)
text(x = 5, y = 55, labels = paste("α = ", round(f$alpha,2)), xpd=T)



hist(f$itemReport$itemMean, main = "Six Items Removed", xlab = "% correct",col="darkmagenta") # distribution of p.values
hist(f$itemReport$pBis, main = "",  xlab = "Point-biserial",col="darkmagenta") 

itemreport  <- t$itemReport
items <- subset(itemreport, itemreport$itemName == "FSM_2" |
                  itemreport$itemName == "FSM_3"|
                  itemreport$itemName == "FSM_9" |
                  itemreport$itemName == "FSM_13" |
                  itemreport$itemName == "FSM_20" |
                  itemreport$itemName == "FSM_19")



items <- items %>% select("Item" = itemName, "Item Mean" = itemMean, "Point-Biserial" = pBis)

kable(items, digits=2)  %>%
  kable_styling() %>% 
  scroll_box(width = "500px", height = "200px")

# item difficulty
rm_fsm <- mirt(fsm_noSubj, 1, itemtype = "Rasch", guess=0.333, technical=list(NCYCLES=5000))
fullscores <- fscores(rm_fsm)
b <- as.data.frame(coef(rm_fsm, IRTpars=TRUE, simplify=TRUE)$items[,2])

wrightMap( fullscores, b$`coef(rm_fsm, IRTpars = TRUE, simplify = TRUE)$items[, 2]`, dim.name = "", item.side = itemClassic)


#Last Sound Matching
roarpa <- roarpa_data_overview #tab res_trans_LSM
ctopp <- ctopp_scores # tab raw scores


roarpa <- subset(roarpa, roarpa$Age <= 9.50)

lsm = subset(roarpa, select=-c(Gender,Type,Age))

ia <- function(df, offset)
  # return item analysis report
{
  ia <- itemAnalysis(as.data.frame(df[offset]))
  
  return(ia)
}

t <- ia(lsm, -c(1))

ncor <- rowSums(lsm[-c(1)])
ncor <- cbind.data.frame("Subject" = lsm$Subject, "lsm_total" = ncor)
c_o <- merge(ctopp, ncor, by = "Subject", all.b = T)
c1 <- cor(c_o$CTOPP_PA_raw, c_o$lsm_total)


par(mfrow = c(2, 3)) 

total <- barplot(table(ncor$lsm_total), xlab = "Total score",col="blue")
text(x = 10, y = 20, labels = paste("α = ", round(t$alpha, 2)), xpd=T)
text(x = 10, y = 25, labels = paste("cor(LSM, CTOPP-2(raw)) =  ", round(c1, 2)), xpd=T)
hist(t$itemReport$itemMean, main = "All Items", xlab = "% correct",col="blue") # distribution of p.values
hist(t$itemReport$pBis, main = "", xlab = "Point-biserial",col="blue")

# akb: remove these items
lsm_updated = subset(lsm, select=-c(LSM_3,LSM_9,LSM_12,LSM_14,LSM_23,LSM_25))


f <- ia(lsm_updated, -c(1))
ncor2 <- rowSums(lsm_updated[-c(1)])
ncor2 <- cbind.data.frame("Subject" = lsm_updated$Subject, "lsm_total" = ncor2)
c_o_2 <- merge(ctopp, ncor2, by = "Subject", all.b = T)
c2 <- cor(c_o_2$CTOPP_PA_raw, c_o_2$lsm_total)


barplot(table(ncor2$lsm_total), xlab = "Total score",col="darkblue")
text(x = 10, y = 30, labels = paste("cor(FSM, CTOPP-2(raw)) =  ", round(c2, 2)), xpd=T)
text(x = 10, y = 25, labels = paste("α = ", round(f$alpha,2)), xpd=T)



hist(f$itemReport$itemMean, main = "Six Items Removed", xlab = "% correct",col="darkblue") # distribution of p.values
hist(f$itemReport$pBis, main = "",  xlab = "Point-biserial",col="darkblue") 

itemreport  <- t$itemReport
items <- subset(itemreport, itemreport$itemName == "LSM_12" |
                  itemreport$itemName == "LSM_23" |
                  itemreport$itemName == "LSM_9" |
                  itemreport$itemName == "LSM_25" |
                  itemreport$itemName == "LSM_3" |
                  itemreport$itemName == "LSM_14")



items <- items %>% select("Item" = itemName, "Item Mean" = itemMean, "Point-Biserial" = pBis)

kable(items, digits=2)  %>%
  kable_styling() %>% 
  scroll_box(width = "500px", height = "200px")

rm_lsm <- mirt(lsm_noSubj, 1, itemtype = "Rasch", guess=0.333, technical=list(NCYCLES=5000))
fullscores <- fscores(rm_lsm)
b <- as.data.frame(coef(rm_lsm, IRTpars=TRUE, simplify=TRUE)$items[,2])

wrightMap( fullscores, b$`coef(rm_lsm, IRTpars = TRUE, simplify = TRUE)$items[, 2]`, dim.name = "", item.side = itemClassic)



#DELETION
roarpa <- roarpa_data_overview #tab res_trans_DEL
ctopp <- ctopp_scores # tab raw scores


roarpa <- subset(roarpa, roarpa$Age <= 9.50)

del = subset(roarpa, select=-c(Gender,Type,Age))

ia <- function(df, offset)
  # return item analysis report
{
  ia <- itemAnalysis(as.data.frame(df[offset]))
  
  return(ia)
}

t <- ia(del, -c(1))

ncor <- rowSums(del[-c(1)])
ncor <- cbind.data.frame("Subject" = del$Subject, "del_total" = ncor)
c_o <- merge(ctopp, ncor, by = "Subject", all.b = T)
c1 <- cor(c_o$CTOPP_PA_raw, c_o$del_total)


par(mfrow = c(2, 3)) 


total <- barplot(table(ncor$del_total), xlab = "Total score",col="orange")
text(x = 10, y = 25, labels = paste("α = ", round(t$alpha, 2)), xpd=T)
text(x = 10, y = 30, labels = paste("cor(DEL, CTOPP-2(raw) =  ", round(c1, 2)), xpd=T)
hist(t$itemReport$itemMean, main = "All Items", xlab = "% correct", col="orange") # distribution of p.values
hist(t$itemReport$pBis, main = "", xlab = "Point-biserial",col="orange")



# akb: remove these items
del_updated = subset(del, select=-c(Deletion_2,Deletion_12,Deletion_19,Deletion_23,Deletion_24))


f <- ia(del_updated, -c(1))
ncor2 <- rowSums(del_updated[-c(1)])
ncor2 <- cbind.data.frame("Subject" = del_updated$Subject, "del_total" = ncor2)
c_o_2 <- merge(ctopp, ncor2, by = "Subject", all.b = T)
c2 <- cor(c_o_2$CTOPP_PA_raw, c_o_2$del_total)


barplot(table(ncor2$del_total), xlab = "Total score",col="darkorange")
text(x = 10, y = 35, labels = paste("cor(DEL, CTOPP-2 (raw)) =  ", round(c2, 2)), xpd=T)
text(x = 10, y = 30, labels = paste("α = ", round(f$alpha,2)), xpd=T)



hist(f$itemReport$itemMean, main = "Five Items Removed", xlab = "% correct",col="darkorange") # distribution of p.values
hist(f$itemReport$pBis, main = "",  xlab = "Point-biserial",col="darkorange") 

itemreport  <- t$itemReport
items <- subset(itemreport, itemreport$itemName == "DEL_2" |
                  itemreport$itemName == "DEL_12" |
                  itemreport$itemName == "DEL_19" |
                  itemreport$itemName == "DEL_20" |
                  itemreport$itemName == "DEL_23" |
                  itemreport$itemName == "DEL_24")



items <- items %>% select("Item" = itemName, "Item Mean" = itemMean, "Point-Biserial" = pBis)

kable(items, digits=2)  %>%
  kable_styling() %>% 
  scroll_box(width = "500px", height = "200px")

rm_del <- mirt(del_noSubj, 1, itemtype = "Rasch", guess=0.333, technical=list(NCYCLES=5000))
fullscores <- fscores(rm_del)
b <- as.data.frame(coef(rm_del, IRTpars=TRUE, simplify=TRUE)$items[,2])

wrightMap( fullscores, b$`coef(rm_del, IRTpars = TRUE, simplify = TRUE)$items[, 2]`, dim.name = "", item.side = itemClassic)

