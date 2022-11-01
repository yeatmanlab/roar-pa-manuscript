# Set working Directory
setwd('~/Desktop/')
library(ggpubr)
library(tidyr)
library(corrplot)

###3 subtests
#read files
l <- read_excel("~/Desktop/roarpa_3subtests.xlsx"+ sheet = "ROAR_PA_3_deleted_prop")
m <- read_excel("~/Desktop/CTOPP_scores.xlsx") # tab raw scores


# to include subtests of CTOPP
ROAR_PA.data <- left_join(l,m)
ROAR_PA.data = subset(ROAR_PA.data, select=-c(Gender,Type,Age,Subject))
ROAR_PA.data <- ROAR_PA.data[!is.na(ROAR_PA.data$CTOPP_PA_raw),]
ROAR_PA.data_sub <- ROAR_PA.data[!is.na(ROAR_PA.data$Overall_3),]
#correlation plot
Cor_table <- cor(ROAR_PA.data_sub)
corrplot(Cor_table,method="number",number.cex=0.75,type="upper",tl.cex=.5,cl.lim=c(0,1),col=colorRampPalette(c("white","lightyellow","darkgreen"))(100))

### only with children age 4-9
ROAR_PA.data <- left_join(l,m)
ROAR_PA.data_agecut <-subset(ROAR_PA.data, Age<9.50)
ROAR_PA.data_agecut
ROAR_PA.data_agecut = subset(ROAR_PA.data_agecut, select=-c(Gender,Type,Age,Subject))
ROAR_PA.data_agecut <- ROAR_PA.data_agecut[!is.na(ROAR_PA.data_agecut$CTOPP_PA_raw),]
ROAR_PA.data_agecut_sub <- ROAR_PA.data_agecut[!is.na(ROAR_PA.data_agecut$Overall_3),]
Cor_table_agecut <- cor(ROAR_PA.data_agecut_sub)
corrplot(Cor_table_agecut,method="number",number.cex=0.75,type="upper",tl.cex=.5,cl.lim=c(0,1),col=colorRampPalette(c("white","lightyellow","darkgreen"))(100))

###for left_join a_b
#join files
ROAR_PA.data <- left_join(a,b)
#drop rows with NA in CTOPP
ROAR_PA.data <- ROAR_PA.data[!is.na(ROAR_PA.data$CTOPP_PA_raw),]
ROAR_PA.data

#drop rows with NA in FSM
ROAR_PA.data_FSM <- ROAR_PA.data[!is.na(ROAR_PA.data$FSM),]
print(as_tibble(ROAR_PA.data_FSM), n = 100)
cor.test(ROAR_PA.data_FSM$FSM,ROAR_PA.data_FSM$CTOPP_PA_raw)

#drop rows with NA in LSM
ROAR_PA.data_LSM <- ROAR_PA.data[!is.na(ROAR_PA.data$LSM),]
print(as_tibble(ROAR_PA.data_LSM), n = 100)
cor.test(ROAR_PA.data_LSM$LSM,ROAR_PA.data_LSM$CTOPP_PA_raw)

#drop rows with NA in DEL
ROAR_PA.data_DEL <- ROAR_PA.data[!is.na(ROAR_PA.data$DEL),]
print(as_tibble(ROAR_PA.data_DEL), n = 100)
cor.test(ROAR_PA.data_DEL$DEL,ROAR_PA.data_DEL$CTOPP_PA_raw)


#drop rows with NA in 3 subtests
ROAR_PA.data_3sub1 <- ROAR_PA.data[!is.na(ROAR_PA.data$Overall_3_fsmlsmdel),]
cor.test(ROAR_PA.data_3sub1$Overall_3_fsmlsmdel,ROAR_PA.data_3sub1$CTOPP_PA_raw)

#Potentially Delete 2 rounds of outliers
model_3sub<-lm(ROAR_PA.data_3sub$CTOPP_PA_raw~ROAR_PA.data_3sub$Overall_3)
plot(model_3sub)
ROAR_PA.data_3sub_outlier = ROAR_PA.data_3sub[-c(17,50,76),]
model_3sub_outlier<-lm(ROAR_PA.data_3sub_outlier$CTOPP_PA_raw~ROAR_PA.data_3sub_outlier$Overall_3)
plot(model_3sub_outlier)
ROAR_PA.data_3sub_outlier = ROAR_PA.data_3sub[-c(53,63,69),]
#cor.test
cor.test(ROAR_PA.data_3sub_outlier$Overall_3,ROAR_PA.data_3sub_outlier$CTOPP_PA_raw)


#other correlations
cor(ROAR_PA.data_5sub$FSM,ROAR_PA.data_5sub$LSM)
cor(ROAR_PA.data_5sub$FSM,ROAR_PA.data_5sub$DEL)
cor(ROAR_PA.data_5sub$LSM,ROAR_PA.data_5sub$DEL)

# to include subtests of CTOPP
ROAR_PA.data <- left_join(l,m)
ROAR_PA.data_plot <- ROAR_PA.data[!is.na(ROAR_PA.data$CTOPP_PA_raw),]
ROAR_PA.data_plot_sub <- ROAR_PA.data_plot[!is.na(ROAR_PA.data_plot$Overall_3),]

#make plot
ROAR_PA.data_plot_sub$Overall_3 <- ROAR_PA.data_plot_sub$Overall_3*100
plot_cor <- ggplot(ROAR_PA.data_plot_sub, aes(x=Overall_3, y=CTOPP_PA_raw, colour=Age)) + geom_point() +xlab("ROAR-PA (% correct, 3 subtests) ")+ylab("CTOPP-2 (raw scores)") + geom_smooth(method='lm', colour="black", size =0.5)+ scale_color_distiller(palette = 'Spectral')
plot_cor

#make plot age 4-9
ROAR_PA.data <- left_join(l,m)
ROAR_PA.data_agecut <-subset(ROAR_PA.data, Age<9.50)
ROAR_PA.data_agecut
ROAR_PA.data_agecut <- ROAR_PA.data_agecut[!is.na(ROAR_PA.data_agecut$CTOPP_PA_raw),]
ROAR_PA.data_agecut_sub <- ROAR_PA.data_agecut[!is.na(ROAR_PA.data_agecut$Overall_3),]
ROAR_PA.data_agecut_sub$Overall_3 <- ROAR_PA.data_agecut_sub$Overall_3*100
plot_cor <- ggplot(ROAR_PA.data_agecut_sub, aes(x=Overall_3, y=CTOPP_PA_raw, colour=Age)) + geom_point() +xlab("ROAR-PA (% correct, 3 subtests) ")+ylab("CTOPP-2 (raw scores)") + geom_smooth(method='lm', colour="black", size =0.5)+ scale_color_distiller(palette = 'Spectral')
plot_cor






###full test
#read files
a <- read_excel("~/Desktop/roarpa_5subtests.xlsx"+ sheet = "ROAR_PA_5_deleted_prop")
b <- read_excel("~/Desktop/CTOPP_scores.xlsx") # tab raw scores

# to include subtests of CTOPP
ROAR_PA.data <- left_join(a,b)
ROAR_PA.data = subset(ROAR_PA.data, select=-c(Gender,Type,Age,Subject))
ROAR_PA.data <- ROAR_PA.data[!is.na(ROAR_PA.data$CTOPP_PA_raw),]
ROAR_PA.data_sub <- ROAR_PA.data[!is.na(ROAR_PA.data$Overall_5),]
#correlation plot
Cor_table <- cor(ROAR_PA.data_sub)
corrplot(Cor_table,method="number",number.cex=0.75,type="upper",tl.cex=.5,cl.lim=c(0,1),col=colorRampPalette(c("white","lightyellow","darkgreen"))(100))

#make plot
plot_cor <- ggplot(OPAA.data, aes(x=total_prop, y=CTOPP_PA_raw, colour=Age)) + geom_point() +xlab("OPAA proportion scores")+ylab("CTOPP PA raw scores") + geom_smooth(method='lm') +scale_color_gradientn(colours = rainbow(4))
plot_cor

