#import sheet
library(readxl)
wj_ctopp_opaa <- read_excel("wj_ctopp_roarpa_data.xlsx",sheet = "Age_restricted_subj_del")
roar_ldt_uw <- read_excel("~/Desktop/ROAR-PA paper/roar_ldt_uw.xlsx")
wj_cor = wj_ctopp_opaa
ldt = roar_ldt_uw

#data summary details
mean(wj_cor$Age)
sd(wj_cor$Age)
min(wj_cor$Age)
max(wj_cor$Age)
table(wj_cor$Gender)

##correlations
   #complete test
cor.test(wj_cor$`WJ LW-ID raw`,wj_cor$`roar-pa`)
cor.test(wj_cor$`WJ WA raw`,wj_cor$`roar-pa`)
   #FSM
cor.test(wj_cor$`WJ LW-ID raw`,wj_cor$FSM)
cor.test(wj_cor$`WJ WA raw`,wj_cor$FSM)
   #LSM
cor.test(wj_cor$`WJ LW-ID raw`,wj_cor$LSM)
cor.test(wj_cor$`WJ WA raw`,wj_cor$LSM)
   #DEL
cor.test(wj_cor$`WJ LW-ID raw`,wj_cor$DEL)
cor.test(wj_cor$`WJ WA raw`,wj_cor$DEL)

   #plot
wj_cor$`roar-pa`=wj_cor$`roar-pa`*100
ggplot(wj_cor, aes(x=`roar-pa`, y=`WJ LW-ID raw`, colour=Age)) + geom_point() +xlab("ROAR-PA (% correct, 3 subtests) ")+ylab("WJ LW-ID (raw scores)") + geom_smooth(method='lm', colour="black", size =0.5)+ scale_color_distiller(palette = 'Spectral')

## roar-ldt correlation
roar_all <-left_join(wj_cor,ldt)
cor.test(roar_all$`roar-pa`,roar_all$`roar-ldt raw score`)


#### Californian school district children doing roar-pa version 2 ###
#subject info
  #pa_subj_info <- excel sheet to load: pa_subj_info.xlsx
pa_v2 = pa_subj_info

#roar-pa scores
pa_total_scores <- read_excel("ROAR-PA paper/Phase 3/pa_total_scores.xlsx")
pa_v2_scores=pa_total_scores
#merge
pa_v2_tot <- left_join(pa_v2,pa_v2_scores)
#summary output
summary(pa_v2_tot)
sd(pa_v2_tot$'roar-pa')


# grade limitation till 5th grade
pa_v2_tot_grade_lim <-subset(pa_v2_tot, grade<5)
# delete NA
pa_v2_tot_grade_lim<-  pa_v2_tot_grade_lim[!is.na( pa_v2_tot_grade_lim$`roar-ldt`),]
## dataset seemed not valid, Roar-ldt should not go >100 - limit
pa_v2_tot_grade_roar_lim <-subset(pa_v2_tot, `roar-ldt` < 100)
#cor.test roar-pa and roar-ldt
cor.test(pa_v2_tot_grade_roar_lim$`roar-ldt`,pa_v2_tot_grade_roar_lim$`roar-pa`)

###score reports
# mastery level
 ## read_excel: roarpa_data_overview.xlsx    sheet = "roar_pa_v.2_skill")
df=roarpa_data_overview
df <-subset(df, Age<10)


mastery <- df %>%count(grade_level, Skill_level)

mastery_grouped <- mastery %>% group_by(grade_level) %>% mutate(percent = n/sum(n)*100)
mastery_grouped$Skill_level <- factor(mastery_grouped$Skill_level, levels = c("Full Mastery", "Some Mastery", "No Mastery"))
mastery_grouped$grade_level <- factor(mastery_grouped$grade_level , levels = c("pre-K","K","1st Grade", "2nd Grade","3rd Grade","4th Grade"))

# histogram 
ggplot(mastery_grouped,aes(y=reorder(grade_level,desc(grade_level)),x=percent))+geom_bar(stat="identity",aes(fill = Skill_level))+xlab("Percentage of students")+ylab("Grade Levels")+labs(fill="Skill Level")+scale_fill_manual(values = c("#99CC99","#FFCC66", "#990000"))+theme_bw()+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank())


#summary stats
summary(df)
sd(df$`roar-pa_total`)
sd(df$Age)
table(df$Gender)



