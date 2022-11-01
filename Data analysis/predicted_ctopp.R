# Phonological Awareness Score Report
#libraries
library(dplyr)
library(kableExtra)
library(DT)
library(ggplot2)
library(tidyverse)

#read excel file
 #report <- roarpa_data_overview sheet roar_pa_v.2_skill
report =  roarpa_data_overview
##metadata <- CTOPP_scores tab: CTOPP_scores
metadata = CTOPP_scores

#make variables
metadata <- dplyr::select(metadata,Subject,CTOPP_SM_Raw, CTOPP_SM_SS,CTOPP_PA_raw,CTOPP_PA_SS,CTOPP_Elision_Raw, CTOPP_Elision_SS)
raw.scores <- cbind.data.frame("Subject"=report_age_restrict$Subject, "ROAR-PA total"=report_age_restrict$`roar-pa_total`,"Age" =report_age_restrict$Age, "Grade" = report_age_restrict$grade_level)
sub_data <- merge(raw.scores, metadata, by="Subject")
sub_data$Age_Months <- sub_data$Age * 12 
report$grade_level <- factor(report$grade_level)
report$grade_level <- factor(report$grade_level , levels = c("pre-K","K","1st Grade", "2nd Grade","3rd Grade","4th Grade","5th grade and up"))


# count histogram 
count_hist <- ggplot(report, aes(x = `roar-pa_total`)) +
  geom_histogram(fill = "dark gray", colour = "black", bins=5) + 
  facet_grid(grade_level ~ .)+
  xlab("ROAR-PA Total Correct") +
  ylab("Count of Students") +
  ggtitle("ROAR-PA Score Distributions by Grade Level") +
  theme_classic(base_size=10)
count_hist

# generate stacked bar chart here (use it down below) 
b <- report %>%
  count(grade_level, risk.level)

c <- b %>% 
  group_by(grade_level) %>% mutate(percent = n/sum(n)*100)

c$risk.level <- factor(c$risk.level, levels = c("Doing Well", "Some Risk", "At Risk"))

# histogram 
stacked <- c %>%
  ggplot(aes(x=reorder(grade_level, desc(grade_level)),y=percent, fill=risk.level)) +
  geom_col() + 
  coord_flip() + 
  geom_bar(stat="identity")+scale_fill_manual(values = c("dodgerblue3","goldenrod3", "firebrick3")) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 


# cut score histogram 
cuts <- hist(report$percentile, seq(0,100,12.5))



### Identify parameters for grid search loess model
#Note: grid-search implemented from <https://www.r-bloggers.com/2016/12/grid-search-in-the-tidyverse/>, and #k-fold validation implemented using the crossv_kfold function to create samples.
set.seed(42)

train <- sub_data 

train$roar_score <- train$`ROAR-PA total`
train$s_score <- train$CTOPP_PA_SS

# Define a named list of parameter values for the exhaustive grid search
gs <-list(enp.target = seq(1, 3, 1),
          degree = seq(1, 2),
          surface = c("direct", "interpolate"),
          normalize = c(TRUE, FALSE)) %>% cross_df() 
## normalize yes or no / do or do not interpolate / enp_target 
#k-fold validation - 2 samples to train, 1 to evaluate - so each of the 3 samples is the held-out samples 1 time.

train <- train %>% dplyr::select(Subject, s_score, roar_score, Age_Months)

# accuracy_score can be r2_score, rmse_score, or corr_score 
results <- grid_search_k_fold(gs = gs, kfold = 3, train, accuracy_score = "rmse_score")

results <- results[order(results$avg, decreasing = FALSE), ]

results %>% 
  kbl(digits = 2) %>%
  kable_material(c("striped", "hover")) %>%
  scroll_box(width = "1000px", height = "200px")


### Final Check (Predicted CTOPP Percentile by Age)
#Based on the grid-search, select the optimized parameters for the loess model. As a final check, plot the predicted CTOPP percentiles. 
final <- loess(s_score ~ roar_score * Age_Months, data=train, enp.target = 1, degree=1,   control = loess.control(surface = "direct"))
percentiles <- predictPercentiles(final)
percentiles
