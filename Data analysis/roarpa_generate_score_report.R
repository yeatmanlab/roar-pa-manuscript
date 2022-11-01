# load files
    #report <- roarpa_data_overview sheet roar_pa_v.2_skill
report = roarpa_data_overview
     #metadata <- CTOPP_scores tab: CTOPP_scores
metadata = CTOPP_scores

# settings
report_age_restrict <-subset(report, Age<10)     #grade limitation till 5th grade
metadata <- dplyr::select(metadata,Subject, CTOPP_SM_Raw, CTOPP_SM_SS,CTOPP_PA_raw, CTOPP_PA_SS,CTOPP_Elision_Raw, CTOPP_Elision_SS)
raw.scores <- cbind.data.frame("Subject"=report_age_restrict$Subject, "ROAR-PA total"=report_age_restrict$`roar-pa_total`,"Age" =report_age_restrict$Age, "Grade" = report_age_restrict$grade_level)
sub_data <- merge(raw.scores, metadata, by="Subject")
sub_data$Age_Months <- sub_data$Age * 12 

a <- sub_data
a$roar_score <- a$`ROAR-PA total`
a$s_score <- a$CTOPP_PA_SS

#create model
ctopp_uw <- loess(s_score ~ roar_score * Age_Months, data=train, enp.target = 1, degree=1,control = loess.control(surface = "direct"))

#function
predictPAClassification <- function(score_file, meta_data, lookup_model, model_name)
{
  # Predicts classifications of student data
  
  # Parameters: 
  #  score_file (df): compiled score file from ROAR assessment
  #  meta_data (df): student metadata (includes ages)
  #  lookup model (df): Look up table for ROAR scores/WJ scores, percentile and classification
  # Return: 
  #   Dataframe with predicted WJ SS, percentiles, and classifications
  
  subj_df <- merge(meta_data, score_file, by="Subject")
  
  pred_ctopp_SS <- predict(lookup_model, subj_df)
  
  report <- cbind.data.frame(subj_df, pred_ctopp_SS)
  
  report$percentile <- pnorm(report$pred_ctopp_SS, mean=100, sd=15)*100
  
  report$RiskLevel <- cut(report$percentile, breaks = c(0, 25, 50, 100),
                          labels = c('At Risk', 'Some Risk', 'Doing Well'))  
  
  return(report)
  
}
# where we get the risk classifications - make sure that the names are the same
report.replicate <- predictPAClassification(score_file=a,meta_data=metadata,lookup_model=ctopp_uw)


report.replicate <- report.replicate %>% dplyr::select("student"=Subject, 
                                                       "Grade" = Grade, 
                                                       "Age" = Age, 
                                                       "ROAR-PA total" = roar_score, 
                                                       "estimated ctopp scaled score" = pred_ctopp_SS, 
                                                       "percentile rank" = percentile, 
                                                       "risk level"= RiskLevel)


write.csv(report.replicate,"risk_level_ROAR-pa.csv")

cuts <- hist(report.replicate$percentile, seq(0,100,12.5))
# plot at risk graph
ymax = max(cuts$counts)+ 10
textloc = max(cuts$counts)+ 5

#plot
plot(cuts,  xaxt='n', main = "", xlab = paste("CTOPP-2 (%) Equivalent Scores", "\n" ,"Percentiles relative to National Norms"), ylab = "Count of Students", ylim = c(0, ymax), xlim = c(0,100), col =c("#CC0000", "#CC0000","#FFCC00", "#FFCC00","#336600", "#336600", "#336600", "#336600")) +abline(v=c(25, 50), col=c("black", "black"), lty=2, lwd =1)
ticks = c(0, 25, 50, 75, 100)
axis(side = 1, at = ticks)

text(10, textloc, "At Risk", col = "#CC0000")
text(38, textloc, "Some Risk", col = "#FFCC00")
text(70, textloc, "Doing Well", col = "#336600") 



