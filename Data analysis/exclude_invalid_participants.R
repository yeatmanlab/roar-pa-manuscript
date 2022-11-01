library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
###### 5 subtests
###select by same answer option (response_action --> position: left, middle or right click)
d=select_indiv_5
resp_act=d[,c("subject","subtest","response_action")]
resp_act <- resp_act %>% group_by(subject,subtest,response_action) %>% summarise(Freq=n())
sd(resp_act$Freq)
mean(resp_act$Freq)
median(resp_act$Freq)
resp_act$subtest <- factor(resp_act$subtest, levels = c("FSM", "LSM","RHY","BLE", "DEL"))
#make histogram
hist_resp_act = ggplot(resp_act,aes(x=Freq,group=response_action, fill=response_action))+geom_histogram(binwidth=1, position="dodge")+facet_grid(~subtest)+geom_vline(xintercept = c(2.83,13.58), linetype="dotted") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
hist_resp_act
# subjects with very low response actions (<3) or very high responses (>13)
resp_act_exclude <- filter(resp_act, Freq < 3|Freq>13)
resp_act_exclude <- resp_act_exclude[order(-resp_act_exclude$Freq),]
resp_act_exclude
print(as_tibble(resp_act_exclude), n = 81)
write.csv(resp_act_exclude, "exclude_5_clicks.csv")
#list of participants to exclude
list_names_exclude <-list(resp_act_exclude$subject)
list_names_exclude

### select by high proportions of foil 2 (worst answer option)
d=select_indiv_5
foil_rate=d[,c("subject","subtest","score","response")]
foil_rate <- filter(foil_rate , score == 0 )
foil_rate <- filter(foil_rate , response != "goal" )
foil_rate <- foil_rate %>% group_by(subject,subtest,response) %>% summarise(Freq=n())
sd(foil_rate$Freq[foil_rate$response=="foil2"])
mean(foil_rate$Freq[foil_rate$response=="foil2"])
median(foil_rate$Freq[foil_rate$response=="foil2"])
foil_rate$subtest <- factor(foil_rate$subtest, levels = c("FSM", "LSM","RHY","BLE", "DEL"))
hist_foil_rate = ggplot(foil_rate,aes(x=Freq,group=response, fill=response))+geom_histogram(binwidth=1, position="dodge")+facet_grid(~subtest)+geom_vline(xintercept = 9.75, linetype="dotted")
hist_foil_rate
foil_rate_exclude <- filter(foil_rate,response=="foil2"& Freq>9.75)
foil_rate_exclude <- foil_rate_exclude[order(-foil_rate_exclude$Freq),]
foil_rate_exclude
print(as_tibble(foil_rate_exclude), n = 24)
write.csv(foil_rate_exclude, "exclude_5_foils.csv")

list_names_exclude2 <-list(foil_rate_exclude$subject)
list_names_exclude2


####### 3 SUBTESTS
###select by same answer option (response_action --> position: left, middle or right click)
a=select_indiv_3
resp_act_3=a[,c("subject","subtest","response_action")]
resp_act_3 <- resp_act_3 %>% group_by(subject,subtest,response_action) %>% summarise(Freq=n())
sd(resp_act_3$Freq)
mean(resp_act_3$Freq)
median(resp_act_3$Freq)
resp_act_3$subtest <- factor(resp_act_3$subtest, levels = c("FSM","LSM","DEL"))
#make histogram
hist_resp_act_3 = ggplot(resp_act_3,aes(x=Freq,group=response_action, fill=response_action))+geom_histogram(binwidth=1, position="dodge")+facet_grid(~subtest)+geom_vline(xintercept = c(2.99,13.48), linetype="dotted") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
hist_resp_act_3
# subjects with very low response actions (<3) or very high responses (>13)
resp_act_exclude_3 <- filter(resp_act_3, Freq < 3|Freq>13)
resp_act_exclude_3 <- resp_act_exclude_3[order(-resp_act_exclude_3$Freq),]
resp_act_exclude_3
print(as_tibble(resp_act_exclude_3), n = 83)
write.csv(resp_act_exclude_3, "exclude_3_clicks.csv")
#list of participants to exclude
list_names_exclude_3 <-list(resp_act_exclude_3$subject)
list_names_exclude_3

### select by high proportions of foil 2 (worst answer option)
a=select_indiv_3
foil_rate_3=a[,c("subject","subtest","score","response")]
foil_rate_3 <- filter(foil_rate_3 , score == 0 )
foil_rate_3 <- filter(foil_rate_3 , response != "goal" )
foil_rate_3 <- foil_rate_3 %>% group_by(subject,subtest,response) %>% summarise(Freq=n())
sd(foil_rate_3$Freq[foil_rate_3$response=="foil2"])
mean(foil_rate_3$Freq[foil_rate_3$response=="foil2"])
median(foil_rate_3$Freq[foil_rate_3$response=="foil2"])
foil_rate_3$subtest <- factor(foil_rate_3$subtest, levels = c("FSM","LSM","DEL"))
hist_foil_rate_3 = ggplot(foil_rate_3,aes(x=Freq,group=response, fill=response))+geom_histogram(binwidth=1, position="dodge")+facet_grid(~subtest)+geom_vline(xintercept = 9.86, linetype="dotted")
hist_foil_rate_3
foil_rate_exclude_3 <- filter(foil_rate_3,response=="foil2"& Freq>9.86)
foil_rate_exclude_3 <- foil_rate_exclude_3[order(-foil_rate_exclude_3$Freq),]
foil_rate_exclude_3
print(as_tibble(foil_rate_exclude_3), n = 24)
write.csv(foil_rate_exclude_3, "exclude_3_foils.csv")

list_names_exclude_2_3 <-list(foil_rate_exclude_3$subject)
list_names_exclude_2_3

