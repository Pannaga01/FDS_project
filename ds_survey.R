ds_survey = read.csv("~/kaggle_survey_2021_responses.csv")
summary(ds_survey)
View(ds_survey)
ds_survey1 = data.frame(ds_survey[-1,])
View(ds_survey1)

#######programming langs##########
program_lang = ds_survey[,c(1,8,9,10,11,12,13,14,17,18)]
View(program_lang)
library(reshape2)
program_lang1 = melt(data = program_lang, id.vars ="Time.from.Start.to.Finish..seconds.",variable.name="languages")
View(program_lang1)
program_lang1 = program_lang1[!(is.na(program_lang1$value) | program_lang1$value==""), ]
langs = unique(program_lang1$value)
langs = langs[c(2,4,6,8,10,12,14,16,18)]
#langs = c(langs)
lang_counts = c(0,0,0,0,0,0,0,0,0)
#lang_counts = c(lang_counts)
for (i in 1:length(langs)){
  print(nrow(program_lang1[program_lang1$value == langs[i], ])) 
  lang_counts[i] = (nrow(program_lang1[program_lang1$value == langs[i], ]))
}

library(ggplot2)
barplot(lang_counts,names.arg=langs,xlab="languages",ylab="count")

#########data visualization########
data_vis = ds_survey[,c(1,60,61,62,63,64,65,66,67,68,71)]
View(data_vis)
library(reshape2)
data_vis1 = melt(data = data_vis, id.vars ="Time.from.Start.to.Finish..seconds.",variable.name="visuals")
View(data_vis1)
data_vis1 = data_vis1[!(is.na(data_vis1$value) | data_vis1$value==""), ]
visuals = unique(data_vis1$value)
visuals = visuals[c(2,4,6,8,10,12,14,16,18,20)]
#langs = c(langs)
vis_counts = c(0,0,0,0,0,0,0,0,0,0)
#lang_counts = c(lang_counts)
for (i in 1:length(visuals)){
  print(nrow(data_vis1[data_vis1$value == visuals[i], ])) 
  vis_counts[i] = (nrow(data_vis1[data_vis1$value == visuals[i], ]))
}
library(ggplot2)
barplot(vis_counts,names.arg=visuals,xlab="visualization",ylab="count",horiz = TRUE,las=1)

##########men-women#############
gender = data.frame(ds_survey[-1,3])
View(gender)
ggplot(gender, aes(x=ds_survey..1..3.))+geom_bar(stat="count")

age = data.frame(ds_survey[ds_survey$Q1 != "70+",2],stringsAsFactors = TRUE)
ds_survey1[, 'Q1'] <- as.factor(ds_survey1[, 'Q1'])
#age = data.frame(age[-1,])
#age1 = as.factor(age$age..1...)
View(age)
ggplot(ds_survey1, aes(x=Q1)) + geom_bar(stat="count")
#ggplot(ds_survey1, aes(x=Q2, y=Q1)) + geom_violin(trim = FALSE)
ggplot(ds_survey1, aes(x=Q1, fill=Q2)) +geom_bar(stat="count")
ds_survey1$Q6 <- as.factor(ds_survey1$Q6)
ds_survey1$Q4 <- as.factor(ds_survey1$Q4)

##########Usage of kaggle#########
#barplot(table(ds_survey1$Q5),las=2,cex.names=0.75) ####kaggle usage
install.packages("dplyr")
#library(dplyr)
#top_n(ds_survey1, n=10, freq) %>% ggplot(ds_survey1, aes(x=Q3))+ geom_bar(stat='count')
ggplot(ds_survey1, aes(x="Q5",fill=Q5))+geom_bar(width = 1, stat = "count",color="white")+coord_polar("y")+geom_text(stat="count", aes(label=..count..),position = position_stack(vjust = 0.5))
#ggplot(ds_survey1, aes(x = Q5, y= Q25)) + geom_boxplot()+theme(axis.text.x = element_text(angle=90))
ds_survey1$Q15 <- as.factor(ds_survey1$Q15)
boxplot(Q15~Q5, data=ds_survey1,las=2)
#ggplot(ds_survey1, aes(x = Q5, y=Q15)) + geom_violin(trim=FALSE)+labs(x="role",y="ml_exp")+theme(axis.text.x = element_text(angle=15),axis.text.y = element_text(angle=15))
ggplot(ds_survey1, aes(x = Q6)) + geom_bar(stat = "count",position = "dodge")+labs(x="experience")+theme(axis.text.x = element_text(angle=15))+geom_text(stat="count", aes(label=..count..),position = position_stack(vjust = 0.5))
ggplot(ds_survey1, aes(x = Q4)) + geom_bar(stat = "count",position = "dodge")+labs(x="education")+theme(axis.text.x = element_text(angle=15))+geom_text(stat="count", aes(label=..count..),position = position_stack(vjust = 0.5))
ggplot(ds_survey1, aes(x = Q15)) + geom_bar(stat = "count",position = "dodge")+labs(x="ml_experience")+theme(axis.text.x = element_text(angle=15))+geom_text(stat="count", aes(label=..count..),position = position_stack(vjust = 0.5))

role_exp = ds_survey1[,c(6,72)]
View(role_exp)
role_exp = role_exp[!(is.na(role_exp$Q15) | role_exp$Q15==""),]
role_exp = role_exp[role_exp$Q5 %in% c("Software Engineer", "Data Scientist","Data Analyst","Research Scientist","Business Analyst"), ]
ggplot(role_exp, aes(x = Q5, fill = Q15)) + geom_bar(stat = "count",position = "dodge")+labs(x="role")+theme(axis.text.x = element_text(angle=90))

ggplot(ds_survey1, aes(x = Q4, fill = Q2)) + geom_bar(stat = "count",position = "dodge")+labs(x="education")+theme(axis.text.x = element_text(angle=15))
######cloud_plat#####
cloud_plat = ds_survey1[,c(1,6,130,131,132,133,134,135,136,137,138,139,140)]
table(cloud_plat$Q5)
View(cloud_plat)
library(reshape2)
cloud_plat1 = melt(data = cloud_plat, id.vars =c("Time.from.Start.to.Finish..seconds.","Q5"),variable.name="platforms")
View(cloud_plat1)
table(cloud_plat1$Q5)
cloud_plat1 = cloud_plat1[!(is.na(cloud_plat1$value) | cloud_plat1$value==""), ]
table(cloud_plat1$Q5)
cloud_plat2 = cloud_plat1[cloud_plat1$platforms %in% c("Q27_A_Part_1","Q27_A_Part_2","Q27_A_Part_3"), ]
table(cloud_plat2$Q5)
View(cloud_plat2)
#barplot(table(cloud_plat1$value),las=2,cex.axis = 0.5)
ggplot(cloud_plat1, aes(x = "value",fill=value))+geom_bar(width = 1, stat = "count",color="white")+coord_polar("y")+geom_text(stat="count", aes(label=..count..),position = position_stack(vjust = 0.5))
ggplot(cloud_plat2, aes(x = Q5, fill = value)) + geom_bar(stat = "count",position = "dodge")+labs(x="role")+theme(axis.text.x = element_text(angle=90))

######ml algos########

ml_algo = ds_survey1[,c(6,91,92,93,94,95,96,97,98,99,100,101,102)]
ml_algo1 = melt(data = ml_algo, id.vars ="Q5",variable.name="ml_algos")
ml_algo1 = ml_algo1[!(is.na(ml_algo1$value) | ml_algo1$value %in% c("Evolutionary Approaches", "","Other","Generative Adversarial Networks","None","Transformer Networks (BERT, gpt-3, etc)")), ]
ml_algo1 = ml_algo1[!(ml_algo1$Q5 %in% c("DBA/Database Engineer","Product Manager ","Statistician","Program/Project Manager","Developer Relations/Advocacy ","Data Engineer")), ]
View(ml_algo1)
ml_framework = ds_survey1[,c(6,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87)]
View(ml_framework)
ml_frame = melt(data = ml_framework, id.vars ="Q5",variable.name="ml_frames")
ml_frame = ml_frame[!(is.na(ml_frame$value) | ml_frame$value==""), ]
#barplot(table(ml_algo1$value))
ggplot(data=ml_frame, aes(x=value)) +geom_bar(stat="count")+geom_text(stat="count", aes(label=..count..), vjust=-1)+theme(axis.text.x = element_text(angle=15))

######Salaries#####
sals_role = ds_survey1[,c(6,128)]
sals_role = sals_role[!(is.na(sals_role$Q25) | sals_role$Q25=="" | sals_role$Q5 == "Student" | sals_role$Q5 == "Currently not employed"), ]
sals_role$Q25 <- gsub(",","",sals_role$Q25)
sals_role$Q25 <- gsub("$","",sals_role$Q25)
sals_role$Q25 <- gsub("-","",sals_role$Q25)
sals_role$Q25 = as.numeric(sals_role$Q25)

######Countries#######
Countries = ds_survey1 %>% arrange(desc(table(Q3))) %>% slice(1:10) %>% ggplot(., aes(x=Q3))+ geom_bar(stat='count')
