#load library packages
#install.packages("readr")
#install.packages("dplyr")
#install.packages("ez")
#install.packages("ggplot2")
library(readr)
library(dplyr)
library(ez)
library(ggplot2)




#import data file
data <- read.csv("bashers_prolific_data.csv")
  



#clean data

data <- data %>%
  select(subject_id, condition, target_present, center, trial_index, trial_type, key_press, correct, rt)%>%
  mutate(condition = if_else(condition == 0 | condition == 1, "language","abstract"))



#implement exclusion criteria 

data_p <- data%>%
  filter(trial_type=="visual-search-circle")
ggplot(data_p, aes(x=rt)) + 
  geom_histogram(binwidth = 15) + 
  labs(x="Response Time (ms)", y="Count") +
  geom_vline(xintercept=250) +
  coord_cartesian(xlim=c(0,1000)) +
  theme_bw()


#zoomed in version

data_p <- data%>%
  filter(trial_type=="visual-search-circle")
ggplot(data_p, aes(x=rt)) + 
  geom_histogram(binwidth = 15) + 
  labs(x="Response Time (ms)", y="Count") +
  geom_vline(xintercept=12500) +
  coord_cartesian(xlim=c(0,20000)) +
  theme_bw()


#first ANOVA with all participants

language_data <- data %>%
  filter(condition== "language") %>%
  filter(trial_index %in% c(11:36, 38:63, 70:95,97:122))

abstract_data <- data %>% 
  filter(condition== "abstract") %>%
  filter(trial_index %in% c(10:35, 37:62, 69:94,96:121))

data1 <- rbind(language_data, abstract_data)
data1 <- data1 %>%
  filter(correct==1)%>%
  filter(rt >250) %>%
  filter(rt <12500) %>%
  group_by(subject_id, condition, center) %>%
  summarise(RT = mean(rt))


#Bar graph (to be changed to box plot)

#summarize

data1_summary <- data1 %>%
  group_by(condition, center) %>%
  summarise(M=mean(RT), SE = sd(RT)/sqrt(n()))

#plot

ggplot(data1_summary, aes(x=condition, y=M, fill=center)) +
  geom_bar(stat = "identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=0.2, position=position_dodge(0.9)) +
  labs(x="Condition", y="Response Time (ms)") +
  scale_fill_manual(name="Center", 
                    labels=c("Cross", "Oddball"),
                    values = c("cross"="darkseagreen3", "oddball"="lightgoldenrod1")) +
  scale_x_discrete(labels=c("Abstract", "Language"))+
  theme_bw()




#boxplot attempt
#ggplot(data1_summary, aes(x=condition, y=M, fill=center)) + 
# geom_boxplot() +

 ## labs(x="Condition", y="Response Time (ms)") +
 # scale_fill_manual(name="Center", labels=c("Cross", "Oddball"), values = c("cross"="darkseagreen3", "oddball"="lightgoldenrod1")) +
 ## geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=0.2, position=position_dodge(0.9)) +
 # scale_x_discrete(labels=c("Abstract", "Language")) +
 # stat_summary(fun=mean, geom="point", shape=23, size=4) 
  
  #geom_jitter(shape=16, position=position_jitter(0.2)) +



  

  #FIRST ANOVA WITH ALL PARTICIPANTS BOXPLOT FINISHED%

data = read.csv("~/../Downloads/bashers_prolific_data.csv")
data1 <- data %>%
  filter(correct==1)%>%
  filter(rt >250) %>%
  filter(rt <12500) %>%
  group_by(subject_id, condition, center) %>%
  summarise(RT = mean(rt), SE = sd(RT)/sqrt(n()))

data1$condition[data1$condition == 0] = "language"
data1$condition[data1$condition == 1] = "language"
data1$condition[data1$condition == 2] = "abstract"
data1$condition[data1$condition == 3] = "abstract"

ggplot(data1, aes(x=condition, y=RT, fill=center)) + geom_boxplot() +
  labs(x="Condition", y="Response Time (ms)") +
  scale_fill_manual(name="Center", labels=c("Cross", "Oddball"), values = c("cross"="darkseagreen3", "oddball"="lightgoldenrod1")) +
  geom_errorbar(aes(ymin=RT-SE, ymax=RT+SE), width=0.2, position=position_dodge(0.9)) +
  scale_x_discrete(labels=c("Abstract", "Language")) +
  stat_summary(fun=mean, geom="point", shape=23, size=4, position=position_dodge(0.75)) +
  theme_bw()






#ANOVA analysis 1

anova1 <- aov(RT ~ center*condition, data=data1)
summary(anova1)




#second ANOVA (participants who followed condition instructions)

l_participants <- data %>%
  filter(condition=="language") %>%
  filter(trial_index==123) %>%
  filter(key_press==65)

language_data2 <- data %>%
  filter(condition=="language") %>%
  filter(subject_id %in% l_participants$subject_id) %>%
  filter(trial_index %in% c(11:36, 38:63, 70:95,97:122))

a_participants <- data %>%
  filter(condition=="abstract") %>%
  filter(trial_index==122) %>%
  filter(key_press==76)

abstract_data2 <- data %>%
  filter(condition=="abstract") %>%
  filter(subject_id %in% a_participants$subject_id) %>%
  filter(trial_index %in% c(10:35, 37:62, 69:94,96:121))

data2 <- rbind(language_data2, abstract_data2)
data2 <- data2 %>%
  filter(correct==1) %>%
  filter(rt >250) %>%
  filter(rt <12500) %>%
  group_by(subject_id, condition, center) %>%
  summarise(RT = mean(rt))

#second bar graph

#summarize

data2_summary <- data2 %>%
  group_by(condition, center) %>%
  summarise(M=mean(RT), SE = sd(RT)/sqrt(n()))

#plot (make this into boxplot if possible)

#ggplot(data2_summary, aes(x=condition, y=M, fill=center)) +
 # geom_bar(stat = "identity", color="black", position=position_dodge()) +
  #geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=0.2, position=position_dodge(0.9)) +
#  labs(x="Condition", y="Response Time (ms)") +
 # scale_fill_manual(name="Center", 
  #                  labels=c("Cross", "Oddball"),
   #                 values = c("cross"="darkseagreen3", "oddball"="lightgoldenrod1")) +
#  scale_x_discrete(labels=c("Abstract", "Language"))+
#  theme_bw()




#SECOND ANOVA BOXPLOT WITH SUBSET PARTICIPANTS 

data = read.csv("~/../Downloads/bashers_prolific_data.csv")

l_participants <- data %>%
  filter(condition=="language") %>%
  filter(trial_index==123) %>%
  filter(key_press==65)

language_data2 <- data %>%
  filter(condition=="language") %>%
  filter(subject_id %in% l_participants$subject_id) %>%
  filter(trial_index %in% c(11:36, 38:63, 70:95,97:122))

a_participants <- data %>%
  filter(condition=="abstract") %>%
  filter(trial_index==122) %>%
  filter(key_press==76)

abstract_data2 <- data %>%
  filter(condition=="abstract") %>%
  filter(subject_id %in% a_participants$subject_id) %>%
  filter(trial_index %in% c(10:35, 37:62, 69:94,96:121))

data2 <- rbind(language_data2, abstract_data2)


data2 <- data2 %>%
  filter(correct==1)%>%
  filter(rt >250) %>%
  filter(rt <12500) %>%
  group_by(subject_id, condition, center) %>%
  summarise(RT = mean(rt), SE = sd(RT)/sqrt(n()))

data1$condition[data1$condition == 0] = "language"
data1$condition[data1$condition == 1] = "language"
data1$condition[data1$condition == 2] = "abstract"
data1$condition[data1$condition == 3] = "abstract"

ggplot(data2, aes(x=condition, y=RT, fill=center)) + geom_boxplot() +
  labs(x="Condition", y="Response Time (ms)") +
  scale_fill_manual(name="Center", labels=c("Cross", "Oddball"), values = c("cross"="darkseagreen3", "oddball"="lightgoldenrod1")) +
  geom_errorbar(aes(ymin=RT-SE, ymax=RT+SE), width=0.2, position=position_dodge(0.9)) +
  scale_x_discrete(labels=c("Abstract", "Language")) +
  stat_summary(fun=mean, geom="point", shape=23, size=4, position=position_dodge(0.75)) +
  theme_bw()









#ANOVA analysis 2

anova2 <- aov(RT ~ center*condition, data=data2)
summary(anova2)































