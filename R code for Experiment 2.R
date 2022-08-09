{r message=FALSE, warning=FALSE}
library(readr)
library(dplyr)
library(ez)
library(ggplot2)
library(pwr2)
library(DescTools)
install.packages("DescTools")
install.packages("pwr2")

data <- read.csv("bashers_prolific_data.csv")


#clean data

data <- data %>%
  select(subject_id, condition, target_present, center, trial_index, trial_type, key_press, correct, rt)%>%
  mutate(condition = if_else(condition < 6 | condition >= 6, "language","abstract"))




#set cutoff 250-12500ms

data_p <- data%>%
  filter(trial_type=="visual-search-circle")
ggplot(data_p, aes(x=rt)) + 
  geom_histogram(binwidth = 15) + 
  labs(x="Response Time (ms)", y="Count") +
  geom_vline(xintercept=250) +
  coord_cartesian(xlim=c(0,1000)) +
  theme_bw()

#plot data (histogram)



data_p <- data%>%
  filter(trial_type=="visual-search-circle")
ggplot(data_p, aes(x=rt)) + 
  geom_histogram(binwidth = 15) + 
  labs(x="Response Time (ms)", y="Count") +
  geom_vline(xintercept=12500) +
  coord_cartesian(xlim=c(0,20000)) +
  theme_bw()

#First ANOVA (all participants)


x <- 14

language_data <- data %>%
  filter(condition== "language") %>%
  filter(trial_index %in% c(11:(11+2*x), (13+2*x):(13+4*x), (20+4*x):(20+6*x), (22+6*x):(22+8*x), (29+8*x):(29+10*x), (31+10*x):(31+12*x)))

abstract_data <- data %>% 
  filter(condition== "abstract") %>%
  filter(trial_index %in% c(10:(10+2*x), (12+2*x):(12+4*x), (19+4*x):(19+6*x), (21+6*x):(21+8*x), (28+8*x):(28+10*x), (30+10*x):(30+12*x)))

data1 <- rbind(language_data, abstract_data)
data1 <- data1 %>%
  filter(correct==1)%>%
  filter(rt >250) %>%
  filter(rt <12500) %>%
  group_by(subject_id, condition, center) %>%
  summarise(RT = mean(rt))


#plot in bar graph

#summarize
##data1_summary <- data1 %>%
#  group_by(condition, center) %>%
#  summarise(M=mean(RT), SE = sd(RT)/sqrt(n()))
#plot

ggplot(data1_summary, aes(x=condition, y=M, fill=center)) +
  geom_bar(stat = "identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=0.2, position=position_dodge(0.9)) +
  labs(x="Condition", y="Response Time (ms)") +
  scale_fill_manual(name="Center", 
                    labels=c("Cross", "Oddball", "Large Oddball"),
                    values = c("cross"="darkseagreen3", "oddball"="lightgoldenrod1", "largeoddball"="red")) +
  scale_x_discrete(labels=c("Abstract", "Language"))+
  theme_bw()

#ANOVA Analysis

data1 <- data1 %>% 
  group_by(subject_id) %>% 
  filter(is.na(RT))

anova1 <- ezANOVA(data1, dv = RT, wid = subject_id, within = center, between = condition)





#second ANOVA (participants who followed instructions) clean data

l_participants <- data %>%
  filter(condition=="language") %>%
  filter(trial_index==(32+12*x)) %>%
  filter(key_press==65)

language_data2 <- data %>%
  filter(condition=="language") %>%
  filter(subject_id %in% l_participants$subject_id) %>%
  filter(trial_index %in% c(11:(11+2*x), (13+2*x):(13+4*x), (20+4*x):(20+6*x), (22+6*x):(22+8*x), (29+8*x):(29+10*x), (31+10*x):(31+12*x)))

a_participants <- data %>%
  filter(condition=="abstract") %>%
  filter(trial_index==(31+12*x)) %>%
  filter(key_press==76)

abstract_data2 <- data %>%
  filter(condition=="abstract") %>%
  filter(subject_id %in% a_participants$subject_id) %>%
  filter(trial_index %in% c(10:(10+2*x), (12+2*x):(12+4*x), (19+4*x):(19+6*x), (21+6*x):(21+8*x), (28+8*x):(28+10*x), (30+10*x):(30+12*x)))

data2 <- rbind(language_data2, abstract_data2)
data2 <- data2 %>%
  filter(correct==1) %>%
  filter(rt >250) %>%
  filter(rt <12500) %>%
  group_by(subject_id, condition, center) %>%
  summarise(RT = mean(rt))


#bar graph

#summarize data
data2_summary <- data2 %>%
  group_by(condition, center) %>%
  
  # plot
  ggplot(data2_summary, aes(x=condition, y=M, fill=center)) +
  geom_bar(stat = "identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=0.2, position=position_dodge(0.9)) +
  labs(x="Condition", y="Response Time (ms)") +
  scale_fill_manual(name="Center", 
                    labels=c("Cross", "Oddball", "Large Oddball"),
                    values = c("cross"="darkseagreen3", "oddball"="lightgoldenrod1", "largeoddball"="red")) +
  scale_x_discrete(labels=c("Abstract", "Language"))+
  theme_bw()

#ANOVA 2 Analysis

anova2 <- aov(RT ~ center*condition, data=data2)
summary(anova2)








































