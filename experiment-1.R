#load library packages
library(readr)
library(dplyr)
library(ez)
library(ggplot2)

#import data file
data <- read.csv("exp-1-data.csv")
  
#get subset of columns relevant to experiment
data <- data %>%
  select(subject_id, condition, target_present, center, trial_index, trial_type, key_press, correct, rt)%>%
  mutate(condition = if_else(condition == 0 | condition == 1, "language", "abstract"))

# visualize RT histogram to determine cutoff times for short/long responses

data_p <- data%>%
  filter(trial_type=="visual-search-circle")

ggplot(data_p, aes(x=rt)) + 
  geom_histogram(binwidth = 15) + 
  labs(x="Response Time (ms)", y="Count") +
  geom_vline(xintercept=250) +
  coord_cartesian(xlim=c(0,1000)) +
  theme_bw()

ggplot(data_p, aes(x=rt)) + 
  geom_histogram(binwidth = 15) + 
  labs(x="Response Time (ms)", y="Count") +
  geom_vline(xintercept=12500) +
  coord_cartesian(xlim=c(0,20000)) +
  theme_bw()


# grab test trial data, merge together, filter to correct responses
# within RT range, calculate subject means for each cell

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

# are there subjects who only have data for one cell?
data1_n_per_subject <- data1 %>% group_by(subject_id) %>%
  summarize(n = n()) %>% filter(n < 2)

# yes, remove from data.
data1 <- data1 %>% filter(!subject_id %in% data1_n_per_subject$subject_id)

# plot data

data1_summary <- data1 %>% group_by(condition, center) %>%
  summarize(M = mean(RT), SE = sd(RT) / sqrt(n()))

plot_all <- ggplot(data1, aes(x=condition, y=RT, color=center)) + 
  geom_jitter(alpha=0.2, position = position_jitterdodge(jitter.width=0.2))+
  geom_point(data=data1_summary, mapping=aes(y=M), position=position_dodge(width=0.75), size=3)+
  geom_errorbar(data=data1_summary, mapping=aes(y=M, ymin=M-SE, ymax=M+SE), position=position_dodge(width=0.75), width=0.2)+
  labs(title="All subjects", x="Condition", y="Response Time (ms)")+
  scale_color_manual(name="Center", labels=c("Cross", "Symbol"), values=c("#E64B35", "#4DBBD5"))+
  theme_bw(base_size=14)+
  theme(panel.grid = element_blank())

#ANOVA analysis 1

anova1 <- ezANOVA(data1, RT, subject_id, within=c(center), between=c(condition))
anova1$ANOVA

#second ANOVA (participants who followed condition instructions)

l_participants <- data %>%
  filter(condition=="language") %>%
  filter(trial_index==123) %>%
  filter(key_press==65)

a_participants <- data %>%
  filter(condition=="abstract") %>%
  filter(trial_index==122) %>%
  filter(key_press==76)

subjects_who_followed_instructions <- unique(
  c(l_participants$subject_id, a_participants$subject_id)
)

data2 <- data1 %>% filter(subject_id %in% subjects_who_followed_instructions)

#second bar graph

data2_summary <- data2 %>% group_by(condition, center) %>%
  summarize(M = mean(RT), SE = sd(RT) / sqrt(n()))

# plot restricted data
plot_restricted <- ggplot(data2, aes(x=condition, y=RT, color=center)) + 
  geom_jitter(alpha=0.2, position = position_jitterdodge(jitter.width=0.2))+
  geom_point(data=data2_summary, mapping=aes(y=M), position=position_dodge(width=0.75), size=3)+
  geom_errorbar(data=data2_summary, mapping=aes(y=M, ymin=M-SE, ymax=M+SE), position=position_dodge(width=0.75), width=0.2)+
  labs(title="Subjects who used intended strategy", x="Condition", y="Response Time (ms)")+
  scale_color_manual(name="Center", labels=c("Cross", "Symbol"), values=c("#E64B35", "#4DBBD5"))+
  theme_bw(base_size=14)+
  theme(panel.grid = element_blank())

#ANOVA analysis 2

anova2 <- ezANOVA(data2, RT, subject_id, within=c(center), between=c(condition))
anova2$ANOVA

# check within abstract group to see if spontaneous language use
# reduces RT


# create table of who used language and who didn't

a_participants <- data %>%
  filter(condition=="abstract") %>%
  filter(trial_index==122) %>%
  mutate(used_language = (key_press == 65)) %>%
  select(subject_id, used_language)

data_abstract_only <- abstract_data %>%
  filter(correct == 1, rt > 250, rt < 12500) %>%
  group_by(subject_id) %>%
  summarise(RT = mean(rt)) %>%
  left_join(a_participants, by=c("subject_id"))

t.test(RT ~ used_language, data=data_abstract_only)


# create merged figure
library(patchwork)

plot_all + plot_restricted

























