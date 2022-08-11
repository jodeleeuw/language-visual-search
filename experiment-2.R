library(readr)
library(dplyr)
library(ez)
library(ggplot2)

all_data <- read.csv("exp-2-data.csv")

#get subset of columns relevant to experiment

data <- all_data %>%
  select(subject_id, condition, target_present, center, trial_index, trial_type, key_press, correct, rt)%>%
  mutate(condition = if_else(condition < 6, "language","abstract")) %>%
  mutate(center = factor(center, levels=c("cross", "oddball", "largeoddball")))

# grab test trial data, merge together, filter to correct responses
# within RT range, calculate subject means for each cell

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

# are there subjects who only have data for not all cells?
data1_n_per_subject <- data1 %>% group_by(subject_id) %>%
  summarize(n = n())

# no, but there are a handful of subjects with data for more than 3 runs! must have restarted the experiment.
# remove these subjects
subjects_who_completed_experiment_twice <- data1_n_per_subject %>% filter(n > 3)

data1 <- data1 %>% filter(!subject_id %in% subjects_who_completed_experiment_twice$subject_id)

# plot

data1_summary <- data1 %>% group_by(condition, center) %>%
  summarize(M = mean(RT), SE = sd(RT) / sqrt(n()))

plot_all <- ggplot(data1, aes(x=condition, y=RT, color=center)) + 
  geom_jitter(alpha=0.1, position = position_jitterdodge(jitter.width=0.2))+
  geom_point(data=data1_summary, mapping=aes(y=M), position=position_dodge(width=0.75), size=3)+
  geom_errorbar(data=data1_summary, mapping=aes(y=M, ymin=M-SE, ymax=M+SE), position=position_dodge(width=0.75), width=0.2)+
  labs(title="All subjects", x="Condition", y="Response Time (ms)")+
  scale_color_manual(name="Center", labels=c("Cross", "Symbol", "Large Symbol"), values=c("#E64B35", "#4DBBD5", "#01A087"))+
  theme_bw(base_size=14)+
  coord_cartesian(ylim = c(0,8000))+
  theme(panel.grid = element_blank())
#ANOVA Analysis

anova1 <- ezANOVA(data1, dv = RT, wid = subject_id, within = center, between = condition)
anova1$ANOVA


# second ANOVA (participants who followed instructions) clean data

l_participants <- data %>%
  filter(condition=="language") %>%
  filter(trial_index==120) %>%
  filter(key_press==65)

a_participants <- data %>%
  filter(condition=="abstract") %>%
  filter(trial_index==119) %>%
  filter(key_press==76)

subjects_who_followed_instructions <- unique(
  c(l_participants$subject_id, a_participants$subject_id)
)

data2 <- data1 %>% 
  filter(subject_id %in% subjects_who_followed_instructions) %>%
  filter(!subject_id %in% subjects_who_completed_experiment_twice)


# plot

data2_summary <- data2 %>% group_by(condition, center) %>%
  summarize(M = mean(RT), SE = sd(RT) / sqrt(n()))

plot_restricted <- ggplot(data2, aes(x=condition, y=RT, color=center)) + 
  geom_jitter(alpha=0.1, position = position_jitterdodge(jitter.width=0.2))+
  geom_point(data=data2_summary, mapping=aes(y=M), position=position_dodge(width=0.75), size=3)+
  geom_errorbar(data=data2_summary, mapping=aes(y=M, ymin=M-SE, ymax=M+SE), position=position_dodge(width=0.75), width=0.2)+
  labs(title="Subjects who used intended strategy", x="Condition", y="Response Time (ms)")+
  scale_color_manual(name="Center", labels=c("Cross", "Symbol", "Large Symbol"), values=c("#E64B35", "#4DBBD5", "#01A087"))+
  theme_bw(base_size=14)+
  coord_cartesian(ylim = c(0,8000))+
  theme(panel.grid = element_blank())

#ANOVA Analysis

data2 <- data2 %>%
  mutate(subject_id = factor(subject_id),
         center = factor(center),
         condition = factor(condition))

anova2 <- ezANOVA(data2, dv = RT, wid = subject_id, within = center, between = condition)
anova2$ANOVA

# create table of who used language and who didn't

a_participants <- data %>%
  filter(condition=="abstract") %>%
  filter(trial_index==119) %>%
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


































