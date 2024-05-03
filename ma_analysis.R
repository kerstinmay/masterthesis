library(tidyverse)
library(rstatix)

pre_post_data <- read.csv2("data_all.csv")
pre_post_data <- pre_post_data %>% 
  mutate(
    Suchformat = ifelse(Suchformat == "websearch", "Websearch", Suchformat)
  )

pre_post_data %>% 
  group_by(task) %>% 
  summarise(
    M_vor = round(mean(Sicherheit), 2),
    SD_vor = round(sd(Sicherheit), 2),
    M_nach = round(mean(Sicherheit2), 2),
    SD_nach = round(sd(Sicherheit2), 2),
  )

# Unterschied Sicherheit Pre/Post based on Task
pre_post_data %>% 
  pivot_longer(cols = c(Sicherheit2, Sicherheit), names_to = "Time", values_to = "Sicherheit") %>% 
  group_by(task) %>% 
  t_test(Sicherheit ~ Time, paired = TRUE)

# Unterschied Sicherheit Pre/Post based on Richtig
pre_post_data %>% 
  pivot_longer(cols = c(Sicherheit, Sicherheit2), names_to = "Time", values_to = "Sicherheit") %>% 
  group_by(Richtig) %>% 
  t_test(Sicherheit ~ Time, paired = TRUE)

# Unterschied Sicherheit Pre/Post based on Suchformat
pre_post_data %>% 
  pivot_longer(cols = c(Sicherheit2, Sicherheit), names_to = "Time", values_to = "Sicherheit") %>% 
  group_by(Suchformat) %>% 
  t_test(Sicherheit ~ Time, paired = TRUE)

# Unterschied Pre-Sicherheit based on Richtig
pre_post_data %>% 
  t_test(Sicherheit ~ Richtig, paired = FALSE, var.equal = TRUE)

# Unterschied Sicherheit Pre/Post generell
pre_post_data %>% 
  pivot_longer(cols = c(Sicherheit2, Sicherheit), names_to = "Time", values_to = "Sicherheit") %>% 
  t_test(Sicherheit ~ Time, paired = TRUE)

pre_post_data %>% 
  group_by(Suchformat) %>% 
  summarise(
    M_vor = round(mean(Sicherheit), 2),
    SD_vor = round(sd(Sicherheit), 2),
    M_nach = round(mean(Sicherheit2), 2),
    SD_nach = round(sd(Sicherheit2), 2),
  )

pre_post_data %>% count(Suchformat)

pre_post_data_web <- pre_post_data[pre_post_data$Suchformat == 'Websearch', ]
pre_post_data_chat <-pre_post_data[pre_post_data$Suchformat == 'Chat', ]
pre_post_data_both <-pre_post_data[pre_post_data$Suchformat == 'beides', ]

# Web Unterschied Sicherheit Pre/Post based on Richtig
pre_post_data_web %>%
  pivot_longer(cols = c(Sicherheit2, Sicherheit), names_to = "Time2", values_to = "Sicherheit") %>% 
  group_by(Richtig) %>% 
  t_test(Sicherheit ~ Time2, paired = TRUE)

# both Unterschied Sicherheit Pre/Post based on Richtig
pre_post_data_both %>%
  pivot_longer(cols = c(Sicherheit2, Sicherheit), names_to = "Time2", values_to = "Sicherheit") %>% 
  group_by(Richtig) %>% 
  t_test(Sicherheit ~ Time2, paired = TRUE)

# chat Unterschied Sicherheit Pre/Post based on Richtig
pre_post_data_chat %>%
  pivot_longer(cols = c(Sicherheit2, Sicherheit), names_to = "Time", values_to = "Sicherheit") %>% 
  group_by(Richtig) %>% 
  t_test(Sicherheit ~ Time)


# Unterschied Post-Sicherheit based on Suchformat
summary(aov(Sicherheit2 ~ Suchformat, data = pre_post_data))

# Post-Sicherheit je nach Suchsystem (wenn Antwort richtig)
pre_post_data %>% 
  filter(Richtig == "ja") %>% 
  anova_test(Sicherheit2 ~ Suchformat)

# Post-Sicherheit je nach Suchsystem (wenn Antwort falsch)
pre_post_data %>% 
  filter(Richtig == "nein") %>% 
  anova_test(Sicherheit2 ~ Suchformat)

# Unterschied richtig based on Suchformat
pre_post_data %>%
  pivot_longer(cols = c(Sicherheit2, Sicherheit), names_to = "Time", values_to = "Sicherheit") %>% 
  group_by(Richtig, Suchformat) %>% 
  t_test(Sicherheit ~ Time, var.equal = TRUE)


#Richtig based on Suchformat
chisq_test(table(pre_post_data$Suchformat, pre_post_data$Richtig))