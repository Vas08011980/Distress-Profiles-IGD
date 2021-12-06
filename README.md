# Distress-Profiles-IGD
library(haven)
library(tidyLPA)
library(dplyr)
library(ggplot2)
library(tidyverse)

data <- read_sav("Masked dataset.sav") ### File location should be inserted between the brackets
View(data)

######################################################################################
######### Estimating LPA models (1 to 6) with different number of classes (1:7) ######
######################################################################################

Models <- data%>%
  select("Depression","Anxiety","Stress")%>%
  single_imputation()%>%
  estimate_profiles(1:6, variances = c("equal", "varying", "equal", "varying"),
                    covariances = c("zero", "zero", "equal", "varying"))%>%
  compare_solutions(statistics = c("AIC","BIC","AWE", "CLC", "KIC"))

Models

######################################################################################
######### Estimating LPA model 1 with 3 classes (CVDP) ###############################
######################################################################################

Final_model <- data%>%
  select("Depression","Anxiety","Stress")%>%
  single_imputation()%>%
  estimate_profiles(3, variances="varying",covariances="zero")

Final_model

Final_model2 <- data%>%
  select("Depression","Anxiety","Stress")%>%
  single_imputation()%>%
  estimate_profiles(2, variances="varying",covariances="varying")

Final_model2

######################################################################################
########################### Creating plot ############################################
######################################################################################


L5 <- data%>%
  select(51:54)%>%
  na.omit() %>%
  pivot_longer(cols=c(ZDepression,ZAnxiety,ZStress), names_to="Model_Indicators",
               values_to="Z_Scores")

L5$Profiles <- L5$Class
L5$Profiles <- as.factor(L5$Profiles)
levels(L5$Profiles) <- c("High comorbidity","Medium comorbidity",
                         "Low comorbidity")

L6 <- L5%>%
  group_by(Profiles,Model_Indicators) %>%
  summarize(Z=mean(Z_Scores))



ggplot(L6, aes(x = Model_Indicators, y = Z, group = Profiles, color=Profiles)) + 
  geom_point(size = 2) + geom_line() +
  labs(x= "Model Indicators", y = "Z scores") + 
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold")) + 
  scale_fill_discrete(name = "Distress Profiles") + 
  scale_y_continuous(breaks=seq(-1.0, 2.0, by = 0.5)) +
  scale_x_discrete(labels=c("ZAnxiety"="Anxiety","ZDepression"="Depression","ZStress"="Stress"),
                   limits = c("ZDepression","ZAnxiety","ZStress"))




################################################################################
###### Plotting relationships between DAS and IGD discriminated by class #######
################################################################################
library(patchwork)
library(plyr)

data_3 <- read_sav("Masked dataset.sav")
str(data_3$Class)
data_3 <- data_3[16:50]
data_3$Class_DAS <- as.factor(data_3$Class)
data_3$Class_DAS <- revalue(data_3$Class_DAS,
                        c("1"="High comorbidity","2"="Medium comorbidity",
                          "3"="Low comorbidity"))


p1 <-   ggplot(data_3, aes(Depression, GamingTotal, color = Class_DAS, size = GamingTotal)) + 
  geom_point() + 
  labs(colour = "Latent profiles") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_text(face = "bold"),
        legend.title = element_blank(),
        legend.position = 'none') + 
  scale_colour_manual(values=c("#999999","#E69F00","#56B4E9")) + 
  scale_size(range = c(1,4))

p2 <-   ggplot(data_3, aes(Anxiety, GamingTotal, color = Class_DAS, size = GamingTotal)) + 
  geom_point() + 
  labs(colour = "Latent profiles") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face ="bold"),
        legend.position = 'none',
        axis.title.y = element_blank()) + 
  scale_colour_manual(values=c("#999999","#E69F00","#56B4E9")) + 
  scale_size(range = c(1,4))

p3 <- ggplot(data_3, aes(Stress, GamingTotal, color = Class_DAS, size = GamingTotal)) + 
  geom_point() +
  labs(colour = "Latent profiles") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face ="bold"),
        axis.title.y = element_blank()) + 
  scale_colour_manual(values=c("#999999","#E69F00","#56B4E9")) + 
  scale_size(range = c(1,4))
  

(p1 | p2 | p3) + plot_annotation(title = "DAS as a function of IGD") &
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = unit(c(0.5, 0.1, 0.5, 0.1), "cm"))
