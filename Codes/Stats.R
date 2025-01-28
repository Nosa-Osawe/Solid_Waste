
library(readxl)
library(tidyverse)
library(agricolae)
library(corrplot)

# Data is not real. This is purely fabricated
# Just to keep things in shape!
set.seed(999)

# Alimosho LGA data
a001_050<-as.vector(rnorm(50, mean =76, sd= 30)) 
a051_100 <-as.vector(rnorm(50, mean = 60, sd=20.1))
a101_150 <-as.vector(rnorm(50, mean= 16, sd= 8))
a151_200 <-as.vector(rnorm(50, mean = 110, sd=30.4))

a201_250 <-as.vector(rnorm(50, mean = 64, sd= 14))
a251_300 <- as.vector(rnorm(50, mean = 52, sd=9))
a301_350 <- as.vector(rnorm(50, mean = 40, sd= 4))
a351_400 <- as.vector(rnorm(50, mean = 48, sd = 13.2))

a401_450 <- as.vector(rnorm(50, mean = 40, sd= 18.1))
a451_500 <- as.vector(rnorm(50, mean = 50, sd = 10.3))
a501_550 <- as.vector(rnorm(50, mean = 24, sd = 20.5))
a551_600 <- as.vector(rnorm(50, mean = 20, sd= 7.01))

a601_650 <- as.vector(rnorm(50, mean = 50, sd= 20))
a651_700 <- as.vector(rnorm(50, mean = 80, sd = 22))

# Ikeja LGA data

I701_750 <- as.vector(rnorm(50, mean = 124, sd= 18.1))
I751_800 <- as.vector(rnorm(50, mean = 60, sd = 14.3))
I801_850 <- as.vector(rnorm(50, mean = 24, sd = 10.6))
I851_900 <- as.vector(rnorm(50, mean = 156, sd = 19.5))

I901_950 <- as.vector(rnorm(50, mean = 94, sd = 21.2))
I951_1000 <- as.vector(rnorm(50, mean = 60, sd= 12.44))
I1001_1050 <- as.vector(rnorm(50, mean = 60, sd= 20.01))
I1051_1100 <- as.vector(rnorm(50, mean = 90, sd= 14.3))

I1101_1150 <- as.vector(rnorm(50, mean = 50, sd= 13.89))
I1151_1200 <- as.vector(rnorm(50, mean = 22, sd = 14.93))
I1201_1250 <- as.vector(rnorm(50, mean = 24, sd = 15.1))
I1251_1300 <- as.vector(rnorm(50, mean = 60, sd = 15))

I1301_1350 <- as.vector(rnorm(50, mean = 70, sd= 19))
I1351_1400 <- as.vector(rnorm(50, mean = 80, sd= 18.43))

# Eti-Osa LGA
e1401_1450 <- as.vector(rnorm(50, mean = 102, sd = 23.01))
e1451_1500 <- as.vector(rnorm(50, mean = 64, sd= 23))
e1501_1550 <- as.vector(rnorm(50, mean = 50, sd= 12))
e1551_1600 <- as.vector(rnorm(50, mean = 170, sd= 14))

e1601_1650 <- as.vector(rnorm(50, mean = 120, sd = 20.93))
e1651_1700 <- as.vector(rnorm(50, mean = 80, sd= 18.0))
e1701_1750 <- as.vector(rnorm(50, mean = 60, sd= 46.2))
e1751_1800 <- as.vector(rnorm(50, mean = 144, sd= 34))

e1801_1850 <- as.vector(rnorm(50, mean = 72, sd= 8.56))
e1851_1900 <- as.vector(rnorm(50, mean = 20, sd= 6.34))
e1901_1950 <- as.vector(rnorm(50, mean = 70, sd = 20.01))
e1951_2000 <- as.vector(rnorm(50, mean = 100, sd = 12.02))

e2001_2050 <- as.vector(rnorm(50, mean = 74, sd = 9.7))
e2051_2100 <- as.vector(rnorm(50, mean = 164, sd= 16.2))


# Everytime you load these stuffs (above) it resets the values. so, take note!!!!


Forged <- cbind(a001_050,a051_100,a101_150,a151_200,
                a201_250,a251_300,a301_350,a351_400,
                a401_450,a451_500,a501_550,a551_600,
                a601_650,a651_700,
                
                I701_750,I751_800,I801_850,I851_900,
                I901_950,I951_1000,I1001_1050,I1051_1100,
                I1101_1150,I1151_1200,I1201_1250,I1251_1300,
                I1301_1350,I1351_1400,
                
                e1401_1450, e1451_1500, e1501_1550, e1551_1600,
                e1601_1650, e1651_1700, e1701_1750, e1751_1800,
                e1801_1850, e1851_1900, e1901_1950, e1951_2000,
                e2001_2050, e2051_2100)

View(Forged) # OK
length(Forged) # OK

Forged <- as.data.frame(Forged)



write.csv( Forged, 
          file = "C:\\Users\\DELL\\Documents\\Git in R\\Solid_Waste\\Data\\forged.csv")

####################################################################################



s.waste <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Solid_Waste\\Data\\Solid_wastes.xlsx",
                      sheet = 'Sheet1')
# view(s.waste) # OK

lm_function <- function(dependent_var, data,  
                        alpha = 0.05) { # -we can adjust the alpha level here
  
  formula <- as.formula(paste(dependent_var, "~ Local_Govt"))
  lm_summary <- summary(lm_model <- lm(formula, data = data))
  aov_summary <- summary(aov_model <- aov(formula, data = data))
  hsd_groups <- agricolae::HSD.test(aov_model, trt = c("Local_Govt"), alpha = alpha, 
                                    group = TRUE)$groups
  
  return(list(
    lm_summary = lm_summary,
    aov_summary = aov_summary,
    hsd_groups = hsd_groups
  ))
}


s.waste %>% 
  filter(Component=="Organic",
         Type=="Yam & potatoe peel") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g)) +
  geom_boxplot(color ="darkgreen")+
  labs(
    title = "Yam & potatoe peel",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Yam & potatoe peel",])

s.waste %>% 
  filter(Type=="Plantain peel") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g), ) +
  geom_boxplot(color ="darkgreen")+
  labs(
    title = "Plantain peel",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Plantain peel",])

s.waste %>% 
  filter(Type=="Egg shell") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g), ) +
  geom_boxplot(color ="darkgreen")+
  labs(
    title = "Egg shell",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Egg shell",])


s.waste %>% 
  filter(Type=="Vegetables and Fruits") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g), ) +
  geom_boxplot(color ="darkgreen")+
  labs(
    title = "Vegetables and Fruits",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Vegetables and Fruits",])


s.waste %>% 
  filter(Type=="Empty drink cans") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g), ) +
  geom_boxplot(color ="darkblue")+
  labs(
    title = "Empty Drink Cans",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Empty drink cans",])

s.waste %>% 
  filter(Type=="Bottle caps") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g)) +
  geom_boxplot(color ="darkblue")+
  labs(
    title = "Bottle caps",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Bottle caps",])



s.waste %>% 
  filter(Type=="Bent spoons and cups") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g)) +
  geom_boxplot(color ="darkblue")+
  labs(
    title = "Bent spoons and cups",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Bent spoons and cups",])


s.waste %>% 
  filter(Type=="Electrical and Mechanical parts") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g)) +
  geom_boxplot(color ="darkblue")+
  labs(
    title = "Electrical and Mechanical parts",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Electrical and Mechanical parts",])

s.waste %>% 
  filter(Type=="Pet bottles") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g)) +
  geom_boxplot(color ="darkred")+
  labs(
    title = "Pet bottles",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Pet bottles",])

s.waste %>% 
  filter(Type=="Empty Water Sachets") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g)) +
  geom_boxplot(color ="darkred")+
  labs(
    title = "Empty Water Sachets",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Empty Water Sachets",])

s.waste %>% 
  filter(Type=="Plastic Bottles") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g)) +
  geom_boxplot(color ="darkred")+
  labs(
    title = "Plastic Bottles",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Plastic Bottles",])

s.waste %>% 
  filter(Type=="Fast-Food Packets") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g)) +
  geom_boxplot(color ="darkred")+
  labs(
    title = "Fast-Food Packets",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Fast-Food Packets",])


s.waste %>% 
  filter(Type=="Paper and Cartons") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g)) +
  geom_boxplot(color ="black")+
  labs(
    title = "Paper and Cartons",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Paper and Cartons",])

s.waste %>% 
  filter(Type=="Glass") %>% 
  select(Local_Govt, Weight_g) %>% 
  ggplot(aes(x = Local_Govt, y = Weight_g)) +
  geom_boxplot(color ="black")+
  labs(
    title = "Glass",
    x = "Local Government",
    y = "Weight (g)",
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) 
lm_function("Weight_g",s.waste[s.waste$Type=="Glass",])

##############################################################################

s.waste %>% 
  group_by(Component,Local_Govt, House_ID) %>% 
  summarise(sum_w_g=sum(Weight_g)) %>% 
  select(Component, sum_w_g) %>% 
  ggplot(aes(x = Component, y = sum_w_g)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), width = 0.6, 
               fill = "lightgrey") +
  stat_summary(geom = "errorbar", fun.data = function(x) {
    data.frame(y = mean(x), 
               ymin = mean(x) - sd(x),
               ymax = mean(x) + sd(x))
  },  position = position_dodge(width = 0.5),
               width = 0.2, colour = "black", size = 0.9) +
  labs(
    title = "Waste Components",
    x = "Wastes",
    y = "Weight (g)",
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
  ) 

component <-s.waste %>% 
  group_by(Component,Local_Govt, House_ID) %>% 
  summarise(sum_w_g=sum(Weight_g)) %>%
  as.data.frame() %>% 
  select(Component, sum_w_g) %>% 
  as.data.frame()
  
summary(component_anova <- aov(sum_w_g~Component,data =component ))
agricolae::HSD.test(component_anova, trt = c("Component"), alpha = 0.05,
                    group = TRUE)$groups
  
component %>% 
  group_by(Component) %>% 
  summarise(sd(sum_w_g))


s.waste %>% 
  select(-Type) %>% 
  group_by(Local_Govt, House_ID) %>% 
  summarise(sum_W_g= sum(Weight_g)) %>% 
  ggplot(aes(x = Local_Govt, y = sum_W_g)) +
  stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.5), 
               width = 0.6, 
               fill = "yellow") +
  stat_summary(
    geom = "errorbar", 
    fun.data = function(x) {
      data.frame(y = mean(x), 
                 ymin = mean(x) - sd(x),
                 ymax = mean(x) + sd(x))
    }, 
    position = position_dodge(width = 0.5),
    width = 0.2, colour = "black", size = 0.9
  ) +
  labs(
    title = " ",
    x = "Local Government",
    y = "Weight (g)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  )

LGA.waste <-s.waste %>% 
  select(-Type) %>% 
  group_by(Local_Govt, House_ID) %>% 
  summarise(sum_W_g= sum(Weight_g)) %>% 
  as.data.frame()

summary(LGA_anova <- aov(sum_W_g~Local_Govt,data =LGA.waste ))
agricolae::HSD.test(LGA_anova, trt = c("Local_Govt"), alpha = 0.05,
                    group = TRUE)$groups

LGA.waste %>% 
  group_by(Local_Govt) %>% 
  summarise(sd(sum_W_g))
  
  
#################################################################################

s.waste<-as.data.frame(s.waste)

s.waste.wide <-s.waste %>%
  select(-Component) %>% 
  pivot_wider(names_from = Type, values_from = Weight_g) %>% 
  as.data.frame() %>% 
  select(-House_ID)

view(s.waste.wide)


# Alimosho correlation
Alimosho_cor <-s.waste.wide %>% 
  filter(Local_Govt=="Alimosho") %>% 
  select(-Local_Govt) %>% 
  cor()

corrplot(Alimosho_cor,  method =c("color"),
         addCoef.col='black',
         number.cex = 0.7,
         tl.cex = 0.8, tl.col = 'black', 
         type = "lower")
title(main = "Correlation Matrix for Waste Data in Alimosho", 
      col.main = "Black", cex.main = 1.2)


##  Ikeja correlation
Ikeja_cor <-s.waste.wide %>% 
  filter(Local_Govt=="Ikeja") %>% 
  select(-Local_Govt) %>% 
  cor()

corrplot(Ikeja_cor,  method =c("color"),
         addCoef.col='black',
         number.cex = 0.7,
         tl.cex = 0.8, tl.col = 'black', 
         type = "lower")
title(main = "Correlation Matrix for Waste Data in Ikeja", 
      col.main = "Black", cex.main = 1.2)

##  Eti-Osa correlation
Eti_Osa_cor <-s.waste.wide %>% 
  filter(Local_Govt=="Eti-Osa") %>% 
  select(-Local_Govt) %>% 
  cor()

corrplot(Eti_Osa_cor,  method =c("color"),
         addCoef.col='black',
         number.cex = 0.7,
         tl.cex = 0.8, tl.col = 'black', 
         type = "lower")
title(main = "Correlation Matrix for Waste Data in Eti-Osa", 
      col.main = "Black", cex.main = 1.2)

## Overall correlation
s.waste.cor <- cor(s.waste.wide [,-1])
corrplot(s.waste.cor,  method =c("color"),
         addCoef.col='black',
         number.cex = 0.7,
         tl.cex = 0.8, tl.col = 'black', 
         type = "lower")
title(main = "Correlation Matrix for Waste Data Across all LGA", 
      col.main = "Black", cex.main = 1.2)


all_waste_corr <- as.data.frame(rbind(Alimosho_cor,
                                Ikeja_cor, Eti_Osa_cor, s.waste.cor))
write.csv(all_waste_corr, 
          "C:\\Users\\DELL\\Documents\\Git in R\\Solid_Waste\\Data\\all_waste_corr.csv")

#####################################################################################


# Prepare the summary statistics in Excel
s.waste %>% 
  group_by(Component, Type, Local_Govt) %>% 
  summarise(mean_weight =round(mean(Weight_g),2) ,
            SD_weight =round(sd(Weight_g), 2)) %>% 
  as.data.frame() %>% 
  write.csv("C:\\Users\\DELL\\Documents\\Git in R\\Solid_Waste\\Data\\summary_stats.csv")

######################################################################################3

head(s.waste)



