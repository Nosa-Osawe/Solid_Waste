
library(readxl)
library(tidyverse)

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

s.waste %>% 
  filter(Component=="Organic",
         Type=="Yam & potatoe peel")


