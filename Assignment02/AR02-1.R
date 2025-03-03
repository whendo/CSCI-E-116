################################################################### 
# AR02: Answer key to Assignment 2
# By William Yu, Harvard Extension
# 9/5/2022
################################################################### 
setwd("C:/Users/wiyu/documents/Zip08/2022 Q4 Fall_Harvard/R/")
source("H02a_crossSection.R", echo = TRUE)

covid2022 = read.csv("covid_deaths_usafacts_aug2022.csv") 
covid2022 = covid2022[,c("countyFIPS","X8.15.2022")]  # COVID119 cumulative deaths as of Aug 15, 2022
names(covid2022)=c("county", "death2208")
new3 = left_join(new2, covid2022, by="county") 
new3 = new3 %>% mutate(deathp2208 = 1000000*death2208/population) 

model01 = lm(deathp2208 ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv + hosp + vcrime +
            commute_p + wfh + computer + p_nursehome + p_liquor + prematured + drinking  + lowbirthw, data=new3)
summary(model01) 

model05 = lm(deathp2208 ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
            mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv + hosp + vcrime +
            commute_p + wfh + computer + p_nursehome + p_liquor + prematured + drinking  + lowbirthw + statename, data=new3)
summary(model05) 


# Compared January 2021 to August 2022:
# (1) Adj. R2 increases from 0.35 to 0.47 (Model 1) from 0.48 to 0.61 (Model 5)
# (2) Age 85 factor reduces its significance (by both coefficient and t-value)
# (3) Race factor reduces its significance
# (4) Health factor (prematured & lowbirthw) increases its significance
# (5) Other significant factors remain similar significance or increase


" Model 1 Regression result
Estimate Std. Error t value Pr(>|t|)    
(Intercept)  8.663e+03  1.070e+03   8.099 8.33e-16 ***
  a85a         1.923e+02  4.257e+01   4.518 6.51e-06 ***
  a7584        2.008e+02  3.352e+01   5.991 2.36e-09 ***
  a6574       -7.247e+01  2.830e+01  -2.561  0.01049 *  
  a5564        2.887e+01  2.771e+01   1.042  0.29764    
a2034       -2.177e+01  1.505e+01  -1.446  0.14820    
pdensity     2.401e-02  2.643e-02   0.908  0.36374    
pop          2.533e-05  8.058e-05   0.314  0.75330    
aindian      4.771e+00  4.971e+00   0.960  0.33724    
black        5.661e+00  3.644e+00   1.554  0.12040    
latino       1.625e+01  2.642e+00   6.151 8.85e-10 ***
  asian       -1.132e+00  1.232e+01  -0.092  0.92681    
sparent      5.409e+01  1.848e+01   2.928  0.00345 ** 
  mincome     -2.912e-03  4.306e-03  -0.676  0.49893    
poverty      1.187e+01  1.080e+01   1.099  0.27190    
chci        -1.705e+01  5.498e+00  -3.100  0.00195 ** 
  lcp         -2.210e+00  6.418e+00  -0.344  0.73059    
ur          -7.547e-01  1.467e+01  -0.051  0.95897    
disable      4.675e+00  1.076e+01   0.434  0.66411    
hi_pub      -1.838e+01  6.310e+00  -2.914  0.00360 ** 
  demv        -2.418e+01  3.237e+00  -7.472 1.07e-13 ***
  hosp         2.020e-02  1.609e-02   1.255  0.20956    
vcrime      -1.353e-01  1.542e-01  -0.877  0.38042    
commute_p    3.603e+01  1.524e+01   2.364  0.01813 *  
  wfh         -6.003e+01  1.207e+01  -4.974 6.97e-07 ***
  computer    -4.252e+01  7.174e+00  -5.927 3.49e-09 ***
  p_nursehome  3.289e+02  5.556e+01   5.920 3.64e-09 ***
  p_liquor     6.462e+02  5.645e+02   1.145  0.25240    
prematured   1.455e-01  1.828e-02   7.958 2.56e-15 ***
  drinking    -2.168e+01  1.150e+01  -1.885  0.05954 .  
lowbirthw    7.059e+01  2.219e+01   3.181  0.00149 ** 
"
