#создайте модель множественной линейной регрессии потоков ночных потоков углекислого
#газа за летний период 2013 года по данным измерений методом турбулентной пульсации

library("tidyverse")
library("nycflights13") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 


data = read.csv("eddypro.csv", skip = 1, na = c("","NA","-9999", "-9999.0"), comment = c("[")) 
data = data[-1,]
data = data[data$DOY > 152 & data$DOY < 243,] #Данные за летний период
data = data[data$daytime == FALSE,] #Данные ночных потоков
glimpse(data) #Отобразить данные за летний период

data = select(data, -(roll)) # 
data = data %>% mutate_if(is.character, factor) 

sapply(data,is.numeric) 
data_numeric = data[,sapply(data,is.numeric)] 
data_non_numeric = data[,!sapply(data,is.numeric) ] 

cor_td = cor(data_numeric) 
cor_td 
cor_td = cor(drop_na(data_numeric)) 
cor_td 
cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(co2_flux) 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""))

mod = lm(formula, data = data) 
summary(mod)
anova(mod)
formula1 = co2_flux ~ (rand_err_H + LE + rand_err_co2_flux + h2o_flux + 
                         co2_molar_density + co2_mole_fraction + co2_mixing_ratio + 
                         sonic_temperature + air_temperature + es + T. + un_LE + un_co2_flux + 
                         un_h2o_flux + ts_var + co2_var + w.co2_cov + w.h2o_cov + 
                         co2 + co2.1)
mod1 = lm(formula1, data = data)
anova(mod1)
summary(mod1)

formula2 = co2_flux ~ (rand_err_H +  rand_err_co2_flux + 
                         T. + un_co2_flux + 
                         ts_var + co2_var)

mod2 = lm(formula2, data = data)
anova(mod2)
summary(mod2)
