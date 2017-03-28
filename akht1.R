iris[1,3]
head[iris]
x=iris
tail(iris)
x(1,1:4)

#1
x <- list("a" = mean(iris$Petal.Width), "b" = mean(iris$Sepal.Width), "c" = mean(iris$Petal.Length), "d"=mean(iris$Sepal.Length))
x
typeof(x)
x
#2
ir=iris[c(1:4)] #ирис без последней колонки
y=vector()
for(i in 1:length(iris$Sepal.Length)){
  y[i] <- mean(t(ir[i,])) #в У вектор из средних
  }
y
#3 нуклеотиды
nucl <- c("A", "T", "G", "C") 
DNA = nucl[runif(1000, 1, 5)] #массив случайных нуклеотидов
dna=summary(factor(DNA)) #сколько их всего
dna
dna_at=dna[-c(2, 3)] #убираем лишние
ratio=dna_at/length(DNA) #доля в общей цепочки
ratio
#4 буквы
txt = letters[runif(1000, 1, 26)] #массив случайных букв
txt=factor(letters[runif(1000, 1, 26)])
txt1=summary(txt)[c("a", "e","i","o","u","y")] #сколько их всего
txt1
n_gl=sum(txt1)
# 5
x <- factor(iris[order(iris$Petal.Length),]$Species)
# 6 Напишите функцию для рассчета медианы вектора
median <- function(x) {
  z=sort(x)
  if((length(z)%%2)!=0){
    result = z[(length(x)/2)+1]
  }
  else
    result = (z[length(x)/2]+z[length(x)/2+1])/2
  return(result)
}
median(iris$Sepal.Length)
#7
library(ggplot2)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_jitter(alpha = 0.6) + facet_grid(. ~ Species)
#8
diamonds
levels(factor(diamonds$clarity))
x=levels(factor(diamonds$clarity))
y=vector()
for (i in 1:length(x)) {
  y[i]=mean(diamonds$price[(diamonds$price>1000) & diamonds$clarity==x[i]])
}
y
##Спирмен
spirman <- function(x, y) {
  if(length(x)==length(y) && is.vector(x)==TRUE && is.vector(y)==TRUE){
    p=1
    n=length(x)
    rx=rank(x)
    ry=rank(y)
    for(i in 1:n)
      {
        p = p - ((( rx[i] - ry[i] )^2)*6)/(n*(n^2-1))
      }
  }
  else{
    if(is.vector(x)==TRUE && is.vector(y)==TRUE)
      print("Ошибка. Вектора должны быть одинаковой длины")
    else
      print("Ошибка.Входные данные должны быть векторами")
    p=-1
  }
  return(p)
}
spirman(iris$Sepal.Length,iris$Petal.Length)

# регрессия 1 попытка
read.csv(file="D:/eddypro.csv", fill = TRUE) # 1 column
ncol <- max(count.fields(file="D:/eddypro.csv", sep = ","))
data = data.frame(read.csv(file="D:/eddypro.csv", fill = TRUE, header = FALSE,
                           col.names = paste0("V", seq_len(ncol))))

data = data.frame(data)

names = c(t(data[2,]))
colnames(data)<- c(names)
data <- data[-c(1:3),]
rownames(data)<- c(1:length(data[,1]))
names
data[data == '-9999.0']=NA

sapply(data, class)


for(i in 1:length(data[1,])){
  data[,i]=as.numeric(as.character(t(data[,i])))
}

reg <- lm(co2_flux ~ Tau + H + LE + h2o_flux + co2_molar_density, data = data)
summary(reg)

#регрессия 2 попытка
library("tidyverse")
library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
tbl = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
tbl = tbl[-1,]
tbl
glimpse(tbl)
tbl = select(tbl, -(roll))
tbl = tbl %>% mutate_if(is.character, factor)
#names(tbl) =  str_replace_all(names(tbl), "[!]","_emph_")
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(tbl)
sapply(tbl,is.numeric)
tbl_numeric = tbl[,sapply(tbl,is.numeric)]
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ]
tbl_numeric <- drop_na(tbl_numeric)
names(tbl_numeric)

cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux)
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""))
formula1 = as.formula(paste("co2_flux", paste(vars,collapse = "+"), sep=""))
teaching_tbl = sample_n(tbl, floor(length(tbl$date)*.7))
testing_tbl = sample_n(tbl, floor(length(tbl$date)*.3))
tbl_numeric = filter(tbl_numeric, DOY > 151)
tbl_numeric = filter(tbl_numeric, DOY < 243)
mod = lm(co2_flux ~ (Tau+rand_err_Tau+H+rand_err_H+LE+qc_LE+rand_err_LE+co2_flux+h2o_flux+qc_h2o_flux+rand_err_h2o_flux+H_strg+co2_molar_density+h2o_time_lag+sonic_temperature+air_temperature+air_density+air_molar_volume+es+RH+VPD+max_speed+u_star_+TKE+T_star_+un_Tau+un_H+un_LE+un_co2_flux+un_h2o_flux+u_var+v_var+w_var+h2o_var+w_div_ts_cov+w_div_co2_cov+w_div_h2o_cov+flowrate)^2, data = tbl_numeric)
summary(mod)
resid(mod)
coef(mod)
names(tbl_numeric)
qplot(co2_flux, DOY, data = tbl_numeric, alpha = I(1/10)) + theme_bw() + geom_line(aes(y = predict(mod)))
qplot(co2_flux, predict(mod), data = tbl_numeric, geom = "line")
qplot(flowrate, co2_flux, data = tbl_numeric, alpha = I(1/10)) + theme_bw() + geom_line(aes(y = predict(mod)))
#lm(earn ~ . - age, data = wages)
anova(mod)
