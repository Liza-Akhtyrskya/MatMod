
#1
x <- list("a" = mean(iris$Petal.Width), "b" = mean(iris$Sepal.Width), "c" = mean(iris$Petal.Length), "d"=mean(iris$Sepal.Length))
x
typeof(x)
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
factor(iris[order(iris$Petal.Length),]$Species)
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

##
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
