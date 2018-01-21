## Exerise 1
library('faraway')
library('tibble')
new=as_tibble(diabetes)
dim(new)
mean(new$hdl,na.rm=TRUE)
mean(new$hdl[new$gender=='female'])
plot(new$weight,new$chol,main='scatter plot', col='blue',xlab='weight', ylab='total cholesterol')
boxplot(new$hdl~new$gender, col=c('red','green'),main='boplots',xlab='gender',ylab='total cholesterol')

## Exercise 2
nutrition<-read.csv('/Users/Constance/Downloads/hw00/nutrition.csv')
hist(nutrition$Calories, xlim=c(0,1000))
plot(4*I(nutrition$Protein)+4*I(nutrition$Carbs)+9*I(nutrition$Fat)+2*I(nutrition$Fiber),I(nutrition$Calories))

## Exercise 3
a = 1:10
b = 10:1
c = rep(1, times = 10)
d = 2 ^ (1:10)
sum_of_squares<- function(x){
  return(sum(x^2))
}
sum_of_squares(x=a)
sum_of_squares(x = c(c, d))
rms_diff<- function(x,y){
  s=sum((x-y)^2)
  n=max(length(x),length(y))
  return(sqrt(s/n))
}
rms_diff(x = a, y = b)
rms_diff(x = d, y = c)
rms_diff(x = d, y = 1)
rms_diff(x = a, y = 0) ^ 2 * length(a)

