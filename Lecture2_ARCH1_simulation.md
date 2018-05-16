```r
par(mar=c(3,3,3,1),mfrow=c(3,2), mgp=c(1.8,0.4,0)) #设置画板格式，3,3,3,1是边距

alpha0 <- 0.1
alpha1 <- 0.9
set.seed(20170222) #设定随机数种子
a <- 1:2000       #data structure
sigma2 <- a       #data structure
sigma2[1] <- alpha0/(1-alpha1)          #unconditional variance,用长期波动率替代初始值
a[1] <- rnorm(1)*sqrt(sigma2[1])   

for(i in 2:2000) 
{
  sigma2[i] = alpha0 + alpha1*a[i-1]*a[i-1]
  a[i] = sqrt(sigma2[i])*rnorm(1)
}
a = a[1001:2000] #去掉前面数据来去掉初值的影响
sigma = sqrt(sigma2[1001:2000])
plot(a, type="l", xlab='t', ylab='', main='(a) ARCH(1) time series', col=4)
hist(a, xlab='', nclass=25, probability=T, col=3,  ylab='', main='(b) Histogram')

mu <- mean(a)
sd <- sqrt(var(a))
h <- 6*sd/200
vv <- 1:200
vv[1] <- mu-3*sd #第一个值是均值减去3sigma
for(i in 2:200) vv[i] = vv[i-1]+h
y = dnorm(vv,mu,sd) #求出相应点下的密度值
lines(vv,y, col=2,lwd=2)

plot(sigma, type="l", xlab='t', ylab='', main='(c) Conditional STD', col=4)

qqnorm(a, xlab='Normal quantile', ylab='ARCH quantile', main='(d) QQ-plot', col=4)
qqline(a, col=2)

acf(a, lag=30, main='(e) ACF', ylab='', col="green") #检验是否是弱相关，即二阶矩相关
acf(a*a, lag=30, main='(f) ACF of squared series', ylab='', col="green")
```
