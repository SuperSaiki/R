```r
#写代码中注意删除变量释放内存，R中内存有上限
sp500 <- read.table('C:\\Users\\Administrator\\Desktop\\R code\\用包拟合ARCH_GARCH_EGARCH\\egarchexample.R",header=TRUE)
sp500$Data <- as.Date(as.character(sp500$Data),format="%Y-%m-%d")
library(xts)
sp500plr <- 100*diff(log(sp500[,7]))
sp500plr <- xts(sp500plr,sp500$Data[-1])
library(rugarch)
m1 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),  mean.model=list(armaOrder=c(0,0), include.mean=F))
sp500EG=ugarchfit(data = sp500plr,spec=m1)
show(sp500EG)
```
