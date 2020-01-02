## -----------------------------------------------------------------------------
logrankmethod <- function(x) UseMethod("logrankmethod")
logrankmethod.default <- function(x) "Unknown class"
logrankmethod.logrank <- function(x){
  xx=as.numeric(as.character(x[1:12]))
  if(as.logical(x[13])){
    cat("     permutation TEST\n")
    cat("\n")
    cat("Mantel-Haenszel logrank\n")
    cat("——————————————————————————————————\n")
    cat("Test Statistic:")
    cat(" ",xx[1],"\n")
    cat("       p value:")
    cat(" ",xx[9],"\n")
    cat("\n")
    cat("Cox-Mantel Logrank\n")
    cat("——————————————————————————————————\n")
    cat("Test Statistic:")
    cat(" ",xx[3],"\n")
    cat("       p value:")
    cat(" ",xx[10],"\n")
    cat("\n")
    cat("Generalized Wilcoxon\n")
    cat("——————————————————————————————————\n")
    cat("Test Statistic:")
    cat(" ",xx[5],"\n")
    cat("       p value:")
    cat(" ",xx[11],"\n")
    cat("\n")
    cat("Tarone and Ware\n")
    cat("——————————————————————————————————\n")
    cat("Test Statistic:")
    cat(" ",xx[7],"\n")
    cat("       p value:")
    cat(" ",xx[12],"\n")
  }else{
    cat("     LOGRANK TEST\n")
    cat("\n")
    cat("Mantel-Haenszel logrank\n")
    cat("——————————————————————————————————\n")
    cat("Test Statistic:")
    cat(" ",xx[1],"\n")
    cat("       p value:")
    cat(" ",xx[2],"\n")
    cat("\n")
    cat("Cox-Mantel Logrank\n")
    cat("——————————————————————————————————\n")
    cat("Test Statistic:")
    cat(" ",xx[3],"\n")
    cat("       p value:")
    cat(" ",xx[4],"\n")
    cat("\n")
    cat("Generalized Wilcoxon\n")
    cat("——————————————————————————————————\n")
    cat("Test Statistic:")
    cat(" ",xx[5],"\n")
    cat("       p value:")
    cat(" ",xx[6],"\n")
    cat("\n")
    cat("Tarone and Ware\n")
    cat("——————————————————————————————————\n")
    cat("Test Statistic:")
    cat(" ",xx[7],"\n")
    cat("       p value:")
    cat(" ",xx[8],"\n")
  }
}

## ----warning=FALSE------------------------------------------------------------
data(aml)
library(SC19075)
library(boot)
z=surlogrank(aml$time,aml$cens,aml$group,permutation = TRUE)
logrankmethod(z)

## ----warning=FALSE------------------------------------------------------------
library(SC19075)  
library(boot)
z=surlogrank(aml$time,aml$cens,aml$group,permutation = FALSE)
logrankmethod(z)

## ----warning=FALSE------------------------------------------------------------
library(survival)
survdiff(Surv(time,status)~x,data=aml)

## ----warning=FALSE------------------------------------------------------------
library(SC19075)  
library(survival)
data(lung)
z=surlogrank(lung$time,lung$status-1,lung$sex,permutation = FALSE)
logrankmethod(z)

## ----warning=FALSE------------------------------------------------------------
library(SC19075)  
library(survival)
z=surlogrank(lung$time,lung$status-1,lung$sex,permutation = TRUE)
logrankmethod(z)

## -----------------------------------------------------------------------------
library(survival)
survdiff(Surv(time,status)~sex,data=lung)

## ----eval=FALSE---------------------------------------------------------------
#  #time      实验观察时间点
#  #status    0-1变量，患者处于的状态（1：fail or 0：at risk）
#  #group     实验分组
#  #permutation TRUE只用permutation检验,FALSE不用permutation检验
#  #该函数仅仅用于检验两样本的logrank检验
#  surlogrank <- function(time,status,group,permutation){
#    err=paraerr(time, status, group)                    #检验函数paraerr
#    if(err==1){
#      return("参数输入有误。")
#    }
#    testgroup=data.frame(time,status,group)
#    label=names(table(testgroup$group))                 #组号
#    testgroup2=testgroup[testgroup$group==label[2],]    #第二个组
#    testgroup1=testgroup[testgroup$group==label[1],]    #第一个组
#    J2=length(table(testgroup2$time))                   #第二个组不同的观察时间点
#    testgoup_status1=testgroup[testgroup$status==1,]    #cens=1的实验数据
#    time=as.numeric(names(table(testgoup_status1$time)))#testgoup_status1中不同的时间观察点
#    J=length(time)                                      #不同的时间观察点的个数
#    ##2.二组死的各个观察时间点的死亡人数   O
#    O=vector(mode="numeric",length=J)
#    O2=vector(mode="numeric",length=J2)
#    time_group2=as.numeric(names(table(testgroup2$time)))
#    for(i in 1:J2){
#      O2[i]=sum(testgroup2[testgroup2$time==time_group2[i],]$status)
#    }
#    for(i in 1:J2){
#      O[which(time==time_group2[i])]=O2[i]
#    }
#    ##3.整组在j时间点死亡的人数   d
#    d=vector(length=J)
#    for(i in 1:J){
#      d[i]=sum(testgroup[testgroup$time==time[i],]$status)
#    }
#    ##4.二组在j时间点剩下的人数   Y2
#    population_2=nrow(testgroup2)
#    Y2=vector(length=J)
#    Y2[1]=population_2
#    for(i in 2:J){
#      Y2[i]=population_2-nrow(testgroup2[testgroup2$time<time[i],])
#    }
#    ##5.整组在j时间点剩下的人数
#    population=nrow(testgroup)
#    Y=vector(length=J)
#    for(i in 1:J){
#      Y[i]=population-nrow(testgroup[testgroup$time<time[i],])
#    }
#    ##6.一组在j时间点剩下的人数
#    Y1=vector(length=J)
#    Y1=Y-Y2
#    ##7.expected number of failures
#    E=vector(length=J)
#    for(j in 1:J){
#      E[j]=d[j]*Y2[j]/Y[j]
#    }
#    ##8.variance of the observed number of failures
#    V=vector(length=J)
#    for(j in 1:J)
#      V[j]=(Y1[j]*Y2[j]*d[j]*(Y[j]-d[j]))/((Y[j])^2*(Y[j]-1))
#    ##9.Mantel-Haenszel Logrank Test Z1统计量和p值
#    Z=sum(O-E)/sqrt(sum(V))
#    Z1=Z^2
#    p_value1=1-pchisq(q=Z1,df=1)
#    ##10.Cox-Mantel Logrank Test Z2统计量和p值
#    E1=vector(length=J)
#    for(j in 1:J){
#      E1[j]=d[j]*Y1[j]/Y[j]
#    }
#    d2=O
#    d1=d-d2
#    E2=vector(length=J)
#    for(j in 1:J){
#      E2[j]=d[j]*Y2[j]/Y[j]
#    }
#    Z2=(sum(d1-E1))^2/(sum(E1))+(sum(d2-E2))^2/(sum(E2))
#    p_value2=1-pchisq(q=Z2,df=1)
#    ##11.Generalized Wilcoxon Z3统计量和和p值
#    Z3=(t(Y) %*%(O-E))^2/((t(Y)^2)%*%V)
#    p_value3=1-pchisq(q=Z3,df=1)
#    ##12.Tarone and Ware  Z4统计量和p值
#    Z4=(t(sqrt(Y)) %*%(O-E))^2/(t(Y)%*%V)
#    p_value4=1-pchisq(q=Z4,df=1)
#    ##13.permutation test p值
#    n1=nrow(testgroup1)       #一组人数
#    n2=nrow(testgroup2)       #二组人数
#    S_obs=mean(testgroup2$status)-mean(testgroup1$status)
#    S_obs=abs(S_obs)
#    nsims=1000       #循环次数
#    sim_re=matrix(0,nrow=nsims,ncol=4)    #循环结果
#    for(i in 1:nsims){
#      tmp=sample(c(rep(1,n1),rep(2,n2)))
#      sim_re[i,]=as.numeric(permut(testgroup$time, testgroup$status, tmp))
#    }
#    p_value5=mean(Z1<abs(sim_re[,1]))
#    p_value6=mean(Z2<abs(sim_re[,2]))
#    p_value7=mean(as.numeric(Z3)<abs(sim_re[,3]))
#    p_value8=mean(as.numeric(Z4)<abs(sim_re[,4]))
#    ##14.函数输出
#    logrankoutput=structure(list(Z1,p_value1,Z2,p_value2,Z3,p_value3,Z4,p_value4,p_value5,p_value6,p_value7,p_value8,permutation),class="logrank")
#    return(logrankoutput)
#  }

## ----fig.width=4, fig.height=3------------------------------------------------
par(mar = c(4, 4, 1, 1), mgp = c(2, 1, 0),cex=0.8)
this.range <- seq(0, 20, .05)
plot (this.range, dgamma(this.range,shape = 3), ty="l", main="Gamma Distributions",
      xlab="x", ylab="f(x)")
lines (this.range, dgamma(this.range,shape = 3, rate=0.5), col="red",lwd=2) 
lines (this.range, dgamma(this.range,shape = 3, rate=0.2), col="blue",lwd=2)

## ----fig.width=4, fig.height=3------------------------------------------------
par(mar = c(4, 4, 1, 1), mgp = c(2, 1, 0), cex = 0.8)
this.range <- seq(0, 20, .05)
plot (this.range, pgamma(this.range,shape = 3), ty="l", main="Gamma Distributions",
      xlab="x", ylab="P(X<x)")
lines (this.range, pgamma(this.range,shape = 3, rate=0.5), col="red",lwd=2) 
lines (this.range, pgamma(this.range,shape = 3, rate=0.2), col="blue",lwd=2)

## ----fig.width=4, fig.height=3------------------------------------------------
par(mar = c(4, 4, 1, 1), mgp = c(2, 1, 0), cex = 0.8)
boxplot(cars$dist, xlab = "dist")
plot(cars, pch = 20, col = 'blue')
library(latex2exp)
text(10, 100, '$Y = \\beta_0 + \\beta_1x + \\epsilon$')
fit <- lm(dist ~ speed, data = cars)
abline(fit,lwd = 2,col='red')
library(ggplot2) 
qplot(speed, dist, data = cars) + geom_smooth()

## ----fig.width=4, fig.height=3------------------------------------------------
par(mar = c(4, 4, 1, 1), mgp = c(2, 1, 0),cex=0.8)
pie.sales = c(0.12, 0.30, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) = c("Blueberry", "Cherry", "Apple", "Boston Creme",
                     "Other", "Vanilla Creme")
pie(pie.sales, col = c("blue", "red", "green", "wheat", "orange", "white"))

## -----------------------------------------------------------------------------
library(knitr)
library(kableExtra)
dt <- mtcars[1:5, 1:6]
kable(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover","condensed","responsive"))


## -----------------------------------------------------------------------------
text_tbl <- data.frame(
  Items = c('mpg','cyl','disp','hp','drat','wt'),
  meanings = c(
    "Miles/(US) gallon","Number of cylinders", "Displacement (cu.in.)","Gross horsepower","Rear axle ratio","Weight (1000 lbs)"
  )
)

kable(text_tbl) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "30em", background = "yellow")

## -----------------------------------------------------------------------------
kable(dt) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(5:7, bold = T) %>%
  row_spec(3:5, bold = T, color = "white", background = "#D7261E")

## -----------------------------------------------------------------------------
library(dplyr)
mtcars[1:10, 1:2] %>%
  mutate(
    car = row.names(.),
    mpg = cell_spec(mpg, "html", color = ifelse(mpg > 20, "red", "green")),
    cyl = cell_spec(cyl, "html", color = "white", align = "r", angle = 30, 
                    background = factor(cyl, c(4, 6, 8), 
                                        c("#666666", "#999999", "#BBBBBB")))
  ) %>%
  select(car, mpg, cyl) %>%
  kable(format = "html", escape = F) %>%
  kable_styling("striped", full_width = F)

## ----tidy=TRUE,warning=FALSE,highlight=TRUE-----------------------------------
library(formatR)
gpl = readLines(file.path(R.home(), "COPYING")) 
head(gpl) 

## -----------------------------------------------------------------------------
library(dplyr)
sometext <- strsplit(paste0(
  "You can even try to make some crazy things like this paragraph. ", 
  "It may seem like a useless feature right now but it's so cool ",
  "and nobody can resist. ;)"
), " ")[[1]]
text_formatted <- paste(
  text_spec(sometext, color = spec_color(1:length(sometext), end = 0.9),
            font_size = spec_font_size(1:length(sometext), begin = 5, end = 20)),
  collapse = " ")

## -----------------------------------------------------------------------------
Rayleigh<-function(a){
  set.seed(123456)
  n <- 10000
k <- 0 # counter for acception
j <- 0 # iterations
x <- numeric(n) # initiation
while(k < n){ 
  u <- runif(1) 
  j <- j + 1 
  y <- rgamma(1,shape = 2,rate=1) # random variate from g 
  if(exp(y-y^2/(2*a^2)-a^2/2)>=u){
    k <- k + 1 
    x[k] <- y } 
}
return(x)}

## -----------------------------------------------------------------------------
a=1
x1<-Rayleigh(1)
plot(density(x1), main = bquote(f(x)==x/a^2*exp(-x^2/2*a^2)))
this.range <- seq(0, 5, 0.01) 
f <- function(x) x/(a^2)*exp(-x^2/(2*a^2)) 
lines(this.range,f(this.range), col="red") 
legend("topright", legend = c("A-R", "True"), col=c("black", "red"), lty=1)

hist(x1, probability = TRUE, main=bquote(f(x)==x/a^2*exp(-x^2/2*a^2))) 
lines(this.range, f(this.range), col="red")


## -----------------------------------------------------------------------------
a=2
x2<-Rayleigh(2)
plot(density(x2), main = bquote(f(x)==x/a^2*exp(-x^2/2*a^2)))
this.range <- seq(0, 10, 0.01) 
f <- function(x) x/(a^2)*exp(-x^2/(2*a^2)) 
lines(this.range,f(this.range), col="red") 
legend("topleft", legend = c("A-R", "True"), col=c("black", "red"), lty=1)

hist(x2, probability = TRUE, main=bquote(f(x)==x/a^2*exp(-x^2/2*a^2))) 
lines(this.range, f(this.range), col="red")

## -----------------------------------------------------------------------------
a=3
x3<-Rayleigh(3)
plot(density(x3), main = bquote(f(x)==x/a^2*exp(-x^2/2*a^2)))
this.range <- seq(0, 15, 0.01) 
f <- function(x) x/(a^2)*exp(-x^2/(2*a^2)) 
lines(this.range,f(this.range), col="red") 
legend("topleft", legend = c("A-R", "True"), col=c("black", "red"), lty=1)

hist(x3, probability = TRUE, main=bquote(f(x)==x/a^2*exp(-x^2/2*a^2))) 
lines(this.range, f(this.range), col="red")

## ----fig.width=10, fig.height=3-----------------------------------------------
mixnorm.engi<-function(p1){
  n<-1000
y1<-rnorm(n,0,1)
y2<-rnorm(n,3,1)
r<-sample(c(0,1),n,prob = c(p1,1-p1),replace = TRUE)
z<-r*y2+(1-r)*y1
return(z)
}
par(mfcol=c(1,4))
i=0.05
while(i<1){
hist(mixnorm.engi(i),probability = TRUE,main = i)
  i=i+0.05
}

## -----------------------------------------------------------------------------
Wishart.eigen<-function(sigma,n){
  set.seed(123456)
  T<-matrix(rnorm(n*n,0,1),n,n)       #generate a n*n matrix of normal distribution N(0,1)
  i=1
  num<-c()
  while (i<=n) {
    num[i]<-sqrt(rchisq(1,n-i+1,ncp = 0))
  }
  diag(T)<-num       #generate the diagram of the matrix, which conforms to the sprt of chi-square distribution,the parameter is n-i-1 of the ith value
  T[upper.tri(T)]<-0       #turn the upper of the matrix into 0
  A=T%*%t(T)       #then we get A whose sigma is In
  csigma<-chol(sigma)
  return(csigma%*%A%*%t(csigma))       #for the genral situation,we use Choleski decomposition to get the arbitrary sigam
}

## -----------------------------------------------------------------------------
set.seed(12345)
m <- 1e4
x <- runif(m, min=0, max=pi/3)
theta.hat <- mean(sin(x)) * (3/pi)
print(c(theta.hat,cos(0) - cos(3/pi)))

## -----------------------------------------------------------------------------
set.seed(12345)
m <- 1e4
u <- runif(m/2, min=0, max=1)
v=1-u
u<-c(u,v)
g<-exp(-u)/(1+u^2)
MC1 <-mean(g)
MC1
var1<-var(g)

## -----------------------------------------------------------------------------
set.seed(12345)
m <- 1e4
u <- runif(m, min=0, max=1)
f <- function(u)(1-u)/(1+u^2)
g <- function(u)exp(-u)/(1+u^2)
B <- f(u)
A <- g(u)
covAB <- cor(A, B)
cstar <- -cov(A,B) / var(B) 
T <- g(u) + cstar * (f(u) - pi/4+log(2)/2) 
MC2<-mean(T)
MC2
var2<-var(T)

## -----------------------------------------------------------------------------
set.seed(12345)
m <- 1e4
u <- rexp(m,rate = 1)
f <- function(u)exp(-u)/(1-exp(-1))
g <- function(u)exp(-u)/(1+u^2)*(u>0)*(u<1)
B <- f(u)
A <- g(u)
MC3<-mean(A/B)/(1-exp(-1))
MC3
var3<-var(A/B)/m

## -----------------------------------------------------------------------------
library(knitr)
library(kableExtra)
dt<-data.frame('classes'=c('a','b','c'),'results'=c(MC1,MC2,MC3),variance=c(var1,var2,var3))
kable(dt) %>%
  kable_styling(full_width = F) %>%
  column_spec(1) %>%
  column_spec(2, width = "10em", background = "yellow")%>%
  column_spec(3, width = "10em", background = "yellow")

## -----------------------------------------------------------------------------
q<-seq(0,1,by=0.2)
p<- -log(1-(1-exp(-1))*q) ##divide into intervals
set.seed(12345)
m <- 1e4
f <- function(u)5*exp(-u)/(1-exp(-1))##conditional probability
theta<-numeric(5)
deta<-numeric(5)
for(i in 1:5){
  u <- rexp(m,rate = 1)
  g<-function(u)exp(-u)/(1+u^2)*(u>p[i])*(u<p[i+1]) ##gi(x)=g(x)when locate at the intrval
  m<-g(u)
  n<-f(u)
  a<-m/n
  deta[i]=var(a)
  theta[i]=5*mean(a)/(1-exp(-1))
}
MC4<-sum(theta)
var<-mean(deta)
c(MC4,var)

## -----------------------------------------------------------------------------
set.seed(123456)
alpha = 0.05
n = 20
m = 1000

UCL = numeric(m)
LCL = numeric(m)

for(i in 1:m)
{
    x = rchisq(n, 2) 
    LCL[i] = mean(x) - qt(alpha / 2, df=n-1, lower.tail = FALSE)*sd(x)/sqrt(n)
    UCL[i] = mean(x) + qt(alpha / 2, df=n-1, lower.tail = FALSE)*sd(x)/sqrt(n)
}

mean(LCL < 2 & UCL > 2)

## -----------------------------------------------------------------------------
set.seed(123456)
alpha = 0.05
n = 20
m = 1000

UCL = numeric(m)
LCL = numeric(m)

for(i in 1:m)
{
    x = rnorm(n)+2
    LCL[i] = mean(x) - qt(alpha / 2, df=n-1, lower.tail = FALSE)*sd(x)/sqrt(n)
    UCL[i] = mean(x) + qt(alpha / 2, df=n-1, lower.tail = FALSE)*sd(x)/sqrt(n)
}

mean(LCL < 2 & UCL > 2)

## -----------------------------------------------------------------------------
set.seed(123456)
alpha = 0.05
n = 20
m = 1000

UCL = numeric(m)
for(i in 1:m)
{
  x <- rnorm(n, mean = 0, sd = 2) 
  UCL[i] <- (n-1) * var(x) / qchisq(alpha, df=n-1)
}
mean(UCL>4)

## -----------------------------------------------------------------------------
set.seed(123456)
m=1000
n=20
skewness<-numeric(m)
sk <- function(x) {
xbar <- mean(x)
m3 <- mean((x - xbar)^3)
m2 <- mean((x - xbar)^2)
return( m3 / m2^1.5 )
}
for(i in 1:1000){
  skewness[i]<-sk(rnorm(n))
}
q_0.025<-quantile(skewness,0.025)
q_0.05<-quantile(skewness,0.05)
q_0.95<-quantile(skewness,0.95)
q_0.975<-quantile(skewness,0.975)
qt_0.025<-qnorm(0.025,mean=0,sd=sqrt(6*(n-2)/((n+1)*(n+3))),lower.tail = TRUE)
qt_0.05<-qnorm(0.05,mean=0,sd=sqrt(6*(n-2)/((n+1)*(n+3))),lower.tail = TRUE)
qt_0.95<-qnorm(0.95,mean=0,sd=sqrt(6*(n-2)/((n+1)*(n+3))),lower.tail = TRUE)
qt_0.975<-qnorm(0.975,mean=0,sd=sqrt(6*(n-2)/((n+1)*(n+3))),lower.tail = TRUE)
var_0.025<-0.025*(1-0.025)/(n*dnorm(qt_0.025,0,sd=sqrt(6*(n-2)/((n+1)*(n+3))))^2)
var_0.05<-0.05*(1-0.05)/(n*dnorm(qt_0.05,0,sd=sqrt(6*(n-2)/((n+1)*(n+3))))^2)
var_0.95<-0.95*(1-0.95)/(n*dnorm(qt_0.95,0,sd=sqrt(6*(n-2)/((n+1)*(n+3))))^2)
var_0.975<-0.975*(1-0.975)/(n*dnorm(qt_0.975,0,sd=sqrt(6*(n-2)/((n+1)*(n+3))))^2)

## -----------------------------------------------------------------------------
library(knitr)
library(kableExtra)
dt<-data.frame('probability'=c(0.025,0.05,0.95,0.975),'quantile from experiment'=c(q_0.025,q_0.05,q_0.95,q_0.975),'quantile from theory'=c(qt_0.025,qt_0.05,qt_0.95,qt_0.975),'variance'=c(var_0.025,var_0.05,var_0.95,var_0.975))
kable(dt) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "10em", background = "yellow")%>%
  column_spec(3, width = "10em", background = "yellow")%>%
   column_spec(4, width = "10em", background = "yellow")%>%
   column_spec(5, width = "10em", background = "yellow")

## ----fig.width=6, fig.height=4------------------------------------------------
par(mar = c(4, 4, 1, 1), mgp = c(2, 1, 0),cex=0.8)
this.range <- seq(0, 1, .01)
plot (this.range, dbeta(this.range,10,10), ty="l", main="Beta Distributions",
      xlab="x", ylab="f(x)",ylim=c(0,6))
lines (this.range, dbeta(this.range,1,1), col="red",lwd=2) 
lines (this.range, dbeta(this.range,1/2,1/2), col="blue",lwd=2)
lines (this.range, dbeta(this.range,30,30), col="yellow",lwd=2)
lines (this.range, dbeta(this.range,20,20), col="gray",lwd=2)
lines (this.range, dbeta(this.range,4,4), col="pink",lwd=2)
lines (this.range, dbeta(this.range,2/3,2/3), col="green",lwd=2,lty=2)
text(0.5, 3.5, 'Beta(10,10)', cex = 1)
text(0.1, 1.5, 'Beta(1,1)', cex = 1,col = 'red')
text(0.05, 2, 'Beta(1/2,1/2)', cex = 1,col='blue')
text(0.6, 6, 'Beta(30,30)', cex = 1,col='yellow')
text(0.6, 5, 'Beta(20,20)', cex = 1,col='gray')
text(0.7, 1.5, 'Beta(4,4)', cex = 1,col='pink')
text(0.9, 1.6, 'Beta(2/3,2/3)', cex = 1,col='green')

## -----------------------------------------------------------------------------
sk<-function(x){#computes the sample skewness coeff
  xbar<-mean(x)
  m3<-mean((x-xbar)^3)
  m2<-mean((x-xbar)^2)
  return(m3/m2^1.5)
}
alpha<-.1
n<-30
m<-2500
af<-c(seq(0,1,.05),seq(1,100,0.5))
N<-length(af)
pwr<-numeric(N)  #critical value for the skewness test
cv<-qnorm(1-alpha/2,0,sqrt(6*(n-2)/((n+1)*(n+3))))
for(j in 1:N){
  a<-af[j]
  sktests<-numeric(m)
  for(i in 1:m){
    x<-rbeta(n,a,a)
    sktests[i]<-as.integer(abs(sk(x))>=cv)
  }
pwr[j]<-mean(sktests)
  
}
se<-sqrt(pwr*(1-pwr)/m)
# plot power vs af
library(ggplot2)
df<-data.frame(af=af,power=pwr,upper=pwr+se,lower=pwr-se)
ggplot(df,aes(x=af,y=power))+geom_line()+labs(x=bquote(af))+geom_hline(yintercept=.1,lty=2)+geom_pointrange(aes(ymin=lower,ymax=upper))
plot(af, pwr, type = "b", xlab = bquote(af))
abline(h = .1, lty = 3) 
lines(af, pwr+se, lty = 3) 
lines(af, pwr-se, lty = 3)

## -----------------------------------------------------------------------------
plot(af,dchisq(af,30),ty='l')

## -----------------------------------------------------------------------------
alpha<-.1
n<-30
m<-2500
af<-c(seq(1,30,.1))
N<-length(af)
pwr<-numeric(N)  #critical value for the skewness test
cv<-qnorm(1-alpha/2,0,sqrt(6*(n-2)/((n+1)*(n+3))))
for(j in 1:N){
  a<-af[j]
  sktests<-numeric(m)
  for(i in 1:m){
    x<-rt(n,a)
    sktests[i]<-as.integer(abs(sk(x))>=cv)
  }
pwr[j]<-mean(sktests)
  
}
se<-sqrt(pwr*(1-pwr)/m)
# plot power vs af
library(ggplot2)
df<-data.frame(af=af,power=pwr,upper=pwr+se,lower=pwr-se)
ggplot(df,aes(x=af,y=power))+geom_line()+labs(x=bquote(af))+geom_hline(yintercept=.1,lty=2)+geom_pointrange(aes(ymin=lower,ymax=upper))
plot(af, pwr, type = "b", xlab = bquote(af))
abline(h = .1, lty = 3) 
lines(af, pwr+se, lty = 3) 
lines(af, pwr-se, lty = 3)

## ----warning = FALSE----------------------------------------------------------
library(knitr)
library(kableExtra)
set.seed(123456)
p.hat<-numeric(3)
se.hat<-numeric(3)
n <- 10
alpha <- .05 
m <- 10000#number of replicates 
p <- numeric(m) #storage for p-values 
for (j in 1:m) { 
  x <- rchisq(n, 1) 
  ttest <- t.test(x, mu =1) 
  p[j] <- ttest$p.value }
p.hat[1] <- mean(p < alpha) 
se.hat[1] <- sqrt(p.hat * (1 - p.hat) / m) 
for (j in 1:m) { 
  x <- rexp(n, 1) 
  ttest <- t.test(x, mu =1) 
  p[j] <- ttest$p.value }
p.hat[2] <- mean(p < alpha) 
se.hat[2] <- sqrt(p.hat * (1 - p.hat) / m) 
for (j in 1:m) { 
  x <- runif(n, min=0,max=2) 
  ttest <- t.test(x, mu =1) 
  p[j] <- ttest$p.value }
p.hat[3] <- mean(p < alpha) 
se.hat[3] <- sqrt(p.hat * (1 - p.hat) / m) 
df<-data.frame('Distribution'=c('chisquare','exponential','uniform'),'Type I error rate'=p.hat,'standard error of the estimate'=se.hat)
kable(df,caption = "n=10") %>%
  kable_styling("striped",full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "20em")%>%
  column_spec(3, width = "20em")


## ----warning = FALSE,echo=FALSE-----------------------------------------------
library(knitr)
library(kableExtra)
set.seed(123456)
p.hat<-numeric(3)
se.hat<-numeric(3)
n <- 50
alpha <- .05 
m <- 10000#number of replicates 
p <- numeric(m) #storage for p-values 
for (j in 1:m) { 
  x <- rchisq(n, 1) 
  ttest <- t.test(x, mu =1) 
  p[j] <- ttest$p.value }
p.hat[1] <- mean(p < alpha) 
se.hat[1] <- sqrt(p.hat * (1 - p.hat) / m) 
for (j in 1:m) { 
  x <- rexp(n, 1) 
  ttest <- t.test(x, mu =1) 
  p[j] <- ttest$p.value }
p.hat[2] <- mean(p < alpha) 
se.hat[2] <- sqrt(p.hat * (1 - p.hat) / m) 
for (j in 1:m) { 
  x <- runif(n, min=0,max=2) 
  ttest <- t.test(x, mu =1) 
  p[j] <- ttest$p.value }
p.hat[3] <- mean(p < alpha) 
se.hat[3] <- sqrt(p.hat * (1 - p.hat) / m) 
df<-data.frame('Distribution'=c('chisquare','exponential','uniform'),'Type I error rate'=p.hat,'standard error of the estimate'=se.hat)
kable(df,caption = "n=50") %>%
  kable_styling("striped",full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "20em")%>%
  column_spec(3, width = "20em")


## ----warning = FALSE,echo=FALSE-----------------------------------------------
library(knitr)
library(kableExtra)
set.seed(123456)
p.hat<-numeric(3)
se.hat<-numeric(3)
n <- 200
alpha <- .05 
m <- 10000#number of replicates 
p <- numeric(m) #storage for p-values 
for (j in 1:m) { 
  x <- rchisq(n, 1) 
  ttest <- t.test(x, mu =1) 
  p[j] <- ttest$p.value }
p.hat[1] <- mean(p < alpha) 
se.hat[1] <- sqrt(p.hat * (1 - p.hat) / m) 
for (j in 1:m) { 
  x <- rexp(n, 1) 
  ttest <- t.test(x, mu =1) 
  p[j] <- ttest$p.value }
p.hat[2] <- mean(p < alpha) 
se.hat[2] <- sqrt(p.hat * (1 - p.hat) / m) 
for (j in 1:m) { 
  x <- runif(n, min=0,max=2) 
  ttest <- t.test(x, mu =1) 
  p[j] <- ttest$p.value }
p.hat[3] <- mean(p < alpha) 
se.hat[3] <- sqrt(p.hat * (1 - p.hat) / m) 
df<-data.frame('Distribution'=c('chisquare','exponential','uniform'),'Type I error rate'=p.hat,'standard error of the estimate'=se.hat)
kable(df,caption = "n=500") %>%
  kable_styling("striped",full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "20em")%>%
  column_spec(3, width = "20em")


## ----echo=FALSE---------------------------------------------------------------

data <- matrix(nrow = 3,ncol = 3)

data[1,1:3] <- c("a","b","a + b")

data[2,1:3] <- c("c","d","c + d")

data[3,1:3] <- c("a + c","b + d","10000")

colnames(data) <- c("Test2 reject","Test2 accpet","row sum")

rownames(data) <- c("Test1 reject","Test1 accpet","column sum")

knitr::kable(data)


## ----fig.height=8,fig.width=10,warning=FALSE----------------------------------
library(boot)
library(bootstrap)
library(bootstrap);attach( scor )
pairs( scor, pch=19 )

## -----------------------------------------------------------------------------
cor( scor )

## -----------------------------------------------------------------------------
B = 5000 #no. boostrap resamples
set.seed(76)
exc12.boot=boot( data=cbind(mec,vec),
 statistic=function(x,i){cor(x[i,1],x[i,2])}, R=B ) 
exc12.boot

## -----------------------------------------------------------------------------
mean( exc12.boot$t )

## -----------------------------------------------------------------------------
median( exc12.boot$t )

## -----------------------------------------------------------------------------
set.seed(76)
exc34.boot=boot( data=cbind(alg,ana),
 statistic=function(x,i){cor(x[i,1],x[i,2])}, R=B ) 
exc34.boot

## -----------------------------------------------------------------------------
set.seed(76)
exc35.boot=boot( data=cbind(alg,sta),
 statistic=function(x,i){cor(x[i,1],x[i,2])}, R=B ) 
exc35.boot

## -----------------------------------------------------------------------------
set.seed(76)
exc45.boot=boot( data=cbind(ana,sta),
 statistic=function(x,i){cor(x[i,1],x[i,2])}, R=B ) 
exc45.boot

## -----------------------------------------------------------------------------
skewness <- function(x){
  #compute the sample skewness
  xbar <- mean(x)
  m3 <- mean((x-xbar)^3)
  m2 <- mean((x-xbar)^2)
  return( m3 / m2^1.5 )
}

## -----------------------------------------------------------------------------
library(boot)
set.seed(12345)
mu <- 0  # the skewness of normal populations
n <- 200  # the sample size in bootstrap
m <- 500 # replicate times
boot.skew <- function(x,i) skewness(x[i])
ci.norm <- ci.basic <- ci.perc <- matrix(NA,m,2)
for(i in 1:m){
  X <- rnorm(n,mean = 0,sd = 1)
  de <- boot(data = X, statistic = boot.skew, R = 1e3)
  ci <- boot.ci(de,type = c("norm","basic","perc"))
  ci.norm[i,] <- ci$norm[2:3]
  ci.basic[i,] <- ci$basic[4:5]
  ci.perc[i,] <- ci$percent[4:5]
}
cat('normal distribution missing on left:\n',
    'norm=',mean(ci.norm[,1]<=0),
    'basic=',mean(ci.basic[,1]<=0),
    'perc=',mean(ci.perc[,1]<=0),'\n'
    )

cat('normal distribution missing in right:\n',
    'norm=',mean(ci.norm[,2]>=0),
    'basic=',mean(ci.basic[,2]>=0),
    'perc=',mean(ci.perc[,2]>=0),'\n'
    )
cat('normal distribution cover.probability:\n','norm=',mean(ci.norm[,1]<= mu & ci.norm[,2]>= mu),
    'basic=',mean(ci.basic[,1]<= mu & ci.basic[,2]>= mu),'perc=',mean(ci.perc[,1]<= mu & ci.perc[,2]>= mu))


## -----------------------------------------------------------------------------
library(boot)
set.seed(12345)
mu <- sqrt(8/5)  # the skewness of chi-squared distribution
n <- 200  # the sample size in bootstrap
m <- 500 # replicate times
boot.skew <- function(x,i) skewness(x[i])
ci.norm <- ci.basic <- ci.perc <- matrix(NA,m,2)
for(i in 1:m){
  X <- rchisq(n,df = 5)
  de <- boot(data = X, statistic = boot.skew, R = 1e3)
  ci <- boot.ci(de,type = c("norm","basic","perc"))
  ci.norm[i,] <- ci$norm[2:3]
  ci.basic[i,] <- ci$basic[4:5]
  ci.perc[i,] <- ci$percent[4:5]
}
cat('chi-square distribution missing on left:\n',
    'norm=',mean(ci.norm[,1]<=mu),
    'basic=',mean(ci.basic[,1]<=mu),
    'perc=',mean(ci.perc[,1]<=mu),'\n'
    )

cat('chi-square distribution missing in right:\n',
    'norm=',mean(ci.norm[,2]>=mu),
    'basic=',mean(ci.basic[,2]>=mu),
    'perc=',mean(ci.perc[,2]>=mu),'\n'
    )
cat('chi-square cover.probability:\n','norm=',mean(ci.norm[,1]<= mu & ci.norm[,2]>= mu),
    'basic=',mean(ci.basic[,1]<= mu & ci.basic[,2]>= mu),'perc=',mean(ci.perc[,1]<= mu & ci.perc[,2]>= mu))

## -----------------------------------------------------------------------------
library(boot)
library(bootstrap)
library(bootstrap);attach( scor )
cov( scor) 

## ----warning=FALSE------------------------------------------------------------
require(bootstrap);attach( scor ) 
 theta = function(x){
   eigen(cov(x))$values[1]/sum(eigen(cov(x))$values)
   }   #end function 

## -----------------------------------------------------------------------------
n = length( scor[,1] )
x = as.matrix(scor) 
theta.jack = numeric( n )
theta.hat=theta(scor)
for (i in 1:n) { theta.jack[i] = theta( x[-i,] ) } 
bias.jack = (n-1)*( mean(theta.jack) - theta.hat ) 
theta.bar = mean(theta.jack) 
se.jack = sqrt( (n-1)*mean( (theta.jack-theta.bar)^2 ) ) 
print( list(theta.hat=theta(scor), bias=bias.jack, se=se.jack) ) 
detach(package:bootstrap) 

## -----------------------------------------------------------------------------
library(DAAG); attach(ironslag) 
n = length( magnetic )  
e5 = numeric( n ) 
for (k in 1:n) {
  y = magnetic[-k]    
  x = chemical[-k] 
  J5 = lm(y ~ x + I(x^2) + I(x^3))    
  e5[k] = magnetic[k]-predict( J5, newdata=data.frame(x=chemical[k]) )
  }   #end for loop  mean(e5^2)
 mean(e5^2) 

## -----------------------------------------------------------------------------
library(knitr)
library(kableExtra)
n <- length(magnetic) #in DAAG ironslag 
e1 <- e2 <- e3 <- e4 <- numeric(n)
for (k in 1:n) {
  y <- magnetic[-k] 
  x <- chemical[-k]
  
  J1 <- lm(y ~ x) 
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k] 
  e1[k] <- magnetic[k] - yhat1
  
  J2 <- lm(y ~ x + I(x^2)) 
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] + J2$coef[3] * chemical[k]^2 
  e2[k] <- magnetic[k] - yhat2

  J3 <- lm(log(y) ~ x) 
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k] 
  yhat3 <- exp(logyhat3) 
  e3[k] <- magnetic[k] - yhat3

  J4 <- lm(log(y) ~ log(x)) 
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k]) 
  yhat4 <- exp(logyhat4) 
  e4[k] <- magnetic[k] - yhat4
}
df<-data.frame('model'=c( 'Linear: Y = β0 + β1X + ε',' Quadratic: Y = β0 + β1X + β2X2 + ε' ,' Exponential: log(Y ) = log(β0)+β1X + ε',' Log-Log: log(Y )=β0 + β1 log(X)+ε',' new cubic model :Y = β0 + β1X + β2X2 + β0X3 + ε'),' prediction error '= c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2), mean(e5^2))) 
kable(df) %>%
  kable_styling("striped",full_width = F) %>%
  column_spec(1, bold = T, border_right = T)

## -----------------------------------------------------------------------------
y = magnetic  
x = chemical 
 L1 = lm( y ~ x ) 
 L2 = lm( y ~ x + I(x^2) ) 
 L3 = lm( log(y) ~ x ) 
 L5 = lm( y ~ x + I(x^2) + I(x^3 )) 
dt<-data.frame('model'=c('Linear: Y = β0 + β1X + ε',' Quadratic: Y = β0 + β1X + β2X2 + ε' ,' Exponential: log(Y ) = log(β0)+β1X + ε',' new cubic model :Y = β0 + β1X + β2X2 + β0X3 + ε'),
'R^2 adj'=c(summary(L1)$adj.r.squared, summary(L2)$adj.r.squared,             summary(L3)$adj.r.squared, summary(L5)$adj.r.squared))
kable(dt) %>%
  kable_styling("striped",full_width = F) %>%
  column_spec(1, bold = T, border_right = T)

## -----------------------------------------------------------------------------
L5

## ----warning=FALSE------------------------------------------------------------
library(latticeExtra)
## Dataset
n_1 <- 20 # Defining the sample sizes
n_2 <- 30
x <- rnorm(n_1, 0, 5)
y <- rnorm(n_2, 0, 5)

## function: permutation test for equal variance based
##           on the maximum number of extreme points
count5 <- function(x, y) {
  count5test <- function(x, y) { # Test statistic
    X <- x - mean(x)
    Y <- y - mean(y)
    outx <- sum(X > max(Y)) + sum(X < min(Y))
    outy <- sum(Y > max(X)) + sum(Y < min(X))
    # Return 1 (reject) or 0 (do not reject H_{0})
    return( as.integer( max(c(outx, outy) ) > 5) )
  }
  r <- 10000 # Permutation samples
  z <- c(x, y)
  n <- length(z)
  reps <- vector("numeric", r)
  t0 <- count5test(x, y)
  for (i in 1:r){ # Permutation test
    k = sample(n, size = length(x), replace = FALSE)
    reps[i] = count5test(z[k], z[-k])
  }
  p <-
    ifelse(t0 == 0, mean( c(t0, reps) > t0 ), mean( c(t0, reps) >= t0 ))
  return(
    histogram(c(t0, reps) # Histogram
              , type = "density"
              , col = "#0080ff"
              , xlab = "Replicates of the Count 5 test"
              , ylab = list(rot = 0)
              , main = "Permutation distribution of the Count 5 test"
              , sub = list(substitute(paste(hat(p), " = ",pvalue)
                                      , list(pvalue = p))
                           , col = 2)
              , panel = function(...){
                panel.histogram(...)
                panel.abline(v = t0, col = 2, lwd = 2)
              })
  )
}
count5(x, y)

## -----------------------------------------------------------------------------
f <- function( x ) { -.5*exp(-abs(x)) } 
set.seed(123) 
 rw.MetropolisL <- function(sigma, x0, m) {         
   x <- numeric(m)         
   x[1] <- x0         
   u <- runif(m)         
   k <- 0         
   for (i in 2:m) {            
     y <- rnorm(1, x[i-1], sigma)                 
     if (u[i] <= (f(y) / f(x[i-1])))                 
       x[i] <- y  
     else {                     
       x[i] <- x[i-1]                     
       k <- k + 1                 }                        
     }                           
     #end for loop         
     return(list(x=x, k=k))}
   #end function 

## ----warning=FALSE------------------------------------------------------------
library(kableExtra)
library(knitr)
 m <- 2000     
sigma <- c(.05, .5, 2,  16)     
x0 <- 25     
rw1 <- rw.MetropolisL(sigma[1], x0, m)     
rw2 <- rw.MetropolisL(sigma[2], x0, m)     
rw3 <- rw.MetropolisL(sigma[3], x0, m)     
rw4 <- rw.MetropolisL(sigma[4], x0, m)    

#number of candidate points rejected
    no.reject <- data.frame(sigma=sigma,no.reject=c(rw1$k, rw2$k, rw3$k, rw4$k),accept = 1-c(rw1$k/m, rw2$k/m, rw3$k/m, rw4$k/m))
    kable(no.reject) %>%
  kable_styling("striped", full_width = F)

## ----fig.height=8,fig.width=12------------------------------------------------
par(mfrow=c(2,2))                #display 4 graphs together     
rw <- cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
refline <- qt(c(.025, .975), df=4)
for (j in 1:4) { 
  plot(rw[,j], type="l", xlab=bquote(sigma == .(round(sigma[j],3))),              ylab="X", ylim=range(rw[,j]),col='blue')
    abline(h=refline,col='red')
  }#end for loop 
   

## -----------------------------------------------------------------------------
x = seq(0.5, 5, 0.5) 
cbind(x, log(exp(x))-exp(log(x)), log(exp(x))==exp(log(x)) ) 

## -----------------------------------------------------------------------------
identical(log(exp(x)),exp(log(x))) 

## -----------------------------------------------------------------------------
all.equal(log(exp(x)),exp(log(x))) 

## -----------------------------------------------------------------------------
f<-function(a,k){
  g<-2*exp(lgamma((k+1)/2)-lgamma((k)/2))/sqrt(pi*k)
  ck<-sqrt((k*a^2)/(k+1-a^2))
  return(g*integrate(function(u){(1+u^2/k)^(-(k+1)/2)},lower = 0,upper = ck,rel.tol=.Machine$double.eps^0.25)$value)
}
g<-function(a,k){
  return(f(a,k)-f(a,k+1))
}

## -----------------------------------------------------------------------------
par(mfrow=c(2,3))
for (k in c(4:25,500,1000)) {
  plot(seq(0,sqrt(k+1),length.out = 100),rep(0,100),type = 'l',ylim = c(-2.5e-3,2.5e-3),main = c('k=',k))
  for (a in seq(0,sqrt(k+1),by=0.01)) {
  points(a,g(a,k),pch=1,cex=0.1,col='blue')
}
}

## -----------------------------------------------------------------------------
ks<-c(4:25,500,1000)
cat(c('k','root','\n'))
for(i in 1:length(ks)){
   cat( c( ks[i], uniroot(g, lower=0.0001, upper=2, k=ks[i])$root,'\n' ) ) 
}

## -----------------------------------------------------------------------------
k = c( 4:25, 500, 1000 ) 
cat(c('k','root','\n'))
 object = function( a, df ){             
   a2 = a^2                
   arg = sqrt( a2*df/(df + 1 - a2) )                
   Sk = pt( q=arg, df=df, lower=F) 
   arg = sqrt( a2*(df-1)/(df - a2) )                
   Skm1 = pt( q=arg, df=df-1, lower=F) 
   return( Sk-Skm1 )                        
   }  
 for ( i in 1:length(k) )  {   
   cat( c( k[i], uniroot(object, lower=1, upper=2, df=k[i])$root ,'\n') )                           } 

## -----------------------------------------------------------------------------
cat(c('k','difference','\n'))
 for ( i in 1:length(k) )  {   
   cat( c( k[i], uniroot(object, lower=1, upper=2, df=k[i])$root-uniroot(g, lower=0.0001, upper=2, k=ks[i])$root,'\n') ) }

## -----------------------------------------------------------------------------
library(kableExtra)
library(knitr)
df<-data.frame('Phenotype'=c('O','A','B','AB'),'Probability'=c('r^2','p^2+2pr','q^2+2qr','2pq'))
kable(df) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T,width = '10em') %>%
  column_spec(2, width = "10em",bold = T)

## -----------------------------------------------------------------------------
lnL <- function(p, q, nA = 28, nB = 24, nAB = 70, nO = 41) {

   r = 1.0 - p - q

   nA * log(p^2 + 2*p*r) + nB * log(q^2 + 2 * q * r) + 
        nAB * log(2 * p * q) + 2 * nO * log(r)

}

## -----------------------------------------------------------------------------
EM <- function (p, q, nA = 28, nB = 24, nAB = 70, nO = 41, debug = TRUE) {

    # Evaluate the likelihood using initial estimates
    llk <- lnL(p, q, nA, nB, nAB, nO)

    # Count the number of iterations so far
    iter <- 1

    # Loop until convergence ...
    while (TRUE)
       {
       # Estimate the frequency for allele O
       r = 1.0 - p - q
 
       # First we carry out the E-step

       # The counts for genotypes O/O and A/B are effectively observed
       # Estimate the counts for the other genotypes
       nAA <- nA * p / (p + 2*r)
       nAO <- nA - nAA
       nBB <- nB * q / (q + 2*r)
       nBO <- nB - nBB
       
       # Print debugging information
       if (debug)
          {
          cat("Round #", iter, "lnLikelihood = ", llk, "\n")
          cat("    Allele frequencies: p = ", p, ", q = ", q, ", r = ", r, "\n")
          cat("    Genotype counts:    nAA = ", nAA, ", nAO = ", nAO, ", nBB = ", nBB, 
              ", nBO = ", nBO, "\n")
          }
       # Then the M-step
       p <- (2 * nAA + nAO + nAB) / (2 * (nA + nB + nO + nAB))
       q <- (2 * nBB + nBO + nAB) / (2 * (nA + nB + nO + nAB))

       # Then check for convergence ...
       llk1 <- lnL(p, q, nA, nB, nAB, nO)

       if (abs(llk1 - llk) < (abs(llk) + abs(llk1)) * 1e-6) break       

       # Otherwise keep going
       llk <- llk1
       iter <- iter + 1
       }

  list(p = p, q = q)
}
EM(0.4,0.4)

## -----------------------------------------------------------------------------
library(ggplot2)
df<-data.frame('x'<-c(1,2,3,4),'lnliklihood'<-c( -270.9849 ,-252.5037,-251.9282,-251.9148))
ggplot(data = df,aes(x=x,y=lnliklihood))+geom_point(aes(y=lnliklihood),col='blue')+geom_line(linetype = 3,aes(y = lnliklihood),size = 0.8) +
  geom_point(aes(y = lnliklihood),size = 3,col='blue') 

## -----------------------------------------------------------------------------
L <- deriv3(expression(
      nA * log(p^2 + 2*p*(1 - p - q)) + nB * log(q^2 + 2 * q * (1 - p - q)) +
      nAB * log(2 * p * q) + 2 * nO * log(1 - p - q)), c("p", "q"),
      function (p, q, nA = 28, nB = 24, nAB = 70, nO = 41) { } )

## -----------------------------------------------------------------------------
Newton <- function(p, q, nA = 28, nB = 24, nAB = 70, nO = 41, debug = TRUE) {

   # Calculate likelihood, score and information
   pointEstimates <- L(p, q, nA, nB, nAB, nO)

   iter <- 1

   while (TRUE) {

       llk <- pointEstimates[1]
       S <- attr(pointEstimates, "gradient") [1,]
       I <- -attr(pointEstimates, "hessian") [1,,]

       # Print debugging information
       if (debug)
          {
          cat("Round #", iter, "lnLikelihood = ", llk, "\n")
          cat("    Allele frequencies: p = ", p, ", q = ", q, ", r = ", 1-p-q, "\n")
          cat("    Score: ", S, "\n")
          }

       # Update point
       delta <- solve(I, S)

       p <- p + delta[1]
       q <- q + delta[2]

       # Calculate likelihood, score and information
       pointEstimates <- L(p, q, nA, nB, nAB, nO)
       llk1 <- pointEstimates[1]

       # Check for convergence 
       if (abs(llk1 - llk) < (abs(llk) + abs(llk1)) * 1e-6) break

       # Keep going
       iter <- iter + 1
       }

    list(p = p, q = q)
}
Newton(0.4,0.4)

## -----------------------------------------------------------------------------
formulas <-list(mpg ~ disp,mpg ~I(1/ disp),mpg ~ disp + wt,mpg ~I(1/ disp) + wt)

## -----------------------------------------------------------------------------
res<-list()
for(i in 1:length(formulas)){
  res[[i]]<-lm(formulas[[i]], data=mtcars)
  }
res

## -----------------------------------------------------------------------------
lapply(formulas, FUN=function(x) lm(formula=x, data=mtcars))

## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

## -----------------------------------------------------------------------------
res2<-list()
for(i in 1:length(bootstraps)){
  res2[[i]]<-lm(formula=mpg ~ disp, data=bootstraps[[i]])
}
res2

## -----------------------------------------------------------------------------
lapply(bootstraps, FUN=function(x) lm(mpg~disp, data=x))

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared

## -----------------------------------------------------------------------------
lapply(formulas, FUN=function(x) rsq(lm(formula=x, data=mtcars)))

## -----------------------------------------------------------------------------
lapply(bootstraps, FUN=function(x) rsq(lm(mpg~disp, data=x)))

## -----------------------------------------------------------------------------
trials <- replicate(
  100, 
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)

## -----------------------------------------------------------------------------
sapply(trials,function(mod){mod[["p.value"]]},simplify=TRUE)

## -----------------------------------------------------------------------------
sapply(trials, function(x) "[["(x,'p.value'))

## ----mcsapply-----------------------------------------------------------------
library(parallel)
library(parallelsugar) ## to fit in the Windows system
mcsapply <- function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) 
  {
  FUN <- match.fun(FUN)
  answer <- mclapply(X = X, FUN = FUN, mc.cores = 2, ...)
  if (USE.NAMES && is.character(X) && is.null(names(answer))) 
    names(answer) <- X
  if (!identical(simplify, FALSE) && length(answer)) 
    simplify2array(answer, higher = (simplify == "array"))
  else answer
}

## -----------------------------------------------------------------------------
mcvapply <- function(x, f, FUN.VALUE, ...) {
    out_list <- mclapply(f, ...)
    out <- matrix(rep(FUN.VALUE, length(x)), nrow = length(x))
    for (i in seq_along(x)) {
        stopifnot(
            length(res) == length(f.value),
            typeof(res) == typeof(f.value))
        out[i, ] <- out_list[[i]]
    }
    out
}

## -----------------------------------------------------------------------------
library(Rcpp) # Attach R package "Rcpp"
    # Define function "rw_MetropolisL"
cppFunction('List rw_MetropolisL(double sigma, int x0, int m){
            NumericVector x(m);
            NumericVector u(m);
            as<DoubleVector>(x)[0] = x0;
            u = as<DoubleVector>(runif(m));
            int k = 0;
            int i;
            for(i=1;i<m;++i){
               double y = as<double>(rnorm(1,x[i-1],sigma));
               if(u[i] <= exp(-fabs(y)+abs(x[i-1]))){
               x[i] = y;
               }
               else{
               x[i] = x[i-1];
               k = k+1;
               }
            }
            List L=List::create(Named("x")=x, Named("k")=k);
            return L;       
            }
            ')

## -----------------------------------------------------------------------------
f <- function( x ) { -.5*exp(-abs(x)) } 
set.seed(123) 
 rw.MetropolisL <- function(sigma, x0, m) {         
   x <- numeric(m)         
   x[1] <- x0         
   u <- runif(m)         
   k <- 0         
   for (i in 2:m) {            
     y <- rnorm(1, x[i-1], sigma)                 
     if (u[i] <= (f(y) / f(x[i-1])))                 
       x[i] <- y  
     else {                     
       x[i] <- x[i-1]                     
       k <- k + 1                 }                        
     }                           
     #end for loop         
     return(list(x=x, k=k))}
   #end function 

## -----------------------------------------------------------------------------
library(microbenchmark) 
N <- 2000
sigma <- c(0.05,0.5,2,4,8,16)
x0 <- 25
for(i in 1:length(sigma)){
  assign(paste0("rw",i),rw.MetropolisL(sigma[i],x0,N))
  assign(paste0('rw_',i),rw_MetropolisL(sigma[i],x0,N))
  assign(paste0('ts',i) ,microbenchmark(meanR=rw.MetropolisL(sigma[i],x0,N),
                                         meancpp=rw_MetropolisL(sigma[i],x0,N))) 
}

## -----------------------------------------------------------------------------
for(i in 1:length(sigma)){

  qqplot(get(paste0("rw",i))$x,

         get(paste0("rw_",i))$x,

         xlab = "from R",ylab = "from Rcpp",

         main = bquote(sigma == .(sigma[i])))

  f <- function(x) x

  curve(f, col = 'red',add = TRUE)

}

## -----------------------------------------------------------------------------
summary(ts1)[,c(1,3,5,6)]
summary(ts2)[,c(1,3,5,6)]
summary(ts3)[,c(1,3,5,6)]
summary(ts4)[,c(1,3,5,6)]
summary(ts5)[,c(1,3,5,6)]
summary(ts6)[,c(1,3,5,6)]

