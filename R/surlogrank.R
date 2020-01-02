#' @title  function that perform the log-rank test using the large-sample approximation and the permutation test
#' @description use four statistics:Mantel-Haenszel Logrank Test;Cox-Mantel Logrank Test;Generalized Wilcoxon;Tarone and Ware
#' @param time time variable,a numeric vector
#' @param status status variable,only conclude 0 and 1 ,1：fail or 0：at risk
#' @param group group variable,only divided to 2 groups of patients with acute myelogenous leukaemia
#' @param permutation TRUE or FLASE ,use permutation or not
#' @return an object that contains the test statistic, the p-value, and the method
#' @importFrom stats pchisq
#' @import survival
#' @import boot
#' @export
surlogrank <- function(time,status,group,permutation){
  err=paraerr(time, status, group)
  if(err==1){
    return("wrong input")
  }
  testgroup=data.frame(time,status,group)
  label=names(table(testgroup$group))                 
  testgroup2=testgroup[testgroup$group==label[2],]    
  testgroup1=testgroup[testgroup$group==label[1],]    
  J2=length(table(testgroup2$time))                   
  testgoup_status1=testgroup[testgroup$status==1,]    
  time=as.numeric(names(table(testgoup_status1$time)))
  J=length(time)                                      
  O=vector(mode="numeric",length=J)
  O2=vector(mode="numeric",length=J2)
  time_group2=as.numeric(names(table(testgroup2$time)))
  for(i in 1:J2){
    O2[i]=sum(testgroup2[testgroup2$time==time_group2[i],]$status)
  }
  for(i in 1:J2){
    O[which(time==time_group2[i])]=O2[i]
  }
  d=vector(length=J)
  for(i in 1:J){
    d[i]=sum(testgroup[testgroup$time==time[i],]$status)
  }
  population_2=nrow(testgroup2)
  Y2=vector(length=J)
  Y2[1]=population_2
  for(i in 2:J){
    Y2[i]=population_2-nrow(testgroup2[testgroup2$time<time[i],])
  }
  population=nrow(testgroup)
  Y=vector(length=J)
  for(i in 1:J){
    Y[i]=population-nrow(testgroup[testgroup$time<time[i],])
  }
  Y1=vector(length=J)
  Y1=Y-Y2
  E=vector(length=J)
  for(j in 1:J){
    E[j]=d[j]*Y2[j]/Y[j]
  }
  V=vector(length=J)
  for(j in 1:J)
    V[j]=(Y1[j]*Y2[j]*d[j]*(Y[j]-d[j]))/((Y[j])^2*(Y[j]-1))
  Z=sum(O-E)/sqrt(sum(V))
  Z1=Z^2
  p_value1=1-pchisq(q=Z1,df=1)
  E1=vector(length=J)
  for(j in 1:J){
    E1[j]=d[j]*Y1[j]/Y[j]
  }
  d2=O
  d1=d-d2
  E2=vector(length=J)
  for(j in 1:J){
    E2[j]=d[j]*Y2[j]/Y[j]
  }
  Z2=(sum(d1-E1))^2/(sum(E1))+(sum(d2-E2))^2/(sum(E2))
  p_value2=1-pchisq(q=Z2,df=1)
  Z3=(t(Y) %*%(O-E))^2/((t(Y)^2)%*%V)
  p_value3=1-pchisq(q=Z3,df=1)
  Z4=(t(sqrt(Y)) %*%(O-E))^2/(t(Y)%*%V)
  p_value4=1-pchisq(q=Z4,df=1)
  n1=nrow(testgroup1)     
  n2=nrow(testgroup2)       
  S_obs=mean(testgroup2$status)-mean(testgroup1$status)
  S_obs=abs(S_obs)
  nsims=1000       
  sim_re=matrix(0,nrow=nsims,ncol=4)    
  for(i in 1:nsims){
    tmp=sample(c(rep(1,n1),rep(2,n2)))
    sim_re[i,]=as.numeric(permut(testgroup$time, testgroup$status, tmp))
  }
  p_value5=mean(Z1<abs(sim_re[,1]))
  p_value6=mean(Z2<abs(sim_re[,2]))
  p_value7=mean(as.numeric(Z3)<abs(sim_re[,3]))
  p_value8=mean(as.numeric(Z4)<abs(sim_re[,4]))
  logrankoutput=structure(list(Z1,p_value1,Z2,p_value2,Z3,p_value3,Z4,p_value4,p_value5,p_value6,p_value7,p_value8,permutation),class="logrank")
  return(logrankoutput)
}

