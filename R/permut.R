#' @title  function that perform the log-rank test using the large-sample approximation
#' @description use four statistics:Mantel-Haenszel Logrank Test;Cox-Mantel Logrank Test;Generalized Wilcoxon;Tarone and Ware
#' @param time time variable,a numeric vector
#' @param status status variable,only conclude 0 and 1 ,suggest live or dead
#' @param group group variable,only divided to 2 groups of patients with acute myelogenous leukaemia
#' @return an object that contains the test statistic, the p-value, and the method
#' @export
permut=function(time,status,group){
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
  Z3=(t(Y) %*%(O-E))^2/((t(Y)^2)%*%V)
  Z4=(t(sqrt(Y)) %*%(O-E))^2/(t(Y)%*%V)
  return(list(Z1, Z2, as.numeric(Z3),as.numeric(Z4)))
}

