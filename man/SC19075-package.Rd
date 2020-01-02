\name{SC19075-package}
\alias{SC19075-package}
\alias{SC19075}
\docType{package}
\title{
  Survival Analysis using logrank test permutation test
}
\description{
  The aml (boot) data contains estimates of the times to remission for two groups of patients with acute myelogenous leukaemia (AML). One group received maintenance chemotherapy treament and the other group did not. This R package perform the log-rank test using the large-sample approximation and the permutation test. The log-rank test is used to test whether the survival distributions of the two groups are equal. we use a function to test whether the survival distributions of the two groups are equal in the aml (boot) data.The results showed that both the logrank test and the Permutation test could not reject the null hypothesis at the 5 percrnt significance level using the aml dataset (n 50), and the logrank test and the Permutation test both rejected the null hypothesis significantly at the 5 percent significance level.
}
\details{
  use the function surlogrank() on the data 
}
\author{
YuLiang Jiang

Maintainer: YuLiang Jiang <ustc666jyl@mail.ustc.edu.cn>
}
\references{
  [1].Richard Peto, Julian Peto.Asymptotically Efficient Rank Invariant Test Produres.In:Journal of Rayal Statistical Society.Series A(General),Vol.135,NO.2(1972),pp.185-207
  
[2].E. L. Kaplan and Paul Meier.Nonparametric Estimation from Incomplete Observations.Journal of the American Statistical Association, Vol. 53, No.282 (Jun., 1958), pp.457-481

[3].Maria L. Rizzo.Statistical Computing with R.Chapman & Hall/CRC:2008

[4].Hadley Wicjham,Garrett GroleMund.R：2018
}
\keyword{ Logrank   Survival Analysis  Permutation test }

