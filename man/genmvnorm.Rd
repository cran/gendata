\name{genmvnorm}
\alias{genmvnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Genmvnorm
}
\description{
Generates a multivariate normal dataset based on a specified correlation matrix. 
}
\usage{
genmvnorm(cor, k, n, seed = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cor}{Can be a correlation matrix-- e.g., data<-cor(xyz)-- or the lower half
  of a correlation matrix, e.g., for a 3 variable dataset, data<-c(.7,.3,.2)-- useful for creating datasets without having to specify both halves of the correlation matrix.

}
  \item{k}{
Indicate the number of variables in your dataset.
}
  \item{n}{
Indicate the number of observations in your new synthetic dataset.
}
  \item{seed}{
For reproducability of results, set a specific seed number.
}
}
\details{
For creating synthetic datasets. Based on the SAS chapter by Fan et al. (2002).
}

\references{Based on:

Fan, X., Felsovalyi, A., Sivo, S., & Keenan, S. (2002). SAS for Monte Carlo studies: A guide for quantitative researchers. SAS Institute.
}
\author{
Francis Huang
}


\seealso{
\link[gendata:revcode]{revcode}
\link[gendata:dtrans]{dtrans}
\link[gendata:recalib]{recalib}

}
\examples{
sdata<-genmvnorm(cor=c(.7,.2,.3),k=3,n=500,seed=12345)
cor(sdata)
#dataset above uses the lower half of a correlation table
#     1  .7  .2
#     .7  1  .3
#     .2 .3   1
# Can also use a correlation table

data(iris)
dat<-cor(iris[,1:3])
dat
sdata<-genmvnorm(cor=dat,k=3,n=100,seed=123)
cor(sdata)

#example above uses the IRIS dataset.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ montecarlo }
\keyword{ synthetic }% __ONLY ONE__ keyword per line
