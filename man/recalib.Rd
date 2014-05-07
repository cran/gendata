\name{recalib}
\alias{recalib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recalibrate (rescale) variables
}
\description{
Rescale variables (one at a time) to have a new minimum and maximum value.
}
\usage{
recalib(data, var, low, high)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{the dataset to use.
}
  \item{var}{indicate the variable number (or variable name).}
  \item{low}{
Indicate the new minimum value.
}
  \item{high}{
Indicate the new maximum value.
}
}
\details{
Specify the rescaling of variables one at a time.
}


\author{
Francis Huang
}


\seealso{

\link[gendata:genmvnorm]{genmvnorm}
\link[gendata:revcode]{revcode}
\link[gendata:dtrans]{dtrans}
}
\examples{
sdata<-genmvnorm(cor=c(.7,.2,.3),k=3,n=500,seed=12345)
cor(sdata)
summary(sdata[,1])
#note the min and max of variable X1
#changes variable one to have a minimum of 10 and a maximum of 50
#correlations remain the same

s2<-recalib(sdata,1,10,50)
cor(s2)
summary(s2[,1])
#note revised values of variable X1

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
