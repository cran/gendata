\name{dtrans}
\alias{dtrans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Transform
}
\description{
Transforms variables in a dataset with a specified mean and standard deviation.
}
\usage{
dtrans(data, m, sd, rnd = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
name of your dataset.
}
  \item{m}{
indicate a vector of desired means.
}
  \item{sd}{
indicate a vector of desired standard deviations.
}
  \item{rnd}{
indicates if you want to round the numbers (no decimals). \code{TRUE} or \code{FALSE}.
}
}
\author{
Francis Huang
}

\examples{

sdata <- genmvnorm(cor = c(.7, .2, .3), k = 3, n = 500, seed = 12345)
cor(sdata)
summary(sdata)
#note: data are in z scores

s2 <- dtrans(sdata, c(0, 100, 50), c(1, 15, 10), rnd = FALSE)
summary(s2)
sd(s2[,2])
sd(s2[,3])
#note: variables X2 and X3 are now rescaled with the appropriate means and standard deviations.
head(s2)

s2 <- dtrans(sdata, c(0, 100, 50), c(1, 15, 10), rnd = TRUE)
#at times, you may want a dataset to not have decimals. use \code{rnd= TRUE}.
head(s2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{transform}
