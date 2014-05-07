\name{gendata-package}
\alias{gendata-package}
\alias{gendata}
\docType{package}
\title{
Generate synthetic datasets
}
\description{
Create synthetic datasets based on a correlation table. Additional functions can be used to rescale, transform, and reverse code variables.
}
\details{
\tabular{ll}{
Package: \tab gendata\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-05-07\cr
License: \tab GPL-3\cr
}

Additional functions are for modifying the dataset. \cr

genmvnorm: 
 creates the dataset (generates a multivariate normal dataset). \cr
recalib : for rescaling the dataset \cr
dtrans  : for giving a variable a new mean and standard deviation \cr
revcode : for reverse coding a variable
}
\author{
Francis Huang

Maintainer: Francis Huang <flh3@hotmail.com>

}
\references{

Fan, X., Sivo, S., & Keenan, S. (2002). SAS for Monte Carlo studies: A guide for quantitative researchers. Sas Institute.
}

\keyword{ package }
\seealso{
\link[gendata:genmvnorm]{genmvnorm}
\link[gendata:revcode]{revcode}
\link[gendata:dtrans]{dtrans}
\link[gendata:recalib]{recalib}


}

