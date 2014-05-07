
####################
genmvnorm<-function(cor,k,n,seed=F){
if(require("psych")!=T){
  print("installing 'psych' package")
  install.packages("psych")
  require(psych)}
if(seed != F){set.seed(seed)}
if(is.matrix(cor)==F){
x<-length(cor)
if(x != (k*(k-1)/2) ){stop("STOP: wrong correlation table")}
cr.cor<-matrix(NA,k,k)
diag(cr.cor)<-1
cr.cor[lower.tri(cr.cor)]<-cor
cr.cor[upper.tri(cr.cor)]<-t(cr.cor)[upper.tri(cr.cor)]
fit<-principal(cr.cor,rotate=F,nfactors=k)}

if(is.matrix(cor)==T){ #if a correlation matrix was used
  fit<-principal(cor,rotate=F,nfactors=k)
}

l<-fit$loadings[1:k,1:k]
f<-matrix(nrow=k,l)
dim(f)
ma<-matrix(nrow=n,ncol=k)
for (i in 1:k){
  ma[,i]<-rnorm(n)
}
tma<-t(ma)
sol<-f %*% tma
sol<-t(sol)

data<-data.frame(sol)
return(data)
}