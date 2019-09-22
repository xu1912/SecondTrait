library(mvtnorm)

##Covariance matrix between X and Y
Sigma=matrix(c(1,0.9,
                0.9,1),nrow=2)

n=500     ##sample size
z=rnorm(n)    ##Y=Z*beta, Z is observed variables, ie. Age, Gender, SNP. Here, assumed to be simple normal distributed. We can change it later.
beta=0    ##Effect size/Regression coefficients. For type 1 error evaluation, beta=0.
y_true=z*beta+1
zx=rnorm(n)   ##X=ZX*betax, simile to the settings of Y.
betax=1
x_true=zx*betax+5

ks=5000   ##Number of simulations.
pwr=0
for(k in 1:ks){
	dm=matrix(0,nrow=n,ncol=2)
	for(i in 1:n){
		dm[i,]=rmvnorm(1, mean=c(x_true[i],y_true[i]),sigma=Sigma)
	}
	dd=data.frame(dm,z,zx)
	colnames(dd)=c("X","Y","Z","XZ")
	d_sort=dd[order(dd$X),]
	d_trun=d_sort[c(1:50,451:500),]       ##Truncated by high 50 and low 50 of sorted X
	pwr=pwr+sum(summary(lm(Y~Z, data=d_trun))$coefficients[2,4]<0.05)
}
pwr

## Some plot to see the distribution
d_sort=d[order(d[,1]),]
d_trun=d_sort[c(1:50,451:500),]
hist(d[,1])
hist(d[,2])
hist(d_trun[,1],breaks=10)
hist(d_trun[,2],breaks=10)
