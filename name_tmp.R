mb03vd<- function(n,p,ilo,a,tau,info)
{n <- as.integer(n)
ilo <- as.integer(ilo)p <- dim(a)[3]tau <- array(as.integer(0), c(max(1,n-1),p))
info <- as.integer(0)res <- .Fortran(MB03VD,N=n,P=p,ILO=ilo,A=a,TAU=tau,INFO=info)return (list(a = res$A,tau = res$TAU,info = res$INFO))