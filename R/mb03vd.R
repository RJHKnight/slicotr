# mb03vd <- function(n, ilo, ihi, a)
# {
#   if (!(length(dim(a)) ==3))
#   {
#     stop("a must be a 3d square matrix.")
#   }
#
#   if (!n>= 0)
#   {
#     stop("n must be >= 0")
#   }
#
#   p <- dim(a)[3]
#   lda1 <- dim(a)[1]
#   lda2 <- dim(a)[2]
#   tau <- matrix(as.double(1), ncol = p, nrow = max(1, n-1))
#   ldtau <- as.integer(max(1, n-1))
#   dwork <- rep(double(0), n)
#   info <- integer(1)
#
#   # N, P, ILO, IHI, A, LDA1, LDA2, TAU, LDTAU, DWORK, INFO
#   res <- .Fortran("MB03VD", N = as.integer(n), P = p, ILO = as.integer(ilo), IHI = as.integer(ihi),
#                   A = a, LDA1 = lda1, LDA2 = lda2, TAU = tau, LDTAU = ldtau,
#                   DWORK = dwork, INFO = info)
#
#   return (list(tau = res$TAU, a = res$A, info = res$INFO))
# }
