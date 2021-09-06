mb03vd <- function(n, ilo, ihi, a) {

    # In Parameters
    n <- as.integer(n)
    ihi <- as.integer(ihi)
    ilo <- as.integer(ilo)

    # Out Parameters
    tau <- array(as.double(0), c(max(1, n - 1), p))
    info <- as.integer(0)

    # Hidden Parameters
    p <- dim(a)[3]
    lda1 <- dim(a)[1]
    lda2 <- dim(a)[2]
    ldtau <- dim(tau)[1]
    dwork <- array(as.double(1), c(n))

    res <- .Fortran("MB03VD", N = n, ILO = ilo, IHI = ihi, A = a, TAU = tau, INFO = info, P = p, LDA1 = lda1, LDA2 = lda2, LDTAU = ldtau, DWORK = dwork)

    return(list(a = res$A, tau = res$TAU, info = res$INFO))
}
