mb03vy <- function(n, ilo, ihi, a, tau, ldwork) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    n <- as.integer(n)
    ihi <- as.integer(ihi)
    ilo <- as.integer(ilo)

    # Out Parameters
    info <- as.integer(0)

    # Hidden Parameters
    p <- dim(a)[3]
    lda1 <- dim(a)[1]
    lda2 <- dim(a)[2]
    ldtau <- dim(tau)[1]
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("MB03VY", N = n, ILO = ilo, IHI = ihi, A = a, TAU = tau, LDWORK = ldwork, INFO = info, P = p, LDA1 = lda1, LDA2 = lda2, LDTAU = ldtau, DWORK = dwork)

    return(list(a = res$A, info = res$INFO))
}
