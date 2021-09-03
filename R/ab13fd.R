ab13fd <- function(n, a, tol) {

    # In Parameters
    n <- as.integer(n)
    tol <- as.double(tol)

    # Out Parameters
    beta <- as.double(0)
    omega <- as.double(0)
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    dwork <- array(as.double(1), c(ldwork))
    ldwork <- max(1, 3 * n * (n + 2))
    lcwork <- max(1, n * (n + 3))

    res <- .Fortran("AB13FD", N = n, A = a, LDA = lda, BETA = beta, OMEGA = omega, TOL = tol, DWORK = dwork, LDWORK = ldwork, LCWORK = lcwork, INFO = info)

    return(list(beta = res$BETA, omega = res$OMEGA, info = res$INFO))
}
