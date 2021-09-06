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
    ldwork <- as.integer(max(1, 3 * n * (n + 2)))
    lcwork <- as.integer(max(1, n * (n + 3)))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AB13FD", N = n, A = a, TOL = tol, BETA = beta, OMEGA = omega, INFO = info, LDA = lda, LDWORK = ldwork, LCWORK = lcwork, DWORK = dwork)

    return(list(beta = res$BETA, omega = res$OMEGA, info = res$INFO))
}
