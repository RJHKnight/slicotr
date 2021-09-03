mb05nd <- function(n, delta, a, tol) {

    # In Parameters
    delta <- as.double(delta)
    n <- as.integer(n)
    tol <- as.double(tol)

    # Out Parameters
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldex <- dim(ex)[1]
    ldexin <- dim(exint)[1]
    iwork <- array(as.integer(1), c(n))
    dwork <- array(as.double(1), c(max(2 * n * n, 1)))
    ldwork <- dim(dwork)[1]

    res <- .Fortran("MB05ND", N = n, DELTA = delta, A = a, LDA = lda, EX = ex, LDEX = ldex, EXINT = exint, LDEXIN = ldexin, TOL = tol, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, INFO = info)

    return(list(ex = res$EX, exint = res$EXINT, info = res$INFO))
}
