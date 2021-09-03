ab13ed <- function(n, a, tol) {

    # In Parameters
    n <- as.integer(n)
    tol <- as.double(tol)

    # Out Parameters
    low <- as.double(0)
    high <- as.double(0)
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    dwork <- array(as.double(1), c(ldwork))
    ldwork <- max(1, 3 * n * (n + 1))

    res <- .Fortran("AB13ED", N = n, A = a, LDA = lda, LOW = low, HIGH = high, TOL = tol, DWORK = dwork, LDWORK = ldwork, INFO = info)

    return(list(low = res$LOW, high = res$HIGH, info = res$INFO))
}
