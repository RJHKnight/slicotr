ab07nd <- function(n, m, a, b, c, d, ldwork) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)

    # Out Parameters
    rcond <- as.double(0)
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    iwork <- array(as.integer(1), c(2 * m))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AB07ND", N = n, M = m, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd, RCOND = rcond, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, INFO = info)

    return(list(a = res$A, b = res$B, c = res$C, d = res$D, rcond = res$RCOND, info = res$INFO))
}
