sb04qd <- function(n, m, a, b, c, ldwork) {

    # In Parameters
    m <- as.integer(m)
    n <- as.integer(n)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    z <- array(as.double(0), c(m, m))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldz <- dim(z)[1]
    iwork <- array(as.integer(1), c(4 * n))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("    FORTRANNAME SB04QD", N = n, M = m, A = a, B = b, C = c, LDWORK = ldwork, Z = z, INFO = info, LDA = lda, LDB = ldb, LDC = ldc, LDZ = ldz, IWORK = iwork, DWORK = dwork)

    return(list(a = res$A, b = res$B, c = res$C, z = res$Z, info = res$INFO))
}
