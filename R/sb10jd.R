sb10jd <- function(n, m, np, a, b, c, d, e, ldwork) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    np <- as.integer(np)

    # Out Parameters
    nsys <- as.integer(0)
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(shape)[NA]
    ldb <- dim(shape)[NA]
    ldc <- dim(shape)[NA]
    ldd <- dim(shape)[NA]
    lde <- dim(shape)[NA]
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("SB10JD", N = n, M = m, NP = np, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd, E = e, LDE = lde, NSYS = nsys, DWORK = dwork, LDWORK = ldwork, INFO = info)

    return(list(a = res$A, b = res$B, c = res$C, d = res$D, nsys = res$NSYS, info = res$INFO))
}
