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
    lda <- max(dim(a)[1], 1)
    ldb <- max(dim(b)[1], 1)
    ldc <- max(dim(c)[1], 1)
    ldd <- max(dim(d)[1], 1)
    lde <- max(dim(e)[1], 1)
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("SB10JD", N = n, M = m, NP = np, A = a, B = b, C = c, D = d, E = e, LDWORK = ldwork, NSYS = nsys, INFO = info, LDA = lda, LDB = ldb, LDC = ldc, LDD = ldd, LDE = lde, DWORK = dwork)

    return(list(a = res$A, b = res$B, c = res$C, d = res$D, nsys = res$NSYS, info = res$INFO))
}
