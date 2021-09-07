tg01fd_ii <- function(joba, l, n, m, p, a, e, b, c, tol, ldwork) {

    # In Parameters
    joba <- as.character(joba)
    l <- as.integer(l)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)

    # Out Parameters
    q <- array(as.double(0), c(l, l))
    z <- array(as.double(0), c(n, n))
    ranke <- as.integer(0)
    rnka22 <- as.integer(0)
    info <- as.integer(0)

    # Hidden Parameters
    compq <- as.character("i")
    compz <- as.character("i")
    lda <- max(dim(a)[1], 1)
    lde <- max(dim(e)[1], 1)
    ldb <- max(dim(b)[1], 1)
    ldc <- max(dim(c)[1], 1)
    ldq <- max(dim(q)[1], 1)
    ldz <- max(dim(z)[1], 1)
    iwork <- array(as.integer(1), c(ldwork))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("TG01FD", JOBA = joba, L = l, N = n, M = m, P = p, A = a, E = e, B = b, C = c, TOL = tol, LDWORK = ldwork, Q = q, Z = z, RANKE = ranke, RNKA22 = rnka22, INFO = info, COMPQ = compq, COMPZ = compz, LDA = lda, LDE = lde,
        LDB = ldb, LDC = ldc, LDQ = ldq, LDZ = ldz, IWORK = iwork, DWORK = dwork)

    return(list(a = res$A, e = res$E, b = res$B, c = res$C, q = res$Q, z = res$Z, ranke = res$RANKE, rnka22 = res$RNKA22, info = res$INFO))
}
