tb03ad_r <- function(equil, n, m, p, a, b, c, d, tol, ldwork) {

    # In Parameters
    equil <- as.character(equil)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)

    # Out Parameters
    nr <- as.integer(0)
    index_bn <- array(as.integer(0), c(m))
    pcoeff <- array(as.double(0), c(m, m, n + 1))
    qcoeff <- array(as.double(0), c(max(m, p), max(m, p), n + 1))
    vcoeff <- array(as.double(0), c(m, n, n + 1))
    info <- as.integer(0)

    # Hidden Parameters
    leri <- as.character("r")
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    ldpco1 <- dim(pcoeff)[1]
    ldpco2 <- dim(pcoeff)[2]
    ldqco1 <- dim(qcoeff)[1]
    ldqco2 <- dim(qcoeff)[2]
    ldvco1 <- dim(vcoeff)[1]
    ldvco2 <- dim(vcoeff)[2]
    iwork <- array(as.integer(1), c(n + max(m, p)))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("TB03AD", EQUIL = equil, N = n, M = m, P = p, A = a, B = b, C = c, D = d, TOL = tol, LDWORK = ldwork, NR = nr, INDEX_BN = index_bn, PCOEFF = pcoeff, QCOEFF = qcoeff, VCOEFF = vcoeff, INFO = info, LERI = leri, LDA = lda,
        LDB = ldb, LDC = ldc, LDD = ldd, LDPCO1 = ldpco1, LDPCO2 = ldpco2, LDQCO1 = ldqco1, LDQCO2 = ldqco2, LDVCO1 = ldvco1, LDVCO2 = ldvco2, IWORK = iwork, DWORK = dwork)

    return(list(a = res$A, b = res$B, c = res$C, nr = res$NR, index_bn = res$INDEX_BN, pcoeff = res$PCOEFF, qcoeff = res$QCOEFF, vcoeff = res$VCOEFF, info = res$INFO))
}
