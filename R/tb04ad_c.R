tb04ad_c <- function(n, m, p, a, b, c, d, tol1, tol2, ldwork) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol1 <- as.double(tol1)
    tol2 <- as.double(tol2)

    # Out Parameters
    nr <- as.integer(0)
    index_bn <- array(as.integer(0), c(m))
    dcoeff <- array(as.double(0), c(max(1, m), n + 1))
    ucoeff <- array(as.double(0), c(max(1, m), max(1, p), n + 1))
    info <- as.integer(0)

    # Hidden Parameters
    rowcol <- "C"
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    lddcoe <- dim(dcoeff)[1]
    lduco1 <- dim(ucoeff)[1]
    lduco2 <- dim(ucoeff)[2]
    iwork <- array(as.integer(1), c(n + max(m, p)))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("   FORTRAN`NA`ME TB04AD", ROWCOL = rowcol, N = n, M = m, P = p, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd, NR = nr, INDEX_BN = index_bn, DCOEFF = dcoeff,
        LDDCOE = lddcoe, UCOEFF = ucoeff, LDUCO1 = lduco1, LDUCO2 = lduco2, TOL1 = tol1, TOL2 = tol2, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, INFO = info)

    return(list(a = res$A, b = res$B, c = res$C, nr = res$NR, index_bn = res$INDEX_BN, dcoeff = res$DCOEFF, ucoeff = res$UCOEFF, info = res$INFO))
}
