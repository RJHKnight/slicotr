ab05md <- function(uplo, n1, m1, p1, n2, p2, a1, b1, c1, d1, a2, b2, c2, d2, ldwork) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    m1 <- as.integer(m1)
    n1 <- as.integer(n1)
    n2 <- as.integer(n2)
    p1 <- as.integer(p1)
    p2 <- as.integer(p2)
    uplo <- as.character(uplo)

    # Out Parameters
    n <- as.integer(0)
    a <- array(as.double(0), c(n1 + n2, n1 + n2))
    b <- array(as.double(0), c(n1 + n2, m1))
    c <- array(as.double(0), c(p2, n1 + n2))
    d <- array(as.double(0), c(p2, m1))
    info <- as.integer(0)

    # Hidden Parameters
    over <- as.character("n")
    lda1 <- dim(a1)[1]
    ldb1 <- dim(b1)[1]
    ldc1 <- dim(c1)[1]
    ldd1 <- dim(d1)[1]
    lda2 <- dim(a2)[1]
    ldb2 <- dim(b2)[1]
    ldc2 <- dim(c2)[1]
    ldd2 <- dim(d2)[1]
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AB05MD", UPLO = uplo, N1 = n1, M1 = m1, P1 = p1, N2 = n2, P2 = p2, A1 = a1, B1 = b1, C1 = c1, D1 = d1, A2 = a2, B2 = b2, C2 = c2, D2 = d2, LDWORK = ldwork, N = n, A = a, B = b, C = c, D = d, INFO = info, OVER = over,
        LDA1 = lda1, LDB1 = ldb1, LDC1 = ldc1, LDD1 = ldd1, LDA2 = lda2, LDB2 = ldb2, LDC2 = ldc2, LDD2 = ldd2, LDA = lda, LDB = ldb, LDC = ldc, LDD = ldd, DWORK = dwork)

    return(list(n = res$N, a = res$A, b = res$B, c = res$C, d = res$D, info = res$INFO))
}
