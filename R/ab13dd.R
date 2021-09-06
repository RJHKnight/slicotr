ab13dd <- function(dico, jobe, equil, jobd, n, m, p, fpeak, a, e, b, c, d, tol) {

    # In Parameters
    dico <- as.character(dico)
    equil <- as.character(equil)
    jobd <- as.character(jobd)
    jobe <- as.character(jobe)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)

    # Out Parameters
    gpeak <- array(as.double(0), c(2))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    lde <- dim(e)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    iwork <- array(as.integer(1), c(n))
    ldwork <- as.integer(max(1, 15 * n * n + p * p + m * m + (6 * n + 3) * (p + m) + 4 * p * m + n * m + 22 * n + 7 * min(p, m)))
    lcwork <- as.integer(max(1, (n + m) * (n + p) + 2 * min(p, m) + max(p, m)))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AB13DD", DICO = dico, JOBE = jobe, EQUIL = equil, JOBD = jobd, N = n, M = m, P = p, FPEAK = fpeak, A = a, E = e, B = b, C = c, D = d, TOL = tol, GPEAK = gpeak, INFO = info, LDA = lda, LDE = lde, LDB = ldb, LDC = ldc,
        LDD = ldd, IWORK = iwork, LDWORK = ldwork, LCWORK = lcwork, DWORK = dwork)

    return(list(fpeak = res$FPEAK, gpeak = res$GPEAK, info = res$INFO))
}
