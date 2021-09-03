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
    dwork <- array(as.double(1), c(ldwork))
    ldwork <- max(1, 15 * n * n + p * p + m * m + (6 * n + 3) * (p + m) + 4 * p * m + n * m + 22 * n + 7 * min(p, m))
    lcwork <- max(1, (n + m) * (n + p) + 2 * min(p, m) + max(p, m))

    res <- .Fortran("AB13DD", DICO = dico, JOBE = jobe, EQUIL = equil, JOBD = jobd, N = n, M = m, P = p, FPEAK = fpeak, A = a, LDA = lda, E = e, LDE = lde, B = b, LDB = ldb, C = c, LDC = ldc, D = d,
        LDD = ldd, GPEAK = gpeak, TOL = tol, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, LCWORK = lcwork, INFO = info)

    return(list(fpeak = res$FPEAK, gpeak = res$GPEAK, info = res$INFO))
}
