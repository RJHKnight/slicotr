ab09nd <- function(dico, job, equil, ordsel, n, m, p, nr, alpha, a, b, c, d, tol1, tol2, ldwork) {

    # In Parameters
    alpha <- as.double(alpha)
    dico <- as.character(dico)
    equil <- as.character(equil)
    job <- as.character(job)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    nr <- as.integer(nr)
    ordsel <- as.character(ordsel)
    p <- as.integer(p)
    tol1 <- as.double(tol1)
    tol2 <- as.double(tol2)

    # Out Parameters
    ns <- as.integer(0)
    hsv <- array(as.double(0), c(n))
    iwarn <- as.integer(0)
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    iwork <- array(as.integer(1), c(max(m, p)))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AB09ND", DICO = dico, JOB = job, EQUIL = equil, ORDSEL = ordsel, N = n, M = m, P = p, NR = nr, ALPHA = alpha, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd,
        NS = ns, HSV = hsv, TOL1 = tol1, TOL2 = tol2, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, IWARN = iwarn, INFO = info)

    return(list(nr = res$NR, a = res$A, b = res$B, c = res$C, d = res$D, ns = res$NS, hsv = res$HSV, iwarn = res$IWARN, info = res$INFO))
}
