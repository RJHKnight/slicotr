ab13bd <- function(dico, jobn, n, m, p, a, b, c, d, tol) {

    # In Parameters
    dico <- as.character(dico)
    jobn <- as.character(jobn)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)

    # Out Parameters
    nq <- as.integer(0)
    iwarn <- as.integer(0)
    info <- as.integer(0)
    ab13bd <- as.double(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    dwork <- array(as.double(1), c(ldwork))
    ldwork <- max(1, max(m * (n + m) + max(n * (n + 5), max(m * (m + 2), 4 * p)), n * (max(n, p) + 4) + min(n, p)))

    res <- .Fortran("AB13BD", DICO = dico, JOBN = jobn, N = n, M = m, P = p, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd, NQ = nq, TOL = tol, DWORK = dwork, LDWORK = ldwork,
        IWARN = iwarn, INFO = info, AB13BD = ab13bd)

    return(list(nq = res$NQ, iwarn = res$IWARN, info = res$INFO, ab13bd = res$AB13BD))
}
