sg02ad_bd <- function(dico, jobl, scal, sort, acc, n, m, p, a, e, b, q, r, l, tol, ldwork) {

    # In Parameters
    acc <- as.character(acc)
    dico <- as.character(dico)
    jobl <- as.character(jobl)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    scal <- as.character(scal)
    sort <- as.character(sort)
    tol <- as.double(tol)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    rcondu <- as.double(0)
    x <- array(as.double(0), c(n, n))
    alfar <- array(as.double(0), c(2 * n))
    alfai <- array(as.double(0), c(2 * n))
    beta <- array(as.double(0), c(2 * n))
    s <- array(as.double(0), c(max(1, 2 * n + m), 2 * n + m))
    t <- array(as.double(0), c(max(1, 2 * n + m), 2 * n))
    u <- array(as.double(0), c(max(1, 2 * n), 2 * n))
    iwarn <- as.integer(0)
    info <- as.integer(0)

    # Hidden Parameters
    jobb <- as.character("b")
    fact <- as.character("d")
    uplo <- as.character("u")
    lda <- dim(a)[1]
    lde <- dim(e)[1]
    ldb <- dim(b)[1]
    ldq <- dim(q)[1]
    ldr <- dim(r)[1]
    ldl <- dim(l)[1]
    ldx <- dim(x)[1]
    lds <- dim(s)[1]
    ldt <- dim(t)[1]
    ldu <- dim(u)[1]
    iwork <- array(as.integer(1), c(max(m, 2 * n)))
    dwork <- array(as.double(1), c(ldwork))
    bwork <- array(as.logical(1), c(2 * n))

    res <- .Fortran("SG02AD", DICO = dico, JOBL = jobl, SCAL = scal, SORT = sort, ACC = acc, N = n, M = m, P = p, A = a, E = e, B = b, Q = q, R = r, L = l, TOL = tol, LDWORK = ldwork, RCONDU = rcondu, X = x, ALFAR = alfar, ALFAI = alfai,
        BETA = beta, S = s, T = t, U = u, IWARN = iwarn, INFO = info, JOBB = jobb, FACT = fact, UPLO = uplo, LDA = lda, LDE = lde, LDB = ldb, LDQ = ldq, LDR = ldr, LDL = ldl, LDX = ldx, LDS = lds, LDT = ldt, LDU = ldu, IWORK = iwork,
        DWORK = dwork, BWORK = bwork)

    return(list(rcondu = res$RCONDU, x = res$X, alfar = res$ALFAR, alfai = res$ALFAI, beta = res$BETA, s = res$S, t = res$T, u = res$U, iwarn = res$IWARN, info = res$INFO))
}
