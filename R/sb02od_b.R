sb02od_b <- function(dico, uplo, jobl, sort, n, m, p, a, b, q, r, l, tol, ldwork) {

    # In Parameters
    dico <- as.character(dico)
    jobl <- as.character(jobl)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    sort <- as.character(sort)
    tol <- as.double(tol)
    uplo <- as.character(uplo)

    # Out Parameters
    rcond <- as.double(0)
    x <- array(as.double(0), c(n, n))
    alfar <- array(as.double(0), c(2 * n))
    alfai <- array(as.double(0), c(2 * n))
    beta <- array(as.double(0), c(2 * n))
    s <- array(as.double(0), c(2 * n + m, 2 * n + m))
    t <- array(as.double(0), c(2 * n + m, 2 * n))
    info <- as.integer(0)

    # Hidden Parameters
    jobb <- "B"
    fact <- "B"
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldq <- dim(q)[1]
    ldr <- dim(r)[1]
    ldl <- dim(l)[1]
    ldx <- dim(x)[1]
    lds <- dim(s)[1]
    ldt <- dim(t)[1]
    u <- array(as.double(1), c(2 * n, 2 * n))
    ldu <- dim(u)[1]
    iwork <- array(as.integer(1), c(max(2 * n, m)))
    dwork <- array(as.double(1), c(ldwork))
    bwork <- array(as.logical(1), c(2 * n))

    res <- .Fortran("SB02OD", DICO = dico, JOBB = jobb, FACT = fact, UPLO = uplo, JOBL = jobl, SORT = sort, N = n, M = m, P = p, A = a, LDA = lda, B = b, LDB = ldb, Q = q, LDQ = ldq, R = r, LDR = ldr,
        L = l, LDL = ldl, RCOND = rcond, X = x, LDX = ldx, ALFAR = alfar, ALFAI = alfai, BETA = beta, S = s, LDS = lds, T = t, LDT = ldt, U = u, LDU = ldu, TOL = tol, IWORK = iwork, DWORK = dwork,
        LDWORK = ldwork, BWORK = bwork, INFO = info)

    return(list(rcond = res$RCOND, x = res$X, alfar = res$ALFAR, alfai = res$ALFAI, beta = res$BETA, s = res$S, t = res$T, info = res$INFO))
}
