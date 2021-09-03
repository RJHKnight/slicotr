sg02ad_g <- function(dico, uplo, sort, acc, n, a, e, b, q, ldwork) {

    # In Parameters
    acc <- as.character(acc)
    dico <- as.character(dico)
    n <- as.integer(n)
    sort <- as.character(sort)
    uplo <- as.character(uplo)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    rcondu <- as.double(0)
    x <- array(as.double(0), c(max(1, n), n))
    alfar <- array(as.double(0), c(2 * n))
    alfai <- array(as.double(0), c(2 * n))
    beta <- array(as.double(0), c(2 * n))
    s <- array(as.double(0), c(max(1, 2 * n), 2 * n))
    t <- array(as.double(0), c(max(1, 2 * n), 2 * n))
    u <- array(as.double(0), c(max(1, 2 * n), 2 * n))
    iwarn <- as.integer(0)
    info <- as.integer(0)

    # Hidden Parameters
    jobb <- "G"
    fact <- "N"
    jobl <- "Z"
    scal <- "N"
    m <- 0
    p <- 0
    lda <- dim(a)[1]
    lde <- dim(e)[1]
    ldb <- dim(b)[1]
    ldq <- dim(q)[1]
    r <- array(as.double(1), c(1, 1))
    ldr <- dim(r)[1]
    l <- array(as.double(1), c(1, 1))
    ldl <- dim(l)[1]
    ldx <- dim(x)[1]
    lds <- dim(s)[1]
    ldt <- dim(t)[1]
    ldu <- dim(u)[1]
    tol <- 0
    iwork <- array(as.integer(1), c(2 * n))
    dwork <- array(as.double(1), c(ldwork))
    bwork <- array(as.logical(1), c(2 * n))

    res <- .Fortran("SG02AD", DICO = dico, JOBB = jobb, FACT = fact, UPLO = uplo, JOBL = jobl, SCAL = scal, SORT = sort, ACC = acc, N = n, M = m, P = p, A = a, LDA = lda, E = e, LDE = lde, B = b,
        LDB = ldb, Q = q, LDQ = ldq, R = r, LDR = ldr, L = l, LDL = ldl, RCONDU = rcondu, X = x, LDX = ldx, ALFAR = alfar, ALFAI = alfai, BETA = beta, S = s, LDS = lds, T = t, LDT = ldt, U = u, LDU = ldu,
        TOL = tol, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, BWORK = bwork, IWARN = iwarn, INFO = info)

    return(list(rcondu = res$RCONDU, x = res$X, alfar = res$ALFAR, alfai = res$ALFAI, beta = res$BETA, s = res$S, t = res$T, u = res$U, iwarn = res$IWARN, info = res$INFO))
}
