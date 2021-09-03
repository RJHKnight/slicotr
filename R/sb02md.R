sb02md <- function(dico, hinv, uplo, scal, sort, n, a, g, q, ldwork) {

    # In Parameters
    dico <- as.character(dico)
    hinv <- as.character(hinv)
    n <- as.integer(n)
    scal <- as.character(scal)
    sort <- as.character(sort)
    uplo <- as.character(uplo)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    u <- array(as.double(0), c(2 * n, 2 * n))
    info <- as.integer(0)
    rcond <- as.double(0)
    wr <- array(as.double(0), c(2 * n))
    wi <- array(as.double(0), c(2 * n))
    s <- array(as.double(0), c(2 * n, 2 * n))

    # Hidden Parameters
    lda <- dim(a)[1]
    ldg <- dim(g)[1]
    ldq <- dim(q)[1]
    ldu <- dim(u)[1]
    lds <- dim(s)[1]
    iwork <- array(as.integer(1), c(2 * n))
    dwork <- array(as.double(1), c(ldwork))
    bwork <- array(as.logical(1), c(2 * n))

    res <- .Fortran("SB02MD", DICO = dico, HINV = hinv, UPLO = uplo, SCAL = scal, SORT = sort, N = n, A = a, LDA = lda, G = g, LDG = ldg, Q = q, LDQ = ldq, U = u, LDU = ldu, INFO = info, RCOND = rcond,
        WR = wr, WI = wi, S = s, LDS = lds, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, BWORK = bwork)

    return(list(a = res$A, q = res$Q, u = res$U, info = res$INFO, rcond = res$RCOND, wr = res$WR, wi = res$WI, s = res$S))
}
