sb03od <- function(dico, fact, trans, n, m, a, q, b, ldwork) {

    # In Parameters
    dico <- as.character(dico)
    fact <- as.character(fact)
    m <- as.integer(m)
    n <- as.integer(n)
    trans <- as.character(trans)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    scale <- as.double(0)
    wr <- array(as.double(0), c(n))
    wi <- array(as.double(0), c(n))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldq <- dim(q)[1]
    ldb <- dim(b)[1]
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("   FORTRANNAME SB03OD", DICO = dico, FACT = fact, TRANS = trans, N = n, M = m, A = a, Q = q, B = b, LDWORK = ldwork, SCALE = scale, WR = wr, WI = wi, INFO = info, LDA = lda, LDQ = ldq, LDB = ldb, DWORK = dwork)

    return(list(b = res$B, scale = res$SCALE, wr = res$WR, wi = res$WI, info = res$INFO))
}
