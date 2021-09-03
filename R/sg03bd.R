sg03bd <- function(dico, fact, trans, n, m, a, e, q, z, b, ldwork) {

    # In Parameters
    dico <- as.character(dico)
    fact <- as.character(fact)
    m <- as.integer(m)
    n <- as.integer(n)
    trans <- as.character(trans)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    scale <- as.double(0)
    alphar <- array(as.double(0), c(n))
    alphai <- array(as.double(0), c(n))
    beta <- array(as.double(0), c(n))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    lde <- dim(e)[1]
    ldq <- dim(q)[1]
    ldz <- dim(z)[1]
    ldb <- dim(b)[1]
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("SG03BD", DICO = dico, FACT = fact, TRANS = trans, N = n, M = m, A = a, LDA = lda, E = e, LDE = lde, Q = q, LDQ = ldq, Z = z, LDZ = ldz, B = b, LDB = ldb, SCALE = scale, ALPHAR = alphar,
        ALPHAI = alphai, BETA = beta, DWORK = dwork, LDWORK = ldwork, INFO = info)

    return(list(b = res$B, scale = res$SCALE, alphar = res$ALPHAR, alphai = res$ALPHAI, beta = res$BETA, info = res$INFO))
}
