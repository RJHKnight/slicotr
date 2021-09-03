sb02mt_cl <- function(uplo, n, m, a, b, q, r, l) {

    # In Parameters
    m <- as.integer(m)
    n <- as.integer(n)
    uplo <- as.character(uplo)

    # Out Parameters
    ipiv <- array(as.integer(0), c(1))
    oufact <- as.integer(0)
    g <- array(as.double(0), c(n, n))
    info <- as.integer(0)

    # Hidden Parameters
    jobg <- "G"
    jobl <- "N"
    fact <- "C"
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldq <- dim(q)[1]
    ldr <- dim(r)[1]
    ldl <- dim(l)[1]
    ldg <- dim(g)[1]
    iwork <- array(as.integer(1), c(m))
    dwork <- array(as.double(1), c(ldwork))
    ldwork <- 1

    res <- .Fortran("SB02MT", JOBG = jobg, JOBL = jobl, FACT = fact, UPLO = uplo, N = n, M = m, A = a, LDA = lda, B = b, LDB = ldb, Q = q, LDQ = ldq, R = r, LDR = ldr, L = l, LDL = ldl, IPIV = ipiv,
        OUFACT = oufact, G = g, LDG = ldg, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, INFO = info)

    return(list(a = res$A, b = res$B, q = res$Q, r = res$R, l = res$L, ipiv = res$IPIV, oufact = res$OUFACT, g = res$G, info = res$INFO))
}
