sg03ad <- function(dico, job, fact, trans, uplo, n, a, e, q, z, x, ldwork) {

    # In Parameters
    dico <- as.character(dico)
    fact <- as.character(fact)
    job <- as.character(job)
    n <- as.integer(n)
    trans <- as.character(trans)
    uplo <- as.character(uplo)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    scale <- as.double(0)
    sep <- as.double(0)
    ferr <- as.double(0)
    alphar <- array(as.double(0), c(n))
    alphai <- array(as.double(0), c(n))
    beta <- array(as.double(0), c(n))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    lde <- dim(e)[1]
    ldq <- dim(q)[1]
    ldz <- dim(z)[1]
    ldx <- dim(x)[1]
    iwork <- array(as.integer(1), c(n * n))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("SG03AD", DICO = dico, JOB = job, FACT = fact, TRANS = trans, UPLO = uplo, N = n, A = a, E = e, Q = q, Z = z, X = x, LDWORK = ldwork, SCALE = scale, SEP = sep, FERR = ferr, ALPHAR = alphar, ALPHAI = alphai, BETA = beta,
        INFO = info, LDA = lda, LDE = lde, LDQ = ldq, LDZ = ldz, LDX = ldx, IWORK = iwork, DWORK = dwork)

    return(list(a = res$A, e = res$E, q = res$Q, z = res$Z, x = res$X, scale = res$SCALE, sep = res$SEP, ferr = res$FERR, alphar = res$ALPHAR, alphai = res$ALPHAI, beta = res$BETA, info = res$INFO))
}
