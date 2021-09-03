mb05md <- function(balanc, n, delta, a) {

    # In Parameters
    balanc <- as.character(balanc)
    delta <- as.double(delta)
    n <- as.integer(n)

    # Out Parameters
    v <- array(as.double(0), c(n, n))
    y <- array(as.double(0), c(n, n))
    valr <- array(as.double(0), c(n))
    vali <- array(as.double(0), c(n))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldv <- dim(v)[1]
    ldy <- dim(y)[1]
    iwork <- array(as.integer(1), c(n))
    dwork <- array(as.double(1), c(4 * n))
    ldwork <- dim(dwork)[1]

    res <- .Fortran("MB05MD", BALANC = balanc, N = n, DELTA = delta, A = a, LDA = lda, V = v, LDV = ldv, Y = y, LDY = ldy, VALR = valr, VALI = vali, IWORK = iwork, DWORK = dwork, LDWORK = ldwork,
        INFO = info)

    return(list(a = res$A, v = res$V, y = res$Y, valr = res$VALR, vali = res$VALI, info = res$INFO))
}
