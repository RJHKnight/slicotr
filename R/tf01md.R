tf01md <- function(n, m, p, ny, a, b, c, d, u, x) {

    # In Parameters
    m <- as.integer(m)
    n <- as.integer(n)
    ny <- as.integer(ny)
    p <- as.integer(p)

    # Out Parameters
    y <- array(as.double(0), c(ldy, ny))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    ldu <- dim(u)[1]
    ldy <- p
    dwork <- array(as.double(1), c(n))

    res <- .Fortran("TF01MD", N = n, M = m, P = p, NY = ny, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd, U = u, LDU = ldu, X = x, Y = y, LDY = ldy, DWORK = dwork, INFO = info)

    return(list(x = res$X, y = res$Y, info = res$INFO))
}
