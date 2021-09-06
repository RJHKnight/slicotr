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
    ldy <- as.integer(p)
    dwork <- array(as.double(1), c(n))

    res <- .Fortran("TF01MD", N = n, M = m, P = p, NY = ny, A = a, B = b, C = c, D = d, U = u, X = x, Y = y, INFO = info, LDA = lda, LDB = ldb, LDC = ldc, LDD = ldd, LDU = ldu, LDY = ldy, DWORK = dwork)

    return(list(x = res$X, y = res$Y, info = res$INFO))
}
