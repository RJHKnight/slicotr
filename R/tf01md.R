#' tf01md
#'
#' Output response sequence of a linear time-invariant discrete-time system
#' @examples 

#'   To compute the output sequence of a linear time-invariant
#'   open-loop system given by its discrete-time state-space model
#'   (A,B,C,D), where A is an N-by-N general matrix.
#' 
#'   The initial state vector x(1) must be supplied by the user.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/TF01MD.html}
#' @export
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

    res <- .Fortran("TF01MD", N = n, M = m, P = p, NY = ny, A = a, B = b, C = c, D = d, U = u, X = x, Y = y, INFO = info, LDA = lda, LDB = ldb, LDC = ldc, LDD = ldd, LDU = ldu, LDY = ldy,
        DWORK = dwork)

    return(list(x = res$X, y = res$Y, info = res$INFO))
}
