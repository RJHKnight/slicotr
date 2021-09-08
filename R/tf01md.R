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

    info <- as.integer(0)
    dwork <- array(as.double(1), c(n))
    ldy <- as.integer(p)
    y <- array(as.double(0), c(ldy, ny))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    ldu <- dim(u)[1]


    res <- .Fortran("TF01MD", N = n, M = m, P = p, NY = ny, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd, U = u, LDU = ldu, X = x, Y = y, LDY = ldy, DWORK = dwork, INFO = info)

    return(list(info = res$INFO, x = res$X, y = res$Y))
}
