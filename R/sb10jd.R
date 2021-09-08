#' sb10jd
#'
#' Converting a descriptor state-space system into regular state-space form
#' @examples 

#'   To convert the descriptor state-space system
#' 
#'   E*dx/dt = A*x + B*u
#'         y = C*x + D*u
#' 
#'   into regular state-space form
#' 
#'    dx/dt = Ad*x + Bd*u
#'        y = Cd*x + Dd*u .
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SB10JD.html}
#' @export
sb10jd <- function(n, m, np, a, b, c, d, e, ldwork) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    np <- as.integer(np)

    nsys <- as.integer(0)
    info <- as.integer(0)
    dwork <- array(as.double(1), c(ldwork))
    lda <- max(dim(a)[1], 1)
    ldb <- max(dim(b)[1], 1)
    ldc <- max(dim(c)[1], 1)
    ldd <- max(dim(d)[1], 1)
    lde <- max(dim(e)[1], 1)


    res <- .Fortran("SB10JD", N = n, M = m, NP = np, A = a, B = b, C = c, D = d, E = e, NSYS = nsys, LDWORK = ldwork, INFO = info, DWORK = dwork, LDA = lda, LDB = ldb, LDC = ldc, LDD = ldd, LDE = lde)

    return(list(a = res$A, b = res$B, c = res$C, d = res$D, nsys = res$NSYS, info = res$INFO))
}
