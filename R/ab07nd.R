#' ab07nd
#'
#' Inverse of a given linear system
#' @examples 

#'   To compute the inverse (Ai,Bi,Ci,Di) of a given system (A,B,C,D).
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB07ND.html}
#' @export
ab07nd <- function(n, m, ldwork, a, b, c, d) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)

    rcond <- as.double(0)
    iwork <- array(as.integer(1), c(2 * m))
    info <- as.integer(0)
    dwork <- array(as.double(1), c(ldwork))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]


    res <- .Fortran("AB07ND", N = n, M = m, RCOND = rcond, IWORK = iwork, LDWORK = ldwork, INFO = info, A = a, B = b, C = c, D = d, DWORK = dwork, LDA = lda, LDB = ldb, LDC = ldc, LDD = ldd)

    return(list(rcond = res$RCOND, info = res$INFO, a = res$A, b = res$B, c = res$C, d = res$D))
}
