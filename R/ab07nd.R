#' ab07nd
#'
#' Inverse of a given linear system
#' @examples 

#'   To compute the inverse (Ai,Bi,Ci,Di) of a given system (A,B,C,D).
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/AB07ND.html}
#' @export
ab07nd <- function(n, m, a, b, c, d, ldwork) {

    # In Parameters
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)

    # Out Parameters
    rcond <- as.double(0)
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]
    iwork <- array(as.integer(1), c(2 * m))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("AB07ND", N = n, M = m, A = a, B = b, C = c, D = d, LDWORK = ldwork, RCOND = rcond, INFO = info, LDA = lda, LDB = ldb, LDC = ldc, LDD = ldd, IWORK = iwork, DWORK = dwork)

    return(list(a = res$A, b = res$B, c = res$C, d = res$D, rcond = res$RCOND, info = res$INFO))
}
