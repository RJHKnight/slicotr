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

    rcond <- as.double(0)
    iwork <- array(as.integer(1), c(2 * m))
    info <- as.integer(0)
    dwork <- array(as.double(1), c(ldwork))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldd <- dim(d)[1]


    res <- suppressWarnings(.Fortran("AB07ND", N = n, M = m, A = a, LDA = lda, B = b, LDB = ldb, C = c, LDC = ldc, D = d, LDD = ldd,
        RCOND = rcond, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, INFO = info))

    return(list(rcond = res$RCOND, info = res$INFO, a = res$A, b = res$B, c = res$C, d = res$D))
}
