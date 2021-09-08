#' sb04qd
#'
#' Solution of discrete-time Sylvester equations (Hessenberg-Schur method)
#' @examples 

#'   To solve for X the discrete-time Sylvester equation
#' 
#'      X + AXB = C,
#' 
#'   where A, B, C and X are general N-by-N, M-by-M, N-by-M and
#'   N-by-M matrices respectively. A Hessenberg-Schur method, which
#'   reduces A to upper Hessenberg form, H = U'AU, and B' to real
#'   Schur form, S = Z'B'Z (with U, Z orthogonal matrices), is used.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SB04QD.html}
#' @export
sb04qd <- function(n, m, a, b, c, ldwork) {

    # In Parameters
    m <- as.integer(m)
    n <- as.integer(n)
    ldwork <- as.integer(ldwork)

    dwork <- array(as.double(1), c(ldwork))
    info <- as.integer(0)
    iwork <- array(as.integer(1), c(4 * n))
    z <- array(as.double(0), c(m, m))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]
    ldz <- dim(z)[1]


    res <- .Fortran("SB04QD", N = n, M = m, DWORK = dwork, INFO = info, A = a, B = b, C = c, IWORK = iwork, LDWORK = ldwork, Z = z, LDA = lda, LDB = ldb, LDC = ldc, LDZ = ldz)

    return(list(info = res$INFO, a = res$A, b = res$B, c = res$C, z = res$Z))
}
