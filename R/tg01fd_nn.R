#' tg01fd_nn
#'
#' Orthogonal reduction of a descriptor system to a SVD-like coordinate form
#' @examples 

#'   To compute for the descriptor system (A-lambda E,B,C)
#'   the orthogonal transformation matrices Q and Z such that the
#'   transformed system (Q'*A*Z-lambda Q'*E*Z, Q'*B, C*Z) is
#'   in a SVD-like coordinate form with
#' 
#'                ( A11  A12 )             ( Er  0 )
#'       Q'*A*Z = (          ) ,  Q'*E*Z = (       ) ,
#'                ( A21  A22 )             (  0  0 )
#' 
#'   where Er is an upper triangular invertible matrix.
#'   Optionally, the A22 matrix can be further reduced to the form
#' 
#'                ( Ar  X )
#'          A22 = (       ) ,
#'                (  0  0 )
#' 
#'   with Ar an upper triangular invertible matrix, and X either a full
#'   or a zero matrix.
#'   The left and/or right orthogonal transformations performed
#'   to reduce E and A22 can be optionally accumulated.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/TG01FD.html}
#' @export
tg01fd_nn <- function(joba, l, n, m, p, tol, ldwork, a, b, c, e) {

    # In Parameters
    joba <- as.character(joba)
    l <- as.integer(l)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    tol <- as.double(tol)

    compq <- as.character("n")
    compz <- as.character("n")
    q <- array(as.double(1), c(0, 0))
    z <- array(as.double(1), c(0, 0))
    ranke <- as.integer(0)
    rnka22 <- as.integer(0)
    info <- as.integer(0)
    dwork <- array(as.double(1), c(ldwork))
    iwork <- array(as.integer(1), c(ldwork))
    ldq <- as.integer(1)
    ldz <- as.integer(1)
    lda <- max(dim(a)[1], 1)
    ldb <- max(dim(b)[1], 1)
    ldc <- max(dim(c)[1], 1)
    lde <- max(dim(e)[1], 1)


    res <- .Fortran("TG01FD", COMPQ = compq, COMPZ = compz, JOBA = joba, L = l, N = n, M = m, P = p, Q = q, Z = z, RANKE = ranke, RNKA22 = rnka22, TOL = tol, LDWORK = ldwork, INFO = info, A = a, B = b, C = c, DWORK = dwork, E = e, IWORK = iwork,
        LDQ = ldq, LDZ = ldz, LDA = lda, LDB = ldb, LDC = ldc, LDE = lde)

    return(list(ranke = res$RANKE, rnka22 = res$RNKA22, info = res$INFO, a = res$A, b = res$B, c = res$C, e = res$E))
}
