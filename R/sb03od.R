#' sb03od
#'
#' Solution of stable continuous- or discrete-time Lyapunov equations (Cholesky factor)
#' @examples 

#'   To solve for X = op(U)'*op(U) either the stable non-negative
#'   definite continuous-time Lyapunov equation
#'                                 2
#'      op(A)'*X + X*op(A) = -scale *op(B)'*op(B)                   (1)
#' 
#'   or the convergent non-negative definite discrete-time Lyapunov
#'   equation
#'                                 2
#'      op(A)'*X*op(A) - X = -scale *op(B)'*op(B)                   (2)
#' 
#'   where op(K) = K or K' (i.e., the transpose of the matrix K), A is
#'   an N-by-N matrix, op(B) is an M-by-N matrix, U is an upper
#'   triangular matrix containing the Cholesky factor of the solution
#'   matrix X, X = op(U)'*op(U), and scale is an output scale factor,
#'   set less than or equal to 1 to avoid overflow in X. If matrix B
#'   has full rank then the solution matrix X will be positive-definite
#'   and hence the Cholesky factor U will be nonsingular, but if B is
#'   rank deficient then X may be only positive semi-definite and U
#'   will be singular.
#' 
#'   In the case of equation (1) the matrix A must be stable (that
#'   is, all the eigenvalues of A must have negative real parts),
#'   and for equation (2) the matrix A must be convergent (that is,
#'   all the eigenvalues of A must lie inside the unit circle).
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SB03OD.html}
#' @export
sb03od <- function(dico, fact, trans, n, m, a, q, b, ldwork) {

    # In Parameters
    dico <- as.character(dico)
    fact <- as.character(fact)
    m <- as.integer(m)
    n <- as.integer(n)
    trans <- as.character(trans)
    ldwork <- as.integer(ldwork)

    scale <- as.double(0)
    dwork <- array(as.double(1), c(ldwork))
    info <- as.integer(0)
    wi <- array(as.double(0), c(n))
    wr <- array(as.double(0), c(n))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldq <- dim(q)[1]


    res <- suppressWarnings(.Fortran("SB03OD", DICO = dico, FACT = fact, TRANS = trans, N = n, M = m, A = a, LDA = lda, Q = q,
        LDQ = ldq, B = b, LDB = ldb, SCALE = scale, WR = wr, WI = wi, DWORK = dwork, LDWORK = ldwork, INFO = info))

    return(list(scale = res$SCALE, info = res$INFO, b = res$B, wi = res$WI, wr = res$WR))
}
