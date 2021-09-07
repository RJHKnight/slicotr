#' sg03bd
#'
#' Solving (for Cholesky factor) generalized stable continuous- or discrete-time Lyapunov equations
#' @examples 

#'   To compute the Cholesky factor U of the matrix X,
#' 
#'               T
#'      X = op(U)  * op(U),
#' 
#'   which is the solution of either the generalized
#'   c-stable continuous-time Lyapunov equation
#' 
#'           T                    T
#'      op(A)  * X * op(E) + op(E)  * X * op(A)
#' 
#'               2        T
#'      = - SCALE  * op(B)  * op(B),                                (1)
#' 
#'   or the generalized d-stable discrete-time Lyapunov equation
#' 
#'           T                    T
#'      op(A)  * X * op(A) - op(E)  * X * op(E)
#' 
#'               2        T
#'      = - SCALE  * op(B)  * op(B),                                (2)
#' 
#'   without first finding X and without the need to form the matrix
#'   op(B)**T * op(B).
#' 
#'   op(K) is either K or K**T for K = A, B, E, U. A and E are N-by-N
#'   matrices, op(B) is an M-by-N matrix. The resulting matrix U is an
#'   N-by-N upper triangular matrix with non-negative entries on its
#'   main diagonal. SCALE is an output scale factor set to avoid
#'   overflow in U.
#' 
#'   In the continuous-time case (1) the pencil A - lambda * E must be
#'   c-stable (that is, all eigenvalues must have negative real parts).
#'   In the discrete-time case (2) the pencil A - lambda * E must be
#'   d-stable (that is, the moduli of all eigenvalues must be smaller
#'   than one).
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SG03BD.html}
#' @export
sg03bd <- function(dico, fact, trans, n, m, a, e, q, z, b, ldwork) {

    # In Parameters
    dico <- as.character(dico)
    fact <- as.character(fact)
    m <- as.integer(m)
    n <- as.integer(n)
    trans <- as.character(trans)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    scale <- as.double(0)
    alphar <- array(as.double(0), c(n))
    alphai <- array(as.double(0), c(n))
    beta <- array(as.double(0), c(n))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    lde <- dim(e)[1]
    ldq <- dim(q)[1]
    ldz <- dim(z)[1]
    ldb <- dim(b)[1]
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("SG03BD", DICO = dico, FACT = fact, TRANS = trans, N = n, M = m, A = a, E = e, Q = q, Z = z, B = b, LDWORK = ldwork, SCALE = scale, ALPHAR = alphar, ALPHAI = alphai,
        BETA = beta, INFO = info, LDA = lda, LDE = lde, LDQ = ldq, LDZ = ldz, LDB = ldb, DWORK = dwork)

    return(list(b = res$B, scale = res$SCALE, alphar = res$ALPHAR, alphai = res$ALPHAI, beta = res$BETA, info = res$INFO))
}
