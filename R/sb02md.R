#' sb02md
#'
#' Solution of continuous- or discrete-time algebraic Riccati equations (Schur vectors method)
#'
#'   To solve for X either the continuous-time algebraic Riccati
#'   equation
#'                            -1
#'      Q + A'*X + X*A - X*B*R  B'*X = 0                            (1)
#' 
#'   or the discrete-time algebraic Riccati equation
#'                                      -1
#'      X = A'*X*A - A'*X*B*(R + B'*X*B)  B'*X*A + Q                (2)
#' 
#'   where A, B, Q and R are N-by-N, N-by-M, N-by-N and M-by-M matrices
#'   respectively, with Q symmetric and R symmetric nonsingular; X is
#'   an N-by-N symmetric matrix.
#'                     -1
#'   The matrix G = B*R  B' must be provided on input, instead of B and
#'   R, that is, for instance, the continuous-time equation
#' 
#'      Q + A'*X + X*A - X*G*X = 0                                  (3)
#' 
#'   is solved, where G is an N-by-N symmetric matrix. SLICOT Library
#'   routine SB02MT should be used to compute G, given B and R. SB02MT
#'   also enables to solve Riccati equations corresponding to optimal
#'   problems with coupling terms.
#' 
#'   The routine also returns the computed values of the closed-loop
#'   spectrum of the optimal system, i.e., the stable eigenvalues
#'   lambda(1),...,lambda(N) of the corresponding Hamiltonian or
#'   symplectic matrix associated to the optimal problem.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SB02MD.html}
#' @export
sb02md <- function(dico, hinv, uplo, scal, sort, n, a, g, q, ldwork) {

    # In Parameters
    dico <- as.character(dico)
    hinv <- as.character(hinv)
    n <- as.integer(n)
    scal <- as.character(scal)
    sort <- as.character(sort)
    uplo <- as.character(uplo)
    ldwork <- as.integer(ldwork)

    info <- as.integer(0)
    rcond <- as.double(0)
    dwork <- array(as.double(1), c(ldwork))
    bwork <- array(as.logical(1), c(2 * n))
    iwork <- array(as.integer(1), c(2 * n))
    s <- array(as.double(0), c(2 * n, 2 * n))
    u <- array(as.double(0), c(2 * n, 2 * n))
    wi <- array(as.double(0), c(2 * n))
    wr <- array(as.double(0), c(2 * n))
    lda <- dim(a)[1]
    ldg <- dim(g)[1]
    ldq <- dim(q)[1]
    lds <- dim(s)[1]
    ldu <- dim(u)[1]


    res <- suppressWarnings(.Fortran("SB02MD", DICO = dico, HINV = hinv, UPLO = uplo, SCAL = scal, SORT = sort, N = n, A = a, LDA = lda,
        G = g, LDG = ldg, Q = q, LDQ = ldq, RCOND = rcond, WR = wr, WI = wi, S = s, LDS = lds, U = u, LDU = ldu, IWORK = iwork,
        DWORK = dwork, LDWORK = ldwork, BWORK = bwork, INFO = info))

    return(list(info = res$INFO, rcond = res$RCOND, a = res$A, q = res$Q, s = res$S, u = res$U, wi = res$WI, wr = res$WR))
}
