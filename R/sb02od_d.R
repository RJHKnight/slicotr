#' sb02od_d
#'
#' Solution of continuous- or discrete-time algebraic Riccati equations (generalized Schur vectors method)
#' @examples 

#'   To solve for X either the continuous-time algebraic Riccati
#'   equation
#'                            -1
#'      Q + A'X + XA - (L+XB)R  (L+XB)' = 0                       (1)
#' 
#'   or the discrete-time algebraic Riccati equation
#'                                   -1
#'      X = A'XA - (L+A'XB)(R + B'XB)  (L+A'XB)' + Q              (2)
#' 
#'   where A, B, Q, R, and L are N-by-N, N-by-M, N-by-N, M-by-M and
#'   N-by-M matrices, respectively, such that Q = C'C, R = D'D and
#'   L = C'D; X is an N-by-N symmetric matrix.
#'   The routine also returns the computed values of the closed-loop
#'   spectrum of the system, i.e., the stable eigenvalues lambda(1),
#'   ..., lambda(N) of the corresponding Hamiltonian or symplectic
#'   pencil, in the continuous-time case or discrete-time case,
#'   respectively.
#'                            -1
#'   Optionally, matrix G = BR  B' may be given instead of B and R.
#'   Other options include the case with Q and/or R given in a
#'   factored form, Q = C'C, R = D'D, and with L a zero matrix.
#' 
#'   The routine uses the method of deflating subspaces, based on
#'   reordering the eigenvalues in a generalized Schur matrix pair.
#'   A standard eigenproblem is solved in the continuous-time case
#'   if G is given.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SB02OD.html}
#' @export
sb02od_d <- function(dico, uplo, jobl, sort, n, m, p, a, b, q, r, l, tol, ldwork) {

    # In Parameters
    dico <- as.character(dico)
    jobl <- as.character(jobl)
    ldwork <- as.integer(ldwork)
    m <- as.integer(m)
    n <- as.integer(n)
    p <- as.integer(p)
    sort <- as.character(sort)
    tol <- as.double(tol)
    uplo <- as.character(uplo)

    jobb <- as.character("b")
    fact <- as.character("d")
    rcond <- as.double(0)
    x <- array(as.double(0), c(n, n))
    alfar <- array(as.double(0), c(2 * n))
    alfai <- array(as.double(0), c(2 * n))
    beta <- array(as.double(0), c(2 * n))
    s <- array(as.double(0), c(2 * n + m, 2 * n + m))
    t <- array(as.double(0), c(2 * n + m, 2 * n))
    u <- array(as.double(1), c(2 * n, 2 * n))
    dwork <- array(as.double(1), c(ldwork))
    info <- as.integer(0)
    bwork <- array(as.logical(1), c(2 * n))
    iwork <- array(as.integer(1), c(max(2 * n, m)))
    lds <- dim(s)[1]
    ldt <- dim(t)[1]
    ldu <- dim(u)[1]
    ldx <- dim(x)[1]
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldl <- dim(l)[1]
    ldq <- dim(q)[1]
    ldr <- dim(r)[1]


    res <- .Fortran("SB02OD", DICO = dico, JOBB = jobb, FACT = fact, UPLO = uplo, JOBL = jobl, SORT = sort, N = n, M = m, P = p, A = a, LDA = lda, B = b, LDB = ldb, Q = q, LDQ = ldq, R = r, LDR = ldr, L = l, LDL = ldl, RCOND = rcond,
        X = x, LDX = ldx, ALFAR = alfar, ALFAI = alfai, BETA = beta, S = s, LDS = lds, T = t, LDT = ldt, U = u, LDU = ldu, TOL = tol, IWORK = iwork, DWORK = dwork, LDWORK = ldwork, BWORK = bwork, INFO = info)

    return(list(rcond = res$RCOND, x = res$X, alfar = res$ALFAR, alfai = res$ALFAI, beta = res$BETA, s = res$S, t = res$T, info = res$INFO))
}
