#' sg02ad_bn
#'
#' Solution of continuous- or discrete-time algebraic Riccati equations for descriptor systems
#' @examples 

#'   To solve for X either the continuous-time algebraic Riccati
#'   equation
#'                                 -1
#'      Q + A'XE + E'XA - (L+E'XB)R  (L+E'XB)' = 0 ,              (1)
#' 
#'   or the discrete-time algebraic Riccati equation
#'                                      -1
#'      E'XE = A'XA - (L+A'XB)(R + B'XB)  (L+A'XB)' + Q ,         (2)
#' 
#'   where A, E, B, Q, R, and L are N-by-N, N-by-N, N-by-M, N-by-N,
#'   M-by-M and N-by-M matrices, respectively, such that Q = C'C,
#'   R = D'D and L = C'D; X is an N-by-N symmetric matrix.
#'   The routine also returns the computed values of the closed-loop
#'   spectrum of the system, i.e., the stable eigenvalues
#'   lambda(1),...,lambda(N) of the pencil (A - BF,E), where F is
#'   the optimal gain matrix,
#'           -1
#'      F = R  (L+E'XB)' ,        for (1),
#' 
#'   and
#'                  -1
#'      F = (R+B'XB)  (L+A'XB)' , for (2).
#'                            -1
#'   Optionally, matrix G = BR  B' may be given instead of B and R.
#'   Other options include the case with Q and/or R given in a
#'   factored form, Q = C'C, R = D'D, and with L a zero matrix.
#' 
#'   The routine uses the method of deflating subspaces, based on
#'   reordering the eigenvalues in a generalized Schur matrix pair.
#' 
#'   It is assumed that E is nonsingular, but this condition is not
#'   checked. Note that the definition (1) of the continuous-time
#'   algebraic Riccati equation, and the formula for the corresponding
#'   optimal gain matrix, require R to be nonsingular, but the
#'   associated linear quadratic optimal problem could have a unique
#'   solution even when matrix R is singular, under mild assumptions
#'   (see METHOD). The routine SG02AD works accordingly in this case.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SG02AD.html}
#' @export
sg02ad_bn <- function(dico, uplo, jobl, scal, sort, acc, n, m, a, e, b, q, r, l, tol, ldwork) {

    # In Parameters
    acc <- as.character(acc)
    dico <- as.character(dico)
    jobl <- as.character(jobl)
    m <- as.integer(m)
    n <- as.integer(n)
    scal <- as.character(scal)
    sort <- as.character(sort)
    tol <- as.double(tol)
    uplo <- as.character(uplo)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    rcondu <- as.double(0)
    x <- array(as.double(0), c(max(1, n), n))
    alfar <- array(as.double(0), c(2 * n))
    alfai <- array(as.double(0), c(2 * n))
    beta <- array(as.double(0), c(2 * n))
    s <- array(as.double(0), c(max(1, 2 * n + m), 2 * n + m))
    t <- array(as.double(0), c(max(1, 2 * n + m), 2 * n))
    u <- array(as.double(0), c(max(1, 2 * n), 2 * n))
    iwarn <- as.integer(0)
    info <- as.integer(0)

    # Hidden Parameters
    jobb <- as.character("b")
    fact <- as.character("n")
    p <- as.integer(0)
    lda <- dim(a)[1]
    lde <- dim(e)[1]
    ldb <- dim(b)[1]
    ldq <- dim(q)[1]
    ldr <- dim(r)[1]
    ldl <- dim(l)[1]
    ldx <- dim(x)[1]
    lds <- dim(s)[1]
    ldt <- dim(t)[1]
    ldu <- dim(u)[1]
    iwork <- array(as.integer(1), c(max(m, 2 * n)))
    dwork <- array(as.double(1), c(ldwork))
    bwork <- array(as.logical(1), c(2 * n))

    res <- .Fortran("SG02AD", DICO = dico, UPLO = uplo, JOBL = jobl, SCAL = scal, SORT = sort, ACC = acc, N = n, M = m, A = a, E = e, B = b, Q = q, R = r, L = l, TOL = tol, LDWORK = ldwork,
        RCONDU = rcondu, X = x, ALFAR = alfar, ALFAI = alfai, BETA = beta, S = s, T = t, U = u, IWARN = iwarn, INFO = info, JOBB = jobb, FACT = fact, P = p, LDA = lda, LDE = lde, LDB = ldb,
        LDQ = ldq, LDR = ldr, LDL = ldl, LDX = ldx, LDS = lds, LDT = ldt, LDU = ldu, IWORK = iwork, DWORK = dwork, BWORK = bwork)

    return(list(rcondu = res$RCONDU, x = res$X, alfar = res$ALFAR, alfai = res$ALFAI, beta = res$BETA, s = res$S, t = res$T, u = res$U, iwarn = res$IWARN, info = res$INFO))
}
