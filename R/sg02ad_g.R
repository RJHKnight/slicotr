#' sg02ad_g
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
sg02ad_g <- function(dico, uplo, sort, acc, n, a, b, e, ldwork, q) {

    # In Parameters
    acc <- as.character(acc)
    dico <- as.character(dico)
    n <- as.integer(n)
    sort <- as.character(sort)
    uplo <- as.character(uplo)
    ldwork <- as.integer(ldwork)

    jobb <- as.character("g")
    fact <- as.character("n")
    jobl <- as.character("z")
    scal <- as.character("n")
    m <- as.integer(0)
    p <- as.integer(0)
    r <- array(as.double(1), c(1, 1))
    l <- array(as.double(1), c(1, 1))
    rcondu <- as.double(0)
    tol <- as.double(0)
    dwork <- array(as.double(1), c(ldwork))
    iwarn <- as.integer(0)
    info <- as.integer(0)
    alfai <- array(as.double(0), c(2 * n))
    alfar <- array(as.double(0), c(2 * n))
    beta <- array(as.double(0), c(2 * n))
    bwork <- array(as.logical(1), c(2 * n))
    iwork <- array(as.integer(1), c(2 * n))
    ldl <- dim(l)[1]
    ldr <- dim(r)[1]
    s <- array(as.double(0), c(max(1, 2 * n), 2 * n))
    t <- array(as.double(0), c(max(1, 2 * n), 2 * n))
    u <- array(as.double(0), c(max(1, 2 * n), 2 * n))
    x <- array(as.double(0), c(max(1, n), n))
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    lde <- dim(e)[1]
    ldq <- dim(q)[1]
    lds <- dim(s)[1]
    ldt <- dim(t)[1]
    ldu <- dim(u)[1]
    ldx <- dim(x)[1]


    res <- .Fortran("SG02AD", DICO = dico, JOBB = jobb, FACT = fact, UPLO = uplo, JOBL = jobl, SCAL = scal, SORT = sort, ACC = acc, N = n, M = m, P = p, R = r, L = l, RCONDU = rcondu, TOL = tol, DWORK = dwork, IWARN = iwarn, INFO = info,
        A = a, ALFAI = alfai, ALFAR = alfar, B = b, BETA = beta, BWORK = bwork, E = e, IWORK = iwork, LDL = ldl, LDR = ldr, LDWORK = ldwork, Q = q, S = s, T = t, U = u, X = x, LDA = lda, LDB = ldb, LDE = lde, LDQ = ldq, LDS = lds, LDT = ldt,
        LDU = ldu, LDX = ldx)

    return(list(rcondu = res$RCONDU, iwarn = res$IWARN, info = res$INFO, alfai = res$ALFAI, alfar = res$ALFAR, beta = res$BETA, s = res$S, t = res$T, u = res$U, x = res$X))
}
