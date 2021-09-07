#' sb03md
#'
#' Solution of continuous- or discrete-time Lyapunov equations and separation estimation
#' @examples 

#'   To solve for X either the real continuous-time Lyapunov equation
#' 
#'      op(A)'*X + X*op(A) = scale*C                             (1)
#' 
#'   or the real discrete-time Lyapunov equation
#' 
#'      op(A)'*X*op(A) - X = scale*C                             (2)
#' 
#'   and/or estimate an associated condition number, called separation,
#'   where op(A) = A or A' (A**T) and C is symmetric (C = C').
#'   (A' denotes the transpose of the matrix A.) A is N-by-N, the right
#'   hand side C and the solution X are N-by-N, and scale is an output
#'   scale factor, set less than or equal to 1 to avoid overflow in X.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/SB03MD.html}
#' @export
sb03md <- function(dico, job, fact, trana, n, a, u, c, ldwork) {

    # In Parameters
    dico <- as.character(dico)
    fact <- as.character(fact)
    job <- as.character(job)
    n <- as.integer(n)
    trana <- as.character(trana)
    ldwork <- as.integer(ldwork)

    # Out Parameters
    scale <- as.double(0)
    sep <- as.double(0)
    ferr <- as.double(0)
    wr <- array(as.double(0), c(n))
    wi <- array(as.double(0), c(n))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldu <- dim(u)[1]
    ldc <- dim(c)[1]
    iwork <- array(as.integer(1), c(n * n))
    dwork <- array(as.double(1), c(ldwork))

    res <- .Fortran("SB03MD", DICO = dico, JOB = job, FACT = fact, TRANA = trana, N = n, A = a, U = u, C = c, LDWORK = ldwork, SCALE = scale, SEP = sep, FERR = ferr, WR = wr, WI = wi,
        INFO = info, LDA = lda, LDU = ldu, LDC = ldc, IWORK = iwork, DWORK = dwork)

    return(list(a = res$A, u = res$U, c = res$C, scale = res$SCALE, sep = res$SEP, ferr = res$FERR, wr = res$WR, wi = res$WI, info = res$INFO))
}
