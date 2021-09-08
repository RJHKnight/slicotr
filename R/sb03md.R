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

    scale <- as.double(0)
    sep <- as.double(0)
    ferr <- as.double(0)
    dwork <- array(as.double(1), c(ldwork))
    info <- as.integer(0)
    iwork <- array(as.integer(1), c(n * n))
    wi <- array(as.double(0), c(n))
    wr <- array(as.double(0), c(n))
    lda <- dim(a)[1]
    ldc <- dim(c)[1]
    ldu <- dim(u)[1]


    res <- .Fortran("SB03MD", DICO = dico, JOB = job, FACT = fact, TRANA = trana, N = n, A = a, LDA = lda, U = u, LDU = ldu, C = c, LDC = ldc, SCALE = scale, SEP = sep, FERR = ferr, WR = wr, WI = wi, IWORK = iwork, DWORK = dwork, LDWORK = ldwork,
        INFO = info)

    return(list(scale = res$SCALE, sep = res$SEP, ferr = res$FERR, info = res$INFO, a = res$A, c = res$C, u = res$U, wi = res$WI, wr = res$WR))
}
