#' tb01id
#'
#' Balancing a system matrix corresponding to a triplet (A,B,C)
#' @examples 

#'   To reduce the 1-norm of a system matrix
#' 
#'           S =  ( A  B )
#'                ( C  0 )
#' 
#'   corresponding to the triple (A,B,C), by balancing. This involves
#'   a diagonal similarity transformation inv(D)*A*D applied
#'   iteratively to A to make the rows and columns of
#'                         -1
#'                diag(D,I)  * S * diag(D,I)
#' 
#'   as close in norm as possible.
#' 
#'   The balancing can be performed optionally on the following
#'   particular system matrices
#' 
#'            S = A,    S = ( A  B )    or    S = ( A )
#'                                                ( C )
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/TB01ID.html}
#' @export
tb01id <- function(job, n, m, p, maxred, a, b, c) {

    # In Parameters
    job <- as.character(job)
    m <- as.integer(m)
    maxred <- as.double(maxred)
    n <- as.integer(n)
    p <- as.integer(p)

    # Out Parameters
    scale <- array(as.double(0), c(n))
    info <- as.integer(0)

    # Hidden Parameters
    lda <- dim(a)[1]
    ldb <- dim(b)[1]
    ldc <- dim(c)[1]

    res <- .Fortran("TB01ID", JOB = job, N = n, M = m, P = p, MAXRED = maxred, A = a, B = b, C = c, SCALE = scale, INFO = info, LDA = lda, LDB = ldb, LDC = ldc)

    return(list(maxred = res$MAXRED, a = res$A, b = res$B, c = res$C, scale = res$SCALE, info = res$INFO))
}
