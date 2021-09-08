#' mb03rd
#'
#' Reduction of a real Schur form matrix to a block-diagonal form
#' @examples 

#'   To reduce a matrix A in real Schur form to a block-diagonal form
#'   using well-conditioned non-orthogonal similarity transformations.
#'   The condition numbers of the transformations used for reduction
#'   are roughly bounded by PMAX*PMAX, where PMAX is a given value.
#'   The transformations are optionally postmultiplied in a given
#'   matrix X. The real Schur form is optionally ordered, so that
#'   clustered eigenvalues are grouped in the same block.
#' 
#' #'
#' @references \url{http://slicot.org/objects/software/shared/doc/MB03RD.html}
#' @export
mb03rd <- function(jobx, sort, n, pmax, a, x, tol) {

    # In Parameters
    jobx <- as.character(jobx)
    n <- as.integer(n)
    pmax <- as.double(pmax)
    sort <- as.character(sort)
    tol <- as.double(tol)

    lda <- dim(a)[1]
    ldx <- dim(x)[1]
    nblcks <- as.integer(0)
    blsize <- array(as.integer(0), c(n))
    wr <- array(as.double(0), c(n))
    wi <- array(as.double(0), c(n))
    dwork <- array(as.double(1), c(n))
    info <- as.integer(0)


    res <- .Fortran("MB03RD", JOBX = jobx, SORT = sort, N = n, PMAX = pmax, A = a, LDA = lda, X = x, LDX = ldx, NBLCKS = nblcks, BLSIZE = blsize, WR = wr, WI = wi, TOL = tol, DWORK = dwork, INFO = info)

    return(list(nblcks = res$NBLCKS, blsize = res$BLSIZE, wr = res$WR, wi = res$WI, info = res$INFO, a = res$A, x = res$X))
}
