mc01td <- function(dico, dp, p) {

    # In Parameters
    dico <- as.character(dico)
    dp <- as.integer(dp)
    p <- as.double(p)

    # Hidden Parameters
    dwork <-  dwork <-
    # Out Parameters
    stable <- as.logical(0)
    nz <- as.integer(0)
    iwarn <- as.integer(0)
    info <- as.integer(0)

    # Check dimensions of input parameters
    if (dim(as.array(p)) != c(dp + 1))
        stop("Incorrect dimensions for matrix p")

    res <- .Fortran("MC01TD", DICO = dico, DP = dp, P = p, STABLE = stable, NZ = nz, DWORK = dwork, IWARN = iwarn, INFO = info)

    return(list(dp = res$DP, stable = res$STABLE, nz = res$NZ, iwarn = res$IWARN, info = res$INFO))
}
