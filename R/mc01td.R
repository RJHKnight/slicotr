mc01td <- function(dico, dp, p) {

    # In Parameters
    dico <- as.character(dico)
    dp <- as.integer(dp)

    # Out Parameters
    stable <- as.logical(0)
    nz <- as.integer(0)
    iwarn <- as.integer(0)
    info <- as.integer(0)

    # Hidden Parameters
    dwork <- array(as.double(1), c(2 * dp + 2))

    res <- .Fortran("MC01TD", DICO = dico, DP = dp, P = p, STABLE = stable, NZ = nz, IWARN = iwarn, INFO = info, DWORK = dwork)

    return(list(dp = res$DP, stable = res$STABLE, nz = res$NZ, iwarn = res$IWARN, info = res$INFO))
}
