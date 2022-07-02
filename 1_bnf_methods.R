####################################################################################################
# Replication Files for Kamber, Morley & Wong:
#
# "Intuitive and Reliable Estimates of the Output Gap from a Beveridge-Nelson Filter"
# The Review of Economics and Statistics
#
# R conversion of some of the MATLAB codes originally written by Ben Wong
# benjamin.wong@rbnz.govt.nz
#
####################################################################################################
# MATLAB codes converted to R by Luke Hartigan, 2017
# Additional R codes and wrapper class 'bnf' written by Luke Hartigan, 2017
####################################################################################################

# Helper functions used by the main functions below

# R emulation of MATLAB eye(N)
eye <- function(n)
{
    return (diag(x = 1, nrow = n))
}

# R emulation of MATLAB ones(M, N)
ones <- function(m, n)
{
    return (matrix(data = 1, nrow = m, ncol = n))
}

# R emulation of MATLAB zeros(M, N)
zeros <- function(m, n)
{
    return (matrix(data = 0, nrow = m, ncol = n))
}

# R emulation of MATLAB repmat(A, M, N)
repmat <- function(mat, m, n)
{
    return (kronecker(matrix(data = 1, nrow = m, ncol = n), mat))
}

# Compute the square of the variable 'x'
square <- function(x)
{
    return (x * x)
}

# Returns a suitably transformed, lagged and/or differenced data.
# OPTIONS:
# take_log = take the natural logarithm? Default is no (FALSE)
#
# dcode = option to specify how y is differenced:
#   dcode == "nd" [No difference, i.e., Level] -- Default --
#   dcode == "d1" [1st Difference, i.e., (1 - B)y]
#   dcode == "d4" [4th Difference, i.e., (1 - B^4)y, use with quarterly data]
#   dcode == "d12" [12th Difference, i.e., (1 - B^12)y, use with monthly data]
#
# pcode = option to specify if percentages are computed:
#   pcode == "np" [no change] -- Default --
#   pcode == "p1" [multiply by 100]
#   pcode == "p4" [multiply by 400, annualised quarterly rate]
#   pcode == "p12" [multiply by 1200, annualised monthly rate]
#
# NB: Inspired by the GAUSS procs originally written by Mark Watson:
# http://www.princeton.edu/~mwatson/wp.html
#
transform_series <- function(y, take_log = FALSE, 
    dcode = c("nd", "d1", "d4", "d12"), pcode = c("np", "p1", "p4", "p12"))
{
    # Save 'ts' attributes if 'y' is a 'ts' object
    if (is.ts(y)) {
        y_ts <- TRUE
        endy <- end(y)
        freqy <- frequency(y)
    } else {
        y_ts <- FALSE
    }  

    # Preliminary stuff  
    ty <- as.matrix(y)

    # Log transformation    
    if (take_log) {
        if (any(ty < 0, na.rm = TRUE)) {
            ty[which(ty < 0)] <- NaN
        }
        ty <- log(as.matrix(ty))
    }

    # Difference transform if requested
    dcode <- match.arg(dcode)

    switch (EXPR = dcode,
        "nd" = { ty }, # do nothing
        "d1" = { ty <- diff(x = ty, lag = 1); },
        "d4" = { ty <- diff(x = ty, lag = 4) },
        "d12" = { ty <- diff(x = ty, lag = 12) }
    )

    # Convert to percentage if requested
    pcode <- match.arg(pcode)

    switch (EXPR = pcode,
        "np" = { ty }, # do nothing
        "p1" = { ty <- ty * 100.0; },
        "p4" = { ty <- ty * 400.0 },
        "p12" = { ty <- ty * 1200.0 }
    )

    # Add 'ts' attribute to 'ty' if is a 'ts' object
    if (y_ts) {
        ty <- ts(ty, end = endy, frequency = freqy)
    }

    return (ty)
}

# Demean relative to the break point(s) provided in 'breaks' 
# The values in 'breaks' are assumed to be in ascending order
# NB: Works even if no breaks given, then full sample mean is used instead
piecewise_demean <- function(y, breaks = c())
{
    # Preliminary stuff
    y <- as.matrix(y)
    tobs <- nrow(y)

    # Allocate storage for the demeaned series
    demeaned_y <- matrix(data = NA_real_, nrow = tobs, ncol = 1)
    br <- c(0, breaks, tobs)
    nb <- length(breaks)

    # Number of break points 'nb' should not be greater than T-1
    if (nb > (tobs - 1)) {
        stop(sprintf("Number of break points \'%d\' not feasible with T - 1 = %d\n",
        nb, tobs - 1))
    }

    # Last break point should not be greater than number of observations 'T'
    if (max(breaks) > tobs) {
        stop(sprintf("Largest break point \'%d\' greater than time series length \'%d\'\n",
        breaks[nb], tobs))
    }
    
    for (i in  1:(nb + 1)) {
        j <- br[i] + 1
        k <- br[i + 1]
        demeaned_y[j:k] <- scale(x = y[j:k], center = TRUE, scale = FALSE)
    }
    
    return (demeaned_y)
}

# Demean relative to a backward rolling window 'wind'
rolling_demean <- function(y, wind = 40)
{
    # Preliminary stuff
    y <- as.matrix(y)
    tobs <- nrow(y)
    windp1 <- wind + 1
    windm1 <- wind - 1

    if (wind > tobs) {
        stop(sprintf("Window length \'%d\' greater than time series length \'%d\'\n",
            wind, tobs))
    }

    # Allocate storage for the demeaned series
    demeaned_y <- matrix(data = NA_real_, nrow = tobs, ncol = 1)

    demeaned_y[1:wind] <- y[1:wind] - mean(y[1:wind])

    for (i in windp1:tobs) {
        demeaned_y[i] <- y[i] - mean(y[(i - windm1):i])
    }

    return (demeaned_y)
}

# Estimate an OLS reduced form VAR
# INPUTS:
# y      Data(T x N)
# p      lags
# nc     put TRUE if you don't want a constant (default is FALSE)
#
# OUTPUTS:
# A      VAR Coefficients
# SIGMA  Reduced form variance covariance matrix
# U      Time Series of Residuals
# invXX  Inverse of the symmetric square matrix X'X
# X      Design matrix
olsvar <- function(y, p, nc = FALSE)
{
    # Preliminary stuff
    y <- as.matrix(y)
    tobs <- nrow(y) - p                         # length of time series
    Y <- y[(p + 1):nrow(y), , drop = FALSE]     # cut away first p lags
    
    if (!nc) {
        X <- rep(1, tobs)
    } else {
        X <- c();
    }

    for (i in 1:p) {
        Z <- y[((p + 1) - i):(nrow(y) - i), , drop = FALSE]
        X <- cbind(X, Z)
    }

    # Do the QR decomposition
    qr_xmat <- qr(X)

    # Get the OLS coefficients
    A <- qr.coef(qr = qr_xmat, y = Y)

    # Get the OLS residuals
    U <- qr.resid(qr = qr_xmat, y = Y)
    
    # Get the OLS VCov matrix
    # df is total sample-lags-number of regressors
    SIGMA <- sum(square(U)) / (tobs - nrow(A))

    invXX <- qr.solve(crossprod(x = X, y = NULL))

    # Collect results as a list object
    olsvar <- list()
    olsvar$A <- A
    olsvar$SIGMA <- SIGMA
    olsvar$U <- U
    olsvar$invXX <- invXX
    olsvar$X <- X

    return (olsvar)
}

# Estimate an AR or VAR in Dickey-Fuller form without a constant
#
# INPUTS:
# y      Data(T x N)
# p      lags
# nc     put TRUE if you don't want a constant (default is FALSE)
#
# OUTPUTS:
# A      VAR Coefficients
# SIGMA  Reduced form variance covariance matrix
# U      Time Series of Residuals
# invXX  Inverse of the symmetric square matrix X'X
# X      Design matrix
olsvar_df <- function(y, p, nc = FALSE)
{
    # Preliminary stuff
    y <- as.matrix(y)
    tobs <- nrow(y) - p                         # length of time series
    Y <- y[(p + 1):nrow(y), , drop = FALSE]     # cut away first p lags
    
    if (!nc) {
        X <- rep(1, tobs);
    } else {
        X <- c();
    }

    # Put in first lag on LHS
    Z <- y[p:(nrow(y) - 1), , drop = FALSE]
    X <- cbind(X, Z)
    Y <- Y[2:nrow(Y), , drop = FALSE]
    X <- X[2:nrow(X), , drop = FALSE]

    # Now do second differences
    dy <- diff(y)
    for (i in 1:(p - 1)) {
        Z <- dy[((p + 1) - i):(nrow(dy) - i), , drop = FALSE]
        X <- cbind(X, Z)
    }

    # Do the QR decomposition
    qr_xmat <- qr(X)

    # Get the OLS coefficients
    A <- qr.coef(qr = qr_xmat, y = Y)

    # Get the OLS residuals
    U <- qr.resid(qr = qr_xmat, y = Y)
    
    # Get the OLS VCov matrix
    # df is total sample-lags-number of regressors
    SIGMA <- sum(square(U)) / (tobs - nrow(A));

    invXX <- qr.solve(crossprod(x = X, y = NULL))

    # Collect results as a list object
    olsvar_df <- list()
    olsvar_df$A <- A
    olsvar_df$SIGMA <- SIGMA
    olsvar_df$U <- U
    olsvar_df$invXX <- invXX
    olsvar_df$X <- X

    return (olsvar_df)

}

# Implement the BN Filter imposing the Signal-to-noise ratio 'delta'
#
# Note: This function depends on functions implemented above
#
# INPUTS:
# y           Time series to implement BN Filter in first differences
# p           AR lags
# delta       Signal to noise ratio
#
# OUTPUTS:
# BN_cycle    The estimated cycle
# BN_cycle_se The estimated cycle standard error
# aux_out     Auxiliary output (AR coefficients & residuals )
BN_Filter <- function(y, p, delta, compute_stderr = TRUE)
{
    # Do a few preliminary things
    y <- as.matrix(y)

    # Compute rho from delta
    rho <- -(1.0 - sqrt(delta)) / sqrt(delta)

    # Re-parametrise model and subtract second differences multiplied by 
    # sum of AR coefficients to fix delta. 
    # Adjust the LHS variable to take note of this

    # Backcast data with mean zero
    augmented_y <- rbind(zeros(p + 1, 1), y)

    # Get data matrix from untransformed model. Estimate without a constant
    tmp_olsvar_df <- olsvar_df(y = augmented_y, p = p, nc = TRUE)

    # Get regressors, variance and lags of untransformed AR(p) to perform BN later.
    # Estimate AR by OLS without a constant
    tmp_olsvar <- olsvar(y = rbind(zeros(p, 1), y), p = p, nc = TRUE)
    
    reparameterised_y <- augmented_y[(p + 2):nrow(augmented_y), , drop = FALSE] - 
        rho * tmp_olsvar_df$X[ , 1, drop = FALSE]

    # Calculate the other preliminaries before estimation
    Y <- reparameterised_y
    X <- tmp_olsvar_df$X[ , 2:ncol(tmp_olsvar_df$X), drop = FALSE]

    # Set other parameters for conjugate priors
    A_prior <- zeros(p - 1, 1)     # constant and (p-1) phi_star coefficients
    # Uninformative prior on constant centred on zero
    V_prior <- diag(drop(repmat(0.5, 1, p - 1) / (square(seq(from = 1, to = p - 1, by = 1)))))

    # Calculate Posteriors
    V_post <- qr.solve(qr.solve(V_prior) + (1.0 / tmp_olsvar$SIGMA) * crossprod(x = X, y = NULL))
    A_post <- V_post %*% (qr.solve(V_post) %*% A_prior + t((1.0 / tmp_olsvar$SIGMA) * X) %*% Y)
    
    # Add rho back into the posterior of estimated parameters
    A_post <- rbind(rho, A_post)

    # Map the parameters in DF-form back to AR-form
    AR_params <- zeros(p, 1)

    # Last lag
    AR_params[p] <- -A_post[p]
    rseq <- seq(from = p - 1, to = 2, by = -1)

    for (i in rseq) {
        AR_params[i] <- -A_post[i] - sum(AR_params[i:p])
    }

    # Calculate AR(1) term
    AR_params[1] <- rho - sum(AR_params[2:p])

    # BN Decomposition starts here
    untransformed_X <- tmp_olsvar$X
    X <- rbind(untransformed_X[2:nrow(untransformed_X), , drop = FALSE],
            cbind(y[nrow(y), 1, drop = FALSE], 
                untransformed_X[nrow(untransformed_X), 
                1:(ncol(untransformed_X) - 1), drop = FALSE]))

    # Compute the BN cycle
    Companion <- rbind(t(AR_params), cbind(eye(p - 1), zeros(p - 1, 1)))
    phi <- -Companion %*% qr.solve(eye(p) - Companion)    
    BN_cycle <- phi %*% t(X)
    BN_cycle <- t(BN_cycle[1, , drop = FALSE])

    # Collect results into list object
    result <- list()
    result$BN_cycle <- BN_cycle
    result$aux_out <- list()
    result$aux_out$AR_coeff <- AR_params
    result$aux_out$residuals <- y - untransformed_X %*% AR_params;

    # Compute the BN cycle standard error if specified
    if (compute_stderr) {
        A <- rbind(t(AR_params), cbind(eye(p - 1), zeros(p - 1, 1)))
        big_A <- qr.solve(eye(square(nrow(A))) - (A %x% A))
        var_y <- big_A[1, 1] * tmp_olsvar$SIGMA
        ind_vec <- cbind(1.0, zeros(1, p - 1))
        Phi <- ind_vec %*% (A %*% qr.solve(eye(p) - A)) %*% t(ind_vec)
        BN_cycle_se <- as.numeric(sqrt((square(Phi) * var_y)))
        result$BN_cycle_se <- BN_cycle_se
    }    

    # Return results
    return (result)
}

# Pick the signal to noise ratio 'delta' by doing a grid search 
# from 'd0' in increments of 'dt' to find the first local maximum of
# the amplitude to noise ratio.
#
# Note: This function depends on functions implemented above
#
# INPUTS:
# y          Time series
# p          AR lag order
# d0         Start grid search here, default is 0.01
# dt         Increment grid by this amount, default is d0
#
# OUTPUTS:
# delta_grid
max_amp_to_noise <- function(y, p, d0 = 0.01, dt = d0)
{
    # Initialise the difference to enter the loop
    diff_t <- 1;

    # Initialise the amplitude to noise ratio
    delta_grid <- d0
    tmp <- BN_Filter(y = y, p = p, delta = delta_grid, compute_stderr = FALSE)
    old <- var(tmp$BN_cycle) / mean(square(tmp$aux_out$residuals))

    while (diff_t > 0) {
        delta_grid <- delta_grid + dt
        tmp <- BN_Filter(y = y, p = p, delta = delta_grid, compute_stderr = FALSE)
        new <- var(tmp$BN_cycle) / mean(square(tmp$aux_out$residuals))

        diff_t <- new - old

        if (diff_t > 0) {
            old <- new
        }
    }

    delta_grid <- delta_grid - d0

    return (delta_grid)
}

# bnf using an automatically determined 'delta'
bnf <- function(y, p = 12, d0 = 0.01, dt = d0, demean = c("sm", "pm", "dm"), ...)
{
    # Save 'ts' attributes if 'y' is a 'ts' object
    if (is.ts(y)) {
        y_ts <- TRUE
        endy <- end(y)
        freqy <- frequency(y)
    } else {
        y_ts <- FALSE
    }

    # Do a few preliminary things
    y <- as.matrix(y)

    # Compute the first-difference and then demean y
    dy <- diff(x = y, lag = 1)
    
    # Demean dy
    demean <- match.arg(demean)

    if (demean == "sm") {
        demeaned_dy <- scale(x = dy, center = TRUE, scale = FALSE)
        demean_method <- "Sample mean"
    } else if (demean == "pm") {
        demeaned_dy <- piecewise_demean(y = dy, ...)
        demean_method <- "Piecewise mean"
    } else {
        demeaned_dy <- rolling_demean(y = dy, ...)
        demean_method <- "Rolling mean"
    }

    # Automatically compute delta to pass to BN_Filter
    delta <- max_amp_to_noise(y = demeaned_dy, p = p, d0 = d0, dt = dt)
    tmp <- BN_Filter(y = demeaned_dy, p = p, delta = delta)
    cycle <- tmp$BN_cycle
    colnames(cycle) <- "Cycle"
    cycle_se <- tmp$BN_cycle_se
    
    # Add 'ts' attribute to 'y' and 'cycle' if 'ts' object was originally passed in
    if (y_ts) {
        y <- ts(data = y, end = endy, frequency = freqy)
        cycle <- ts(data = cycle, end = endy, frequency = freqy)
    }

    # Return result invisibly
    result <- list()
    result$call <- match.call()
    result$y <- y
    result$cycle <- cycle
    result$cycle_se <- cycle_se
    result$delta <- delta
    result$demean_method <- demean_method
    class(result) <- "bnf"
    invisible (result)
}

# Print method for class "bnf"
print.bnf <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
    cat("\nCall: ", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")
    
    cat("\n\tIntuitive and Reliable Estimates of the Output Gap from a Beveridge-Nelson Filter\n")
    
    cat(sprintf("\nAutomatic Delta: %2.4f\n", x$delta))

    cat(sprintf("\nDemean method: %s\n", x$demean_method))
    
    cat("\nCycle:\n")
    print(x$cycle)
    
}

# Plot method for class "bnf"
plot.bnf <- function(x,  main = "BN Filter Cycle", 
    plot_ci = TRUE, col = "blue", lwd = 2, ...) 
{
    # All par settings which could be changed
    oldpar <- par(no.readonly = TRUE) 
    
    # Reset plotting device to original settings on exiting
    on.exit(par(oldpar))
   
    # Set up new plotting device
    newpar <- par(mar = c(0, 0, 0, 0), oma = c(4, 4, 3, 2) + 0.1, 
        lty = 1, lwd = 1, xaxs = 'r', yaxs = 'r', mgp = c(3, 1, 0), 
        bty = 'o', tcl = -0.5, cex.main = 1.2, font.main = 2, 
        cex.lab = 1.1, font.lab = 1, cex.axis = 1.1, font.axis = 1)

    # Establish the panel layout -- 2 x 1
    layout(matrix(data = c(1, 2), nrow = 2, ncol = 1))

    # Preliminary values - y
    y_ub <- ceiling(max(x$y))
    y_lb <- floor(min(x$y))

    # Establish the y plotting region
    plot(x$y, type = 'n', col = NA, 
        ylim = c(y_lb, y_ub), yaxt = 'n', xaxt = 'n',
        main = "", ylab = "", xlab = "")
    abline(h = 0, col = 1, lwd = 1, lty = 1)
    lines(x$y, col = 1, lwd = lwd)

    # Mark the LHS y-axis
    axis(side = 2, las = 1)
    
    # Individual title for the sub-panel
    mtext(text = "Level", side = 3, line = -1.5, outer = FALSE, 
        las = 1, cex = 1.1, font = 2)        
    box()

    # Preliminary values - cycle
    if (is.ts(x$cycle)) {
        st <- start(x$cycle)[1] + (start(x$cycle)[2] / 12.0)
        et <- end(x$cycle)[1] + (end(x$cycle)[2] / 12.0)
        x_axis <- seq(from = st + 0.5, to = et + 0.5, length.out = length(x$cycle))
    } else {
        x_axis <- seq(from = 1, to = length(x$cycle), by = 1)
    }
    
    ci <- round(qnorm(p = 0.05 / 2.0, lower.tail = FALSE), 2) * x$cycle_se
    c_ub <- ceiling(ifelse(plot_ci, max(x$cycle) + ci, max(x$cycle)))
    c_lb <- floor(ifelse(plot_ci, min(x$cycle) - ci, min(x$cycle)))

    # Establish the cycle plotting region
    plot(x$cycle, type = 'n', col = NA, 
        ylim = c(c_lb, c_ub), yaxt = 'n', xaxt = 'n',
        main = "", ylab = "", xlab = "")
    abline(h = 0, col = 1, lwd = 1, lty = 1)
    lines(x$cycle, col = col, lwd = lwd)

    # 95% Confidence Intervals (if requested)
    if (plot_ci) { 
        polygon(x = c(x_axis, rev(x_axis)), y = c((x$cycle - ci), rev(x$cycle + ci)),
            col = adjustcolor(col, alpha.f = 0.2), border = NA)
    }

    # Mark the LHS y-axis
    axis(side = 2, las = 1)

    # Mark the x-axis with dates if available
    if (is.ts(x$cycle)) {
        times <- time(x$cycle)
        ticks <- floor(seq(from = times[1], to = times[length(times)], 
            by = ceiling(length(times) / (10 * frequency(x$cycle)))))
        axis(side = 1, at = ticks)
    } else {
        axis(side = 1)
    }    

    # Individual title for the sub-panel
    mtext(text = "Cycle", side = 3, line = -1.5, outer = FALSE, 
        las = 1, cex = 1.1, font = 2)  
    box(which = "plot", col = 1)

    # Main title
    mtext(text = main, side = 3, line = 1, outer = TRUE, 
        las = 1, cex = 1.5, font = 2)

    invisible () # execute par(oldpar)

}

# EOF
