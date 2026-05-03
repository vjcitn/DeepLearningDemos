# v2: 3x3 (Linear -> ReLU -> Scaled ReLU) plotting utilities
#
# Produces a 9-panel 3x3 array of plots over x in [0,2] by default:
#   Row 1: linear functions y = a0[i] + a1[i] * x
#   Row 2: ReLU applied to row 1
#   Row 3: scaled ReLU using b[i]
#
# Also includes a helper that chooses (a0, a1, b) so linear outputs
# stay within y_range on the xlim interval (by constraining endpoints).

choose_params_mostly_in_range <- function(k = 3, seed = NULL,
                                         xlim = c(0, 2),
                                         y_range = c(-1, 1),
                                         b_range = c(0.5, 2.0)) {
  stopifnot(k >= 1, length(xlim) == 2, length(y_range) == 2, xlim[2] > xlim[1])
  if (!is.null(seed)) set.seed(seed)

  L <- xlim[2] - xlim[1]
  ymin <- y_range[1]; ymax <- y_range[2]

  a0 <- numeric(k)
  a1 <- numeric(k)

  for (i in seq_len(k)) {
    # Pick a0 within range
    a0[i] <- runif(1, ymin, ymax)

    # Constrain slope so that y(xlim[2]) also stays in range.
    # Since a line's extrema on an interval occur at endpoints, this
    # implies y(x) stays within range for all x in [xlim[1], xlim[2]].
    a1_min <- (ymin - a0[i]) / L
    a1_max <- (ymax - a0[i]) / L
    a1[i] <- runif(1, a1_min, a1_max)
  }

  # Scaling: keep positive so it stays "ReLU-like".
  b <- runif(k, b_range[1], b_range[2])

  list(a0 = a0, a1 = a1, b = b)
}

plot_3x3_relu_scaled <- function(a0, a1, b,
                                 xlim = c(0, 2),
                                 n = 401,
                                 main_prefix = "",
                                 col = "steelblue",
                                 lwd = 2,
                                 same_ylim_by_row = TRUE) {
  stopifnot(length(a0) == 3, length(a1) == 3, length(b) == 3)

  x <- seq(xlim[1], xlim[2], length.out = n)

  lin  <- function(i) a0[i] + a1[i] * x
  relu <- function(y) pmax(0, y)

  y1 <- lapply(1:3, lin)
  y2 <- lapply(1:3, function(i) relu(lin(i)))
  y3 <- lapply(1:3, function(i) b[i] * relu(lin(i)))

  ylim1 <- range(unlist(y1))
  ylim2 <- range(unlist(y2))
  ylim3 <- range(unlist(y3))

  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)

  par(mfrow = c(3, 3), mar = c(3.5, 3.5, 2.5, 1.0), oma = c(0, 0, 1.5, 0))

  # Row 1: linear
  for (i in 1:3) {
    plot(x, y1[[i]], type = "l", col = col, lwd = lwd,
         xlab = "x", ylab = "y",
         ylim = if (same_ylim_by_row) ylim1 else NULL,
         main = sprintf("%sLinear i=%d: y=%.2f + %.2f x", main_prefix, i, a0[i], a1[i]))
    abline(h = 0, col = "gray80", lty = 2)
    abline(h = c(-1, 1), col = "gray90", lty = 3)
  }

  # Row 2: ReLU(linear)
  for (i in 1:3) {
    plot(x, y2[[i]], type = "l", col = col, lwd = lwd,
         xlab = "x", ylab = "ReLU(y)",
         ylim = if (same_ylim_by_row) ylim2 else NULL,
         main = sprintf("%sReLU i=%d", main_prefix, i))
    abline(h = 0, col = "gray80", lty = 2)
  }

  # Row 3: b_i * ReLU(linear)
  for (i in 1:3) {
    plot(x, y3[[i]], type = "l", col = col, lwd = lwd,
         xlab = "x", ylab = "b_i * ReLU(y)",
         ylim = if (same_ylim_by_row) ylim3 else NULL,
         main = sprintf("%sScaled i=%d: b=%.2f", main_prefix, i, b[i]))
    abline(h = 0, col = "gray80", lty = 2)
  }

  mtext("Linear \u2192 ReLU \u2192 Scaled ReLU (3\u00d73 panels)", outer = TRUE, cex = 1.0)

  invisible(list(
    x = x, a0 = a0, a1 = a1, b = b,
    ylim1 = ylim1, ylim2 = ylim2, ylim3 = ylim3
  ))
}

# Example usage:
# params <- choose_params_mostly_in_range(seed = 1)
# print(params)
# plot_3x3_relu_scaled(params$a0, params$a1, params$b)