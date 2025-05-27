#' Provide axes coordinates
#'
#' @param bp Object
#'
#' @returns Axes coordinates
#'
axes_coordinates <- function(bp,which.var=1:bp$p)
{
  slope = c()
  if(bp$p < bp$n)
  {
    z.axes <- lapply(which.var, .calibrate.axis, bp$unscaled.X, bp$means, bp$sd,
                     bp$ax.one.unit,1:bp$p,rep(5,bp$p),
                     rep(0,bp$p), rep(0,bp$p))
    for(i in 1:length(which.var)) slope[i] <- z.axes[[i]][[3]]
    for(i in 1:length(z.axes)) z.axes[[i]]<-z.axes[[i]][[1]]

  } else if (bp$p > bp$n) {
    z.axes <- lapply(which.var, .calibrate.axis, bp$unscaled.X, bp$means, bp$sd,
                     bp$ax.one.unit_gsvd,1:bp$p,rep(5,bp$p),
                     rep(0,bp$p), rep(0,bp$p))
    for(i in 1:length(which.var)) slope[i] <- z.axes[[i]][[3]]
    for(i in 1:length(z.axes)) z.axes[[i]]<-z.axes[[i]][[1]]
  }
  return(list(z.axes=z.axes,slope=slope))
}

#' Calibrate axis
#'
#' @param j j
#' @param Xhat Xhat
#' @param means means
#' @param sd sd
#' @param axes.rows axes.rows
#' @param ax.which ax.which
#' @param ax.tickvec ax.tickvec
#' @param ax.orthogxvec ax.orthogxvec
#' @param ax.orthogyvec ax.orothogyvec
#'
#' @returns Calibrated axes
#'
.calibrate.axis <- function (j, Xhat, means, sd,
                             axes.rows, ax.which, ax.tickvec,
                             ax.orthogxvec, ax.orthogyvec)
{
  ax.num <- ax.which[j]
  tick <- ax.tickvec[j]
  ax.direction <- axes.rows[ax.num,]
  r <- ncol(axes.rows)
  ax.orthog <- rbind(ax.orthogxvec, ax.orthogyvec)
  if (nrow(ax.orthog) < r)    ax.orthog <- rbind(ax.orthog, 0)
  if (nrow(axes.rows) > 1)    phi.vec <- diag(1 / diag(axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, ax.num] else
    phi.vec <- (1 / (axes.rows %*% t(axes.rows))) %*% axes.rows %*% ax.orthog[, ax.num]

  std.ax.tick.label <- pretty(range(Xhat[, ax.num]), n = tick)
  std.range <- range(std.ax.tick.label)
  std.ax.tick.label.min <-  std.ax.tick.label - (std.range[2] - std.range[1])
  std.ax.tick.label.max <-  std.ax.tick.label + (std.range[2] - std.range[1])
  std.ax.tick.label <-  c(std.ax.tick.label,  std.ax.tick.label.min, std.ax.tick.label.max)
  interval <- (std.ax.tick.label - means[ax.num]) / sd[ax.num]
  axis.vals <- sort(unique(interval))

  number.points <- length(axis.vals)
  axis.points <- matrix(0, nrow = number.points, ncol = r)
  for (i in 1:r)
    axis.points[, i] <-  ax.orthog[i, ax.num] + (axis.vals - phi.vec[ax.num]) * ax.direction[i]
  axis.points <- cbind(axis.points, axis.vals * sd[ax.num] + means[ax.num])

  #slope = delta y / delta x of two datapoints
  slope <- (axis.points[1, 2] - axis.points[2, 2]) / (axis.points[1, 1] - axis.points[2, 1])
  #if slope is infinite then all x-values are same
  v <- NULL
  if (is.na(slope)){
    v <- axis.points[1, 1]
    slope = NULL
  } else if (abs(slope) == Inf) {  v <- axis.points[1, 1]
  slope = NULL
  }

  #y=mx+c... c=y-mx
  intercept <- axis.points[1, 2] - slope * axis.points[1, 1]

  details <- list(a = intercept, b = slope, v = v)
  retvals <- list(coords = axis.points, a = intercept, b = slope, v = v)
  return(retvals)
}

