#' Plot the CVA biplot
#'
#' @param x Object from CVA
#'
#' @returns
#' @export
#'
plot.biplot <- function(x)
{
  # Only axes
  if(x$n > x$p) axes_tbl <- dplyr::as_tibble(x$ax.one.unit) else
    axes_tbl <- dplyr::as_tibble(x$ax.one.unit_gsvd)

  # Axes with coordinates
  Vr_coords <- axes_coordinates(x)
  for(i in 1:x$p) Vr_coords[[i]] <- cbind(Vr_coords[[i]],var=i)
  Vr_coords <- do.call(rbind, Vr_coords)
  colnames(Vr_coords)[1:3] <- c("V1","V2","tick")
  Vr_coords_tbl <- dplyr::as_tibble(Vr_coords) |>
    dplyr::mutate(var = as.factor(var))

  # Samples
  if(x$n > x$p) samples_tbl <- dplyr::as_tibble(x$XM) |>
    dplyr::mutate(Group = x$group) else
    samples_tbl <- dplyr::as_tibble(x$YM) |> dplyr::mutate(Group = x$group)

  # Plot
  ggplot2::ggplot() +
    ggplot2::geom_line(data = Vr_coords_tbl,
                       ggplot2::aes(x = V1, y = V2,group = var),
                       colour="#d9d9d9") +
    #ggplot2::geom_text(data=Vr_coords_tbl, aes(x=V1,y=V2,label=round(tick,0)),
    #          size=2,colour="black") +
    #ggplot2::geom_point(data=samples_tbl,aes(x=V1,y=V2, group = Group,colour = Group)) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.ticks = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_blank())

}
