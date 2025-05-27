#' Plot the CVA biplot
#'
#' @param x Object from CVA
#' @param which.var which variable to display on the biplot.
#' @param group.col vector of colours for the groups in the data
#'
#' @returns
#' @export
#'
CVAggplot <- function(x,which.var=1:x$p,group.col=NULL)
{

  # Axes with coordinates
  axes_info <- axes_coordinates(x,which.var = which.var)
  Vr_coords <- axes_info$z.axes
  for(i in 1:length(Vr_coords)) Vr_coords[[i]] <- cbind(Vr_coords[[i]],var=which.var[i])
  Vr_coords <- do.call(rbind, Vr_coords)
  colnames(Vr_coords)[1:3] <- c("V1","V2","tick")
  Vr_coords_tbl <- dplyr::as_tibble(Vr_coords) |>
    dplyr::mutate(var = as.factor(var))

  # Samples
  if(x$n > x$p) samples_tbl <- dplyr::as_tibble(x$XM) |>
    dplyr::mutate(Group = x$group) else
    samples_tbl <- dplyr::as_tibble(x$YM) |> dplyr::mutate(Group = x$group)

  # Axes labels
  Vr_labs <-  Vr_coords_tbl |>
    filter(tick == max(tick), .by = var) |>
    mutate(var.name = colnames(x$X)[which.var]) |>
    mutate(slope = sign(axes_info$slope)) |>
    mutate(hadj = -slope, vadj = -1)

  # Plot

  if(is.null(group.col)) colorScales <- rainbow(10) else colorScales <- group.col

  ggplot2::ggplot() +
    ggplot2::geom_line(data = Vr_coords_tbl,
                       ggplot2::aes(x = V1, y = V2,group = var),
                       colour="#d9d9d9") +
    #ggplot2::geom_text(data=Vr_coords_tbl,
    #                   aes(x=V1,y=V2,label=round(tick,0)),
    #          size=2,colour="black") +
    geom_text(data=Vr_labs, aes(x=V1, y=V2, label = var, hjust=hadj, vjust=vadj),colour="maroon",size=3) +
    ggplot2::geom_point(data=samples_tbl,
                        aes(x=V1,y=V2, group = Group,colour = Group)) +
    scale_color_manual(name="Class",values=colorScales) +
    ggplot2::theme_classic() +
    ggplot2::theme(aspect.ratio=1,
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.ticks = element_blank(),
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   panel.border = element_rect(colour="black",fill=NA,linewidth = 1))
}
