#' Plot the CVA biplot
#'
#' @param x Object from CVA
#' @param which.var which variable to display on the biplot
#' @param var.label whether to display label for variable name
#' @param group.col vector of colours for the groups in the data
#' @param zoom.out percentage to zoom out of the plot
#'
#'
#' @export
#' @examples
#' Penguins <- penguins[stats::complete.cases(penguins),]
#' CVAbiplot(X=Penguins[,3:6],group = Penguins[,1]) |>
#' CVAggplot(group.col=c("blue","purple","forestgreen"))
#'
#' simulated_data
#' CVAbiplot(X=sim_data[,2:301],group = sim_data[,1])|>
#' CVAggplot(group.col=c("tan1","darkcyan","darkslateblue"),which.var = 1:10,zoom.out=80)
CVAggplot <- function(x,which.var=1:x$p,var.label=FALSE,group.col=NULL,zoom.out=50)
{

  # Samples
  if(x$n > x$p) samples_tbl <- dplyr::as_tibble(x$XM) |> dplyr::mutate(Group = x$group) else
    samples_tbl <- dplyr::as_tibble(x$YM) |> dplyr::mutate(Group = x$group)

  if(is.null(group.col)) colorScales <- grDevices::rainbow(10) else colorScales <- group.col

  # xlim
  minx <- min(samples_tbl$V1)
  maxx <- max(samples_tbl$V1)
  range_x <- maxx - minx

  # ylim
  miny <- min(samples_tbl$V2)
  maxy <- max(samples_tbl$V2)
  range_y <- maxy - miny

  perc <- zoom.out/100
  xlim <- c(minx - perc*range_x,maxx + perc*range_x)
  ylim <- c(miny - perc*range_y,maxy + perc*range_y)

  if(!is.null(which.var))
  {
    # Axes with coordinates
    axes_info <- axes_coordinates(x,which.var = which.var)
    Vr_coords <- axes_info$z.axes
    for(i in 1:length(Vr_coords)) Vr_coords[[i]] <- cbind(Vr_coords[[i]],var=which.var[i])
    Vr_coords <- do.call(rbind, Vr_coords)
    colnames(Vr_coords)[1:3] <- c("V1","V2","tick")
    Vr_coords_tbl <- dplyr::as_tibble(Vr_coords) |>
      dplyr::mutate(var = as.factor(var))


    Vr_coords_tbl <- Vr_coords_tbl |>
      dplyr::filter((V1 > xlim[1] & V1 < xlim[2])  & (V2 > ylim[1] & V2 < ylim[2]),.by = var)

    # Axes labels
    Vr_labs <-  Vr_coords_tbl |>
      dplyr::filter(tick == max(tick), .by = var) |>
      dplyr::mutate(var.name = colnames(x$X)[which.var]) |>
      dplyr::mutate(slope = sign(axes_info$slope)) |>
      dplyr::mutate(hadj = -slope, vadj = -1)

    # Plot
    ggplot2::ggplot() +
      # Axes lines
      ggplot2::geom_line(data = Vr_coords_tbl,
                         ggplot2::aes(x = V1, y = V2,group = var),colour="#d9d9d9") +
      # Axes labels
      ggplot2::geom_text(data=Vr_labs,
                         ggplot2::aes(x=V1, y=V2,
                                      label = var, # dplyr::if_else(var.label,var.name,var),
                                      hjust=hadj, vjust=vadj,group=var),colour="maroon",size=3) +
      # Axes tick marks
      ggplot2::geom_text(data=Vr_coords_tbl,
                         ggplot2::aes(x=V1,y=V2,label=round(tick,1)),size=2,colour="black") +
      # Samples
      ggplot2::geom_point(data=samples_tbl,
                          ggplot2::aes(x=V1,y=V2, group = Group,colour = Group)) +
      ggplot2::scale_color_manual(name="Class",values=colorScales) +
      # Limits
      xlim(xlim) +
      ylim(ylim) +
      # Theme
      ggplot2::theme_classic() +
      ggplot2::theme(aspect.ratio=1,
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     panel.border = ggplot2::element_rect(colour="black",fill=NA,linewidth = 1))
  } else {


    # Plot
    ggplot2::ggplot() +
      # Samples
      ggplot2::geom_point(data=samples_tbl,
                          ggplot2::aes(x=V1,y=V2, group = Group,colour = Group)) +
      ggplot2::scale_color_manual(name="Class",values=colorScales) +
      # Limits
      xlim(xlim) +
      ylim(ylim) +
      # Theme
      ggplot2::theme_classic() +
      ggplot2::theme(aspect.ratio=1,
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     panel.border = ggplot2::element_rect(colour="black",fill=NA,linewidth = 1))
  }
}
