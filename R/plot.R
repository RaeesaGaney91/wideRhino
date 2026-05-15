#' Plot the CVA biplot
#'
#' @param x Object from CVAgsvd
#' @param which.var which variable to display on the biplot
#' @param var.label whether to display label for variable name
#' @param group.col vector of colours for the groups in the data
#'
#' @returns A CVA biplot based on the GSVD
#' @export
#' @examples
#' data(sim_data)
#' CVAgsvd(X=sim_data[,2:301],group = sim_data[,1])|>
#' CVAbiplot(group.col=c("tan1","darkcyan","darkslateblue"),which.var = 1:10)
CVAbiplot <- function(x,which.var=1:x$p,var.label=TRUE,group.col=NULL)
{

  # Samples
  if(x$n > x$p) samples_tbl <- dplyr::as_tibble(x$XM) |> dplyr::mutate(Group = x$group) else
    samples_tbl <- dplyr::as_tibble(x$YM) |> dplyr::mutate(Group = x$group)

  if(is.null(group.col)) colorScales <- grDevices::rainbow(10) else colorScales <- group.col

  if(!is.null(which.var))
  {
    axes_info <- axes_coordinates(x,which.var = which.var)
    Vr <- x$ax.one.unit_gsvd[which.var,]
    colnames(Vr) <- c("V1","V2")
    Vr <- dplyr::as_tibble(Vr)
    Vr_tbl <- Vr |> dplyr::mutate(var = colnames(x$X)[which.var]) |>
      dplyr::mutate(slope = sign(axes_info$slope)) |>
      dplyr::mutate(hadj = -slope, vadj = -1)

    # Scaling factor
    point_range <- max(
      diff(range(samples_tbl$V1)),
      diff(range(samples_tbl$V2))
    )
    vector_range <- max(
      max(abs(Vr_tbl$V1)),
      max(abs(Vr_tbl$V2))
    )
    scale_factor <- point_range / 2

    Vr_tbl <- Vr_tbl |>
      dplyr::mutate(
        norm  = sqrt(V1^2 + V2^2),      # vector magnitude
        V1s   = (V1 / norm) * scale_factor,  # normalize then scale
        V2s   = (V2 / norm) * scale_factor
      )


    # Compute sample bounds
    x_range <- range(samples_tbl$V1)
    y_range <- range(samples_tbl$V2)

    # Build tick marks tibble, filtered to sample extent
    ticks_tbl <- purrr::imap_dfr(axes_info$z.axes, function(mat, i) {
      colnames(mat) <- c("V1", "V2", "tick")
      dplyr::as_tibble(mat) |>
        dplyr::mutate(
          var  = colnames(x$X)[which.var[i]],
          norm = Vr_tbl$norm[i],
          V1s  = (V1 / norm) * scale_factor,
          V2s  = (V2 / norm) * scale_factor
        ) |>
        # Keep only ticks within the sample point cloud extent
        dplyr::filter(
          V1s >= x_range[1] & V1s <= x_range[2],
          V2s >= y_range[1] & V2s <= y_range[2]
        )
    }) |>
      dplyr::mutate(
        perp_x  = -(V2 / norm),
        perp_y  =  (V1 / norm),
        tick_len = scale_factor * 0.02   # fixed fraction — no longer grows
      )

    # PLOT
    ggplot2::ggplot() +
      # Axes lines
      ggplot2::geom_segment(data = Vr_tbl,
                         ggplot2::aes(x= -V1s, y=-V2s,xend = V1s, yend = V2s),colour="#d9d9d9") +
      # Axes labels
      {if(var.label) {
        ggplot2::geom_text(data=Vr_tbl,
                         ggplot2::aes(x=V1s, y=V2s,
                                      label = var,
                                      hjust="outward", vjust="outward"),
                         colour="maroon",size=3) }
        else {
          ggplot2::geom_text(data=Vr_labs,
                             ggplot2::aes(x=V1s, y=V2s,
                                          label = var,
                                          hjust="outward", vjust="outward",group=var),
                             colour="maroon",size=3)

        }} +
      # Tick marks
      ggplot2::geom_segment(data = ticks_tbl,
                            ggplot2::aes(x = V1s - perp_x * tick_len,
                                         y = V2s - perp_y * tick_len,
                                         xend = V1s + perp_x * tick_len,
                                         yend = V2s + perp_y * tick_len),
                            colour = "#aaaaaa", linewidth = 0.3) +
      # Tick labels
      ggplot2::geom_text(data = ticks_tbl,
                         ggplot2::aes(x = V1s + perp_x * tick_len * 3,
                                      y = V2s + perp_y * tick_len * 3,
                                      label = tick),
                         size = 1.8, colour = "#888888") +
      # Samples
      ggplot2::geom_point(data=samples_tbl,
                          ggplot2::aes(x=V1,y=V2, group = Group,colour = Group)) +
      ggplot2::scale_color_manual(name="Class",values=colorScales) +
      # Limits
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2)) +
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
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2)) +
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
