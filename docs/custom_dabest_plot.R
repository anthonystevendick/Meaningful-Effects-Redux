# --- Dependencies ---
library(dabestr)
library(ggplot2)
library(cowplot)

# --- Safe null default operator ---
`%||%` <- function(a, b) if (!is.null(a)) a else b

# --- Custom version of add_swarm_bars_to_raw_plot using geom_point instead of geom_rect ---
custom_add_swarm_bars_to_raw_plot <- function(dabest_effectsize_obj, plot_kwargs, x_values, y_values, 
                                              y_min, main_plot_type) {

  message("✅ custom_add_swarm_bars_to_raw_plot() called")
  stopifnot(length(x_values) == length(y_values))

  params_swarm_bars <- plot_kwargs$params_swarm_bars
  if (is.null(params_swarm_bars)) params_swarm_bars <- list()

  # Default aesthetics
  shape  <- params_swarm_bars$shape  %||% 21
  fill   <- params_swarm_bars$fill   %||% "white"
  color  <- params_swarm_bars$color  %||% "gray"
  size   <- params_swarm_bars$size   %||% 3
  stroke <- params_swarm_bars$stroke %||% 1

  df <- data.frame(x = x_values, y = y_values)

  ggplot2::geom_point(
    data = df,
    mapping = ggplot2::aes(x = x, y = y),
    shape = shape,
    fill = fill,
    color = color,
    size = size,
    stroke = stroke,
    inherit.aes = FALSE,
    show.legend = FALSE
  )
}
# Helper functions that deal with assignment of colour palettes for the overall plots
#
# Contains function `apply_palette`.

# Applies palettes to <ggplot> objects
# TODO add proper documentation.
# ── 1) Modify custom_apply_palette() ────────────────────────────────────────────

custom_apply_palette <- function(ggplot_object, palette_name) {
  message(">> custom_apply_palette() called with palette_name = '", palette_name, "'")
  
  result_plot <- switch(palette_name,
    # ─── existing palettes ──────────────────────────────────────────────────
    "npg"        = ggplot_object + ggsci::scale_color_npg() + ggsci::scale_fill_npg(),
    "aaas"       = ggplot_object + ggsci::scale_color_aaas() + ggsci::scale_fill_aaas(),
    "nejm"       = ggplot_object + ggsci::scale_color_nejm() + ggsci::scale_fill_nejm(),
    "lancet"     = ggplot_object + ggsci::scale_color_lancet() + ggsci::scale_fill_lancet(),
    "jama"       = ggplot_object + ggsci::scale_color_jama() + ggsci::scale_fill_jama(),
    "jco"        = ggplot_object + ggsci::scale_color_jco() + ggsci::scale_fill_jco(),
    "ucscgb"     = ggplot_object + ggsci::scale_color_ucscgb() + ggsci::scale_fill_ucscgb(),
    "d3"         = ggplot_object + ggsci::scale_color_d3() + ggsci::scale_fill_d3(),
    "locuszoom"  = ggplot_object + ggsci::scale_color_locuszoom() + ggsci::scale_fill_locuszoom(),
    "igv"        = ggplot_object + ggsci::scale_color_igv() + ggsci::scale_fill_igv(),
    "cosmic"     = ggplot_object + ggsci::scale_color_cosmic() + ggsci::scale_fill_cosmic(),
    "uchicago"   = ggplot_object + ggsci::scale_color_uchicago() + ggsci::scale_fill_uchicago(),
    "brewer"     = ggplot_object + ggplot2::scale_color_brewer() + ggplot2::scale_fill_brewer(),
    "ordinal"    = ggplot_object + ggplot2::scale_color_ordinal() + ggplot2::scale_fill_ordinal(),
    "viridis_d"  = ggplot_object + ggplot2::scale_color_viridis_d() + ggplot2::scale_fill_viridis_d(),
    
    # ─── Harry Potter Houses ───────────────────────────────────────────────
    "gryffindor" = ggplot_object +
                     ggplot2::scale_color_manual(values = c("#ae0001", "#d3a625")) +
                     ggplot2::scale_fill_manual(values  = c("#ae0001", "#d3a625")),
    
    "slytherin"  = ggplot_object +
                     ggplot2::scale_color_manual(values = c("#2a623d", "#aaaaaa")) +
                     ggplot2::scale_fill_manual(values  = c("#2a623d", "#aaaaaa")),
    
    "ravenclaw"  = ggplot_object +
                     ggplot2::scale_color_manual(values = c("#0e1a40", "#946b2d")) +
                     ggplot2::scale_fill_manual(values  = c("#0e1a40", "#946b2d")),
    
    "hufflepuff" = ggplot_object +
                     ggplot2::scale_color_manual(values = c("#f0c75e", "#000000")) +
                     ggplot2::scale_fill_manual(values  = c("#f0c75e", "#000000")),
    
    # ─── Fallback (any other name) ─────────────────────────────────────────
    ggplot_object
  )
  
  if (is.null(result_plot)) {
    message(">> switch(...) returned NULL – check your palette_name.")
  } else {
    message(">> switch(...) returned a non‐NULL ggplot object.")
  }
  return(result_plot)
}


get_palette_colours <- function(palette_name, num_colours) {
  message(">> get_palette_colours(): palette_name = '", palette_name,
          "', num_colours = ", num_colours)
  
  colours <- switch(palette_name,
    # ─── existing palettes ──────────────────────────────────────────────────
    "npg"        = ggsci::pal_npg()(num_colours),
    "aaas"       = ggsci::pal_aaas()(num_colours),
    "nejm"       = ggsci::pal_nejm()(num_colours),
    "lancet"     = ggsci::pal_lancet()(num_colours),
    "jama"       = ggsci::pal_jama()(num_colours),
    "jco"        = ggsci::pal_jco()(num_colours),
    "ucscgb"     = ggsci::pal_ucscgb()(num_colours),
    "d3"         = ggsci::pal_d3()(num_colours),
    "locuszoom"  = ggsci::pal_locuszoom()(num_colours),
    "igv"        = ggsci::pal_igv()(num_colours),
    "cosmic"     = ggsci::pal_cosmic()(num_colours),
    "uchicago"   = ggsci::pal_uchicago()(num_colours),
    "brewer"     = RColorBrewer::brewer.pal(num_colours, "Set1"),  # or change the brewer palette name
    "ordinal"    = viridisLite::viridis(n = num_colours, option = "viridis"),
    "viridis_d"  = viridisLite::viridis(n = num_colours, option = "viridis"),
    
    # ─── Harry Potter Houses ───────────────────────────────────────────────
    "gryffindor" = rep(c("#ae0001", "#d3a625"), length.out = num_colours),
    "slytherin"  = rep(c("#2a623d", "#aaaaaa"), length.out = num_colours),
    "ravenclaw"  = rep(c("#0e1a40", "#946b2d"), length.out = num_colours),
    "hufflepuff" = rep(c("#f0c75e", "#000000"), length.out = num_colours),
    
    # ─── Fallback ───────────────────────────────────────────────────────────
    rep("#000000", num_colours)
  )
  
  message(">> colours => ", paste(colours, collapse = ", "))
  return(colours)
}



# Helper functions that generates plot components.
#
# Contains functions `create_rawplot_components`, `create_deltaplot_components` and `create_violinplot_components`.

#' Generates list of TRUE/FALSE for raw plot components that will be built
#'
#' This function generates a list of booleans determining whether certain
#' plot components will be constructed for the rawplot.
#'
#' @param proportional Boolean value as initially passed to [load()].
#' @param is_paired Boolean value determining if it is a paired plot.
#' @param float_contrast Boolean value determining which plot will be produced. If TRUE, a
#' Gardner-Altman plot will be produced.If FALSE, a Cumming estimation plot will be produced.
#'
#' @return List of booleans for raw plot components
#'
#' @noRd
create_rawplot_components <- function(proportional,
                                      is_paired,
                                      float_contrast) {
  main_plot_type <- switch(paste(proportional, is_paired),
    "TRUE FALSE" = "unpaired proportions",
    "FALSE FALSE" = "swarmplot",
    "TRUE TRUE" = "sankey",
    "FALSE TRUE" = "slope"
  )

  is_summary_lines <- is_tufte_lines <- TRUE
  if (!float_contrast) {
    is_summary_lines <- FALSE
  }
  if (main_plot_type == "slope") {
    is_tufte_lines <- FALSE
  }
  if (main_plot_type == "sankey") {
    is_summary_lines <- FALSE
  }

  plot_component <- list(
    main_plot_type = main_plot_type,
    is_summary_lines = is_summary_lines,
    is_tufte_lines = is_tufte_lines
  )

  return(plot_component)
}


#' Contains custom <ggproto> geom_objects for plotting.
#'
#' List of geom_*:
#' - `custom_geom_halfviolin`
#' - `custom_geom_bootci`,
#' - `custom_geom_proportionbar`
#' - `custom_geom_sankeyflow`.
#'
#' @importFrom ggplot2 .pt
#' @noRd
# Halfviolin Geom
custom_draw_group_halfviolin <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)

  first_row <- coords[1, , drop = FALSE]

  violin <- grid::polygonGrob(
    x = coords$x,
    y = coords$y,
    gp = grid::gpar(
      col = first_row$colour,
      fill = scales::alpha(first_row$fill, first_row$alpha)
    )
  )
}

# TODO Add documentation
CustomGeomHalfViolin <- ggplot2::ggproto("CustomGeomHalfViolin", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = NA,
    fill = "grey35",
    alpha = 0.8
  ),
  draw_key = ggplot2::draw_key_point,
  draw_group = custom_draw_group_halfviolin
)

custom_geom_halfviolin <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", show.legend = NA,
                            na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = CustomGeomHalfViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# Boot_CI Geom
custom_draw_panel_boot_ci <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)

  ci_line <- grid::segmentsGrob(
    x0 = coords$x,
    x1 = coords$x,
    y0 = coords$ymin,
    y1 = coords$ymax,
    gp = grid::gpar(
      lwd = coords$linesize * .pt,
      lineend = coords$lineend
    )
  )

  ci_dot <- grid::pointsGrob(
    x = coords$x,
    y = coords$middle,
    pch = coords$shape,
    size = grid::unit(coords$dotsize, "char")
  )

  grid::gTree(children = grid::gList(ci_line, ci_dot))
}

# TODO Add documentation
CustomGeomBootCI <- ggplot2::ggproto("CustomGeomBootCI", ggplot2::Geom,
  required_aes = c("x", "ymin", "ymax", "middle"),
  default_aes = ggplot2::aes(
    linesize = 0.8,
    dotsize = 0.5,
    shape = 19,
    lwd = 2,
    lineend = "square"
  ),
  draw_key = ggplot2::draw_key_point,
  draw_panel = custom_draw_panel_boot_ci
)

custom_geom_bootci <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", show.legend = NA,
                        na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = CustomGeomBootCI,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

custom_draw_group_proportion_bar <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)

  first_row <- coords[1, , drop = FALSE]

  failure_bar <- grid::polygonGrob(
    x = coords$x,
    y = coords$y,
    gp = grid::gpar(
      col = first_row$colour,
      fill = scales::alpha(first_row$fill, first_row$alpha)
    )
  )
}

# TODO Add documentation
CustomGeomProportionBar <- ggplot2::ggproto("CustomGeomProportionBar", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = NA,
    fill = "white",
    alpha = NA
  ),
  draw_key = ggplot2::draw_key_polygon,
  draw_group = custom_draw_group_proportion_bar
)

custom_geom_proportionbar <- function(mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               ...,
                               show.legend = NA,
                               na.rm = FALSE,
                               inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = CustomGeomProportionBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# SankeyFlow Geom
custom_draw_group_sankey_flow <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)
  first_row <- coords[1, , drop = FALSE]

  flow <- grid::polygonGrob(
    x = coords$x,
    y = coords$y,
    gp = grid::gpar(
      col = first_row$colour,
      fill = scales::alpha(first_row$fill, first_row$alpha)
    )
  )
}

# TODO Add documentation
CustomGeomSankeyFlow <- ggplot2::ggproto("CustomGeomSankeyFlow", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = NA,
    fill = "gray50",
    alpha = NA
  ),
  draw_key = ggplot2::draw_key_polygon,
  draw_group = custom_draw_group_sankey_flow
)

custom_geom_sankeyflow <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", show.legend = NA,
                            na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = CustomGeomSankeyFlow,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' Contains custom <ggproto> geom_objects for plotting.
#'
#' List of geom_*:
#' - `custom_geom_halfviolin`
#' - `custom_geom_bootci`,
#' - `custom_geom_proportionbar`
#' - `custom_geom_sankeyflow`.
#'
#' @importFrom ggplot2 .pt
#' @noRd
# Halfviolin Geom
custom_draw_group_halfviolin <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)

  first_row <- coords[1, , drop = FALSE]

  violin <- grid::polygonGrob(
    x = coords$x,
    y = coords$y,
    gp = grid::gpar(
      col = first_row$colour,
      fill = scales::alpha(first_row$fill, first_row$alpha)
    )
  )
}

# TODO Add documentation
CustomGeomHalfViolin <- ggplot2::ggproto("CustomGeomHalfViolin", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = NA,
    fill = "grey35",
    alpha = 0.8
  ),
  draw_key = ggplot2::draw_key_point,
  draw_group = custom_draw_group_halfviolin
)

custom_geom_halfviolin <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", show.legend = NA,
                            na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = CustomGeomHalfViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# Boot_CI Geom
custom_draw_panel_boot_ci <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)

  ci_line <- grid::segmentsGrob(
    x0 = coords$x,
    x1 = coords$x,
    y0 = coords$ymin,
    y1 = coords$ymax,
    gp = grid::gpar(
      lwd = coords$linesize * .pt,
      lineend = coords$lineend
    )
  )

  ci_dot <- grid::pointsGrob(
    x = coords$x,
    y = coords$middle,
    pch = coords$shape,
    size = grid::unit(coords$dotsize, "char")
  )

  grid::gTree(children = grid::gList(ci_line, ci_dot))
}

# TODO Add documentation
CustomGeomBootCI <- ggplot2::ggproto("CustomGeomBootCI", ggplot2::Geom,
  required_aes = c("x", "ymin", "ymax", "middle"),
  default_aes = ggplot2::aes(
    linesize = 0.8,
    dotsize = 0.5,
    shape = 19,
    lwd = 2,
    lineend = "square"
  ),
  draw_key = ggplot2::draw_key_point,
  draw_panel = custom_draw_panel_boot_ci
)

custom_geom_bootci <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", show.legend = NA,
                        na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = CustomGeomBootCI,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

custom_draw_group_proportion_bar <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)

  first_row <- coords[1, , drop = FALSE]

  failure_bar <- grid::polygonGrob(
    x = coords$x,
    y = coords$y,
    gp = grid::gpar(
      col = first_row$colour,
      fill = scales::alpha(first_row$fill, first_row$alpha)
    )
  )
}

# TODO Add documentation
CustomGeomProportionBar <- ggplot2::ggproto("CustomGeomProportionBar", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = NA,
    fill = "white",
    alpha = NA
  ),
  draw_key = ggplot2::draw_key_polygon,
  draw_group = custom_draw_group_proportion_bar
)

custom_geom_proportionbar <- function(mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               ...,
                               show.legend = NA,
                               na.rm = FALSE,
                               inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = CustomGeomProportionBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# SankeyFlow Geom
custom_draw_group_sankey_flow <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)
  first_row <- coords[1, , drop = FALSE]

  flow <- grid::polygonGrob(
    x = coords$x,
    y = coords$y,
    gp = grid::gpar(
      col = first_row$colour,
      fill = scales::alpha(first_row$fill, first_row$alpha)
    )
  )
}

# TODO Add documentation
CustomGeomSankeyFlow <- ggplot2::ggproto("CustomGeomSankeyFlow", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = NA,
    fill = "gray50",
    alpha = NA
  ),
  draw_key = ggplot2::draw_key_polygon,
  draw_group = custom_draw_group_sankey_flow
)

custom_geom_sankeyflow <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", show.legend = NA,
                            na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = CustomGeomSankeyFlow,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Generates list of TRUE/FALSE for delta plot components that will be built
#'
#' This function generates a list of booleans determining whether certain
#' plot components will be constructed for the deltaplot.
#'
#' @param proportional Boolean value as initially passed to [load()].
#' @param is_paired Boolean value determining if it is a paired plot.
#' @param float_contrast Boolean value determining which plot will be produced. If TRUE, a
#' Gardner-Altman plot will be produced.If FALSE, a Cumming estimation plot will be produced.
#' @param is_colour Boolean value determining if there is a colour column for the plot.
#' @param delta2 Boolean value determining if delta-delta analysis for
#' 2 by 2 experimental designs is conducted.
#' @param show_zero_dot Boolean value determining if there is a dot on
#' the zero line of the effect size for the control-control group.
#' @param flow Boolean value determining whether the bars will be plotted in pairs.
#' @param show_baseline_ec Boolean value determining whether the baseline curve is shown.
#'
#' @return List of booleans for delta plot components
#' @noRd
create_deltaplot_components <- function(proportional,
                                        is_paired,
                                        float_contrast,
                                        is_colour,
                                        delta2,
                                        show_zero_dot,
                                        flow,
                                        show_baseline_ec) {
  main_violin_type <- if (is_paired || is_colour) "singlecolour" else "multicolour"
  is_summary_lines <- float_contrast
  is_bootci <- TRUE
  is_deltadelta <- delta2
  is_zero_dot <- show_zero_dot && flow && !float_contrast
  is_baseline_ec <- show_baseline_ec

  plot_component <- list(
    main_violin_type = main_violin_type,
    is_summary_lines = is_summary_lines,
    is_bootci = is_bootci,
    is_deltadelta = is_deltadelta,
    is_zero_dot = is_zero_dot,
    is_baseline_ec = is_baseline_ec
  )
  return(plot_component)
}

#' Generates list of values for the violin plot components that will be built
#'
#' This function generates the data and metadata necessary to create a
#' violin plot with specific characteristics
#'
#' @param boots Boot result obtained from boot.ci
#' @param idx List of vectors of control-test groupings that determines the arrangement
#' of the final dataframe output.
#' @param float_contrast Boolean value determining if a Gardner-Altman plot or
#' Cumming estimation plot will be produced.
#' @param delta_y_max Max y limits for the delta-delta plot
#' @param delta_y_min Min y limits for the delta-delta plot
#' @param flow Boolean value determining whether the bars will be plotted in pairs.
#' @param zero_dot Boolean value determining if the zero dot will be constructed.
#'
#' @return List of components essential for the violinplot.
#' @noRd
create_violinplot_components <- function(boots,
                                         idx,
                                         float_contrast,
                                         delta_y_max,
                                         delta_y_min,
                                         flow = TRUE,
                                         zero_dot = TRUE) {
  df_for_violin <- data.frame(x = NA, y = NA, tag = NA)
  x_axis_breaks <- c()
  zero_dot_x_breaks <- c()
  curr_boot_idx <- 1
  curr_x_idx <- 0
  x_axis_scalar <- if (flow) 0 else 0.5

  for (group in idx) {
    curr_x_idx <- curr_x_idx + 1
    if (zero_dot) {
      zero_dot_x_breaks <- c(zero_dot_x_breaks, curr_x_idx)
    }
    temp_df_violin <- data.frame(x = NA, y = NA, tag = toString(curr_x_idx))
    df_for_violin <- rbind(df_for_violin, temp_df_violin)

    for (i in 2:length(group)) {
      curr_x_idx <- curr_x_idx + 1
      x_axis_breaks <- c(x_axis_breaks, curr_x_idx)

      ci_coords <- stats::density(boots[[curr_boot_idx]])
      x_coords_ci <- ci_coords$x
      y_coords_ci <- ci_coords$y

      # Standardise y
      y_coords_ci <- (y_coords_ci - min(y_coords_ci)) / (max(y_coords_ci) - min(y_coords_ci))
      y_coords_ci <- y_coords_ci / 6

      if (!float_contrast) {
        y_coords_ci <- y_coords_ci / 1.5
      }

      y_coords_ci <- y_coords_ci + curr_x_idx - x_axis_scalar

      min_x_coords <- min(x_coords_ci)
      max_x_coords <- max(x_coords_ci)

      # Keeping track of ylim limits
      delta_y_min <- min(min_x_coords, delta_y_min)
      delta_y_max <- max(max_x_coords, delta_y_max)

      temp_df_violin <- data.frame(x = x_coords_ci, y = y_coords_ci, tag = rep(toString(curr_x_idx), 512))
      df_for_violin <- rbind(df_for_violin, temp_df_violin)
      curr_boot_idx <- curr_boot_idx + 1
    }
  }

  df_for_violin <- df_for_violin %>%
    dplyr::arrange(tag, x, y)

  plot_component <- list(
    df_for_violin = df_for_violin,
    delta_y_min = delta_y_min,
    delta_y_max = delta_y_max,
    x_axis_breaks = x_axis_breaks,
    zero_dot_x_breaks = zero_dot_x_breaks
  )

  return(plot_component)
}

# TODO add documentation
add_violinplot_component_to_delta_plot <- function(delta_plot, dabest_effectsize_obj, main_violin_type, flow, float_contrast, zero_dot_x_breaks) {
  baseline_ec_boot_result <- dabest_effectsize_obj$baseline_ec_boot_result
  baseline_boots <- baseline_ec_boot_result$bootstraps

  df_for_baseline_ec_violin <- dabestr:::create_dfs_for_baseline_ec_violin(
    baseline_boots,
    zero_dot_x_breaks,
    float_contrast,
    flow
  )
  if (main_violin_type == "multicolour") {
    delta_plot <- delta_plot +
      custom_geom_halfviolin(
        na.rm = TRUE,
        data = df_for_baseline_ec_violin,
        ggplot2::aes(x = y, y = x, fill = tag)
      )
  } else {
    delta_plot <- delta_plot +
      custom_geom_halfviolin(
        na.rm = TRUE,
        data = df_for_baseline_ec_violin,
        ggplot2::aes(x = y, y = x, group = tag)
      )
  }
  return(delta_plot)
}

# TODO add documentation
add_bootci_component_to_delta_plot <- function(delta_plot, x_axis_breaks, ci_low, ci_high, difference, es_marker_size, es_line_size) {
  delta_plot <- delta_plot +
    custom_geom_bootci(ggplot2::aes(
      x = x_axis_breaks,
      ymin = ci_low,
      ymax = ci_high,
      middle = difference,
      dotsize = es_marker_size,
      linesize = es_line_size
    ))
  return(delta_plot)
}

# TODO add documentation
add_scaling_component_to_delta_plot <- function(delta_plot, float_contrast, boot_result, delta_x_axis_params, delta_y_axis_params, summary_data, plot_kwargs) {
  minimeta <- plot_kwargs$show_mini_meta
  delta2 <- plot_kwargs$show_delta2

  # summary control and test
  control_summary <- summary_data[[1]]
  test_summary <- summary_data[[2]]

  # axis params
  delta_x_max <- delta_x_axis_params[[1]]
  delta_x_labels <- delta_x_axis_params[[2]]
  x_axis_breaks <- delta_x_axis_params[[3]]

  delta_y_min <- delta_y_axis_params[[1]]
  delta_y_max <- delta_y_axis_params[[2]]
  delta_y_mean <- delta_y_axis_params[[3]]
  raw_ylim <- delta_y_axis_params[[4]]

  delta_text_space <- 0
  if (!(float_contrast) && (plot_kwargs$delta_text) && (plot_kwargs$params_delta_text$x_location == "right")) {
    delta_text_space <- 0.4
  }

  min_y_coords <- NULL # only valid for float_contrast

  if (float_contrast) {
    difference <- boot_result$difference

    # Calculate new ylims to align summary lines
    min_raw_y <- raw_ylim[1]
    max_raw_y <- raw_ylim[2]
    raw_y_range <- max_raw_y - min_raw_y
    min_y_coords <- difference / (1 - (test_summary - min_raw_y) / (control_summary - min_raw_y))
    delta_y_range <- raw_y_range * -min_y_coords / (control_summary - min_raw_y)

    delta_plot <- delta_plot +
      ggplot2::theme_classic() +
      ggplot2::coord_cartesian(
        ylim = c(min_y_coords, min_y_coords + delta_y_range),
        xlim = c(1.8, delta_x_max + 0.4 + delta_text_space),
        expand = FALSE,
        clip = "off"
      ) +
      ggplot2::scale_x_continuous(
        breaks = c(2),
        labels = delta_x_labels
      ) +
      ggplot2::scale_y_continuous(position = "right")
  } else {
    delta_x_min <- 0.6
    delta_x_scalar <- 0.3

    # Extend xaxis for minimeta/deltadelta.
    if (minimeta || delta2) {
      delta_x_max <- delta_x_max + 2
    }
    ## Custom contrast_ylim
    delta_ylim <- plot_kwargs$contrast_ylim
    if (!(is.null(delta_ylim))) {
      delta_y_min <- delta_ylim[1]
      delta_y_max <- delta_ylim[2]
      delta_y_mean <- (delta_y_max - delta_y_min) / 2
    }

    delta_plot <- delta_plot +
      ggplot2::theme_classic() +
      ggplot2::coord_cartesian(
        ylim = c(
          delta_y_min - delta_y_mean / 10,
          delta_y_max
        ),
        xlim = c(delta_x_min, delta_x_max + delta_x_scalar + delta_text_space),
        expand = FALSE,
        clip = "off"
      ) +
      ggplot2::scale_x_continuous(
        breaks = x_axis_breaks,
        labels = delta_x_labels
      )
  }
  delta_y_params <- list(min_y_coords, delta_y_min, delta_y_max, delta_y_mean)
  return(list(delta_plot, delta_x_max, delta_y_params))
}


# --- 1. Custom assign_plot_kwargs (unchanged for now) ---
# Helper functions that deal with assignment of plot_kwargs for plot
#
# Contains function `assign_plot_kwargs`.

#' Adjustable Plot Aesthetics
#'
#' @name plot_kwargs
#'
#' @description
#' These are the available plot kwargs for adjusting the plot aesthetics of your
#' estimation plot:
#'
#' - `swarm_label` Default "value" or "proportion of success" for proportion plots.
#' Label for the y-axis of the swarm plot.
#' - `contrast_label` Default "effect size", based on the effect sizes as given in [effect_size()].
#' Label for the y-axis of the contrast plot.
#' - `delta2_label` Default NULL. Label for the y-label for the delta-delta plot.
#' - `swarm_x_text` Default 11. Numeric value determining the font size of the x-axis of the swarm plot.
#' - `swarm_y_text` Default 15. Numeric value determining the font size of the y-axis of the swarm plot.
#' - `contrast_x_text` Default 11. Numeric value determining the font size of the x-axis of the delta plot.
#' - `contrast_y_text` Default 15. Numeric value determining the font size of the y-axis of the delta plot.
#' - `swarm_ylim` Default NULL. Vector containing the y limits for the swarm plot
#' - `contrast_ylim` Default NULL. Vector containing the y limits for the delta plot.
#' - `delta2_ylim` Default NULL. Vector containing the y limits for the delta-delta plot.
#' - `raw_marker_size` Default 1.5. Numeric value determining the size of the points used in the swarm plot.
#' - `tufte_size` Default 0.8. Numeric value determining the size of the tufte line in the swarm plot.
#' - `es_marker_size` Default 0.5. Numeric value determining the size of the points used in the delta plot.
#' - `es_line_size` Default 0.8. Numeric value determining the size of the ci line in the delta plot.
#' - `raw_marker_alpha` Default 1. Numeric value determining the transparency of the points in the swarm plot.
#' - `raw_bar_width` Default 0.3. Numeric value determining the width of the bar in the sankey diagram.
#' - `raw_marker_spread` Default 2. The distance between the points if it is a swarm plot.
#' - `raw_marker_side_shift` Default 0. The horizontal distance that the swarm plot points are moved in the
#' direction of the `asymmetric_side`.
#' - `asymmetric_side` Default "right". Can be either "right" or "left". Controls which side the swarm points are shown.
#' - `show_delta2` Default FALSE. Boolean value determining if the delta-delta plot is shown.
#' - `show_mini_meta` Default FALSE. Boolean value determining if the weighted average plot is shown.
#' If False, the resulting graph would be identical to a multiple two-groups plot.
#' - `show_zero_dot` Default TRUE. Boolean value determining if there is a dot on
#' the zero line of the effect size for the control-control group.
#' - `show_baseline_ec` Default FALSE. Boolean value determining whether the baseline curve is shown.
#' - `show_legend` Default TRUE. If TRUE, legend will be shown. If FALSE, legend will not be shown.
#' - `sankey` Default TRUE. Boolean value determining if the flows between the bar charts will be plotted.
#' - `raw_flow_alpha` Default 0.5. Numeric value determining the transparency of the sankey flows in a
#' paired proportion plot.
#' - `flow` Default TRUE. Boolean value determining whether the bars will be plotted in pairs.
#' - `custom_palette` Default "d3". String. The following palettes are available for use:
#' npg, aaas, nejm, lancet, jama, jco, ucscgb, d3, locuszoom, igv, cosmic, uchicago, brewer, ordinal, viridis_d.
#' - `contrast_bars` Default TRUE. Whether or not to display the contrast bars at the delta plot.
#' - `params_contrast_bars`. Default value: list(color = NULL, alpha = 0.3). Pass relevant keyword arguments to the contrast bars.
#' - `swarm_bars` Default TRUE. Whether or not to display the swarm bars.
#' - `params_swarm_bars`. Default value: list(color = NULL, alpha = 0.3). Pass relevant keyword arguments to the swarm bars.
#'
NULL


#' @param dabest_effectsize_obj dabest_effsize_obj generated after applying effect_size function to dabest_obj
#' @param plot_kwargs viable plot kwargs to alter plot elements being drawn
#'
#' @noRd
#'
custom_assign_plot_kwargs <- function(dabest_effectsize_obj, plot_kwargs) {
  # Set defaults for delta2 and mini_meta if not provided
if (is.null(plot_kwargs$show_mini_meta)) plot_kwargs$show_mini_meta <- FALSE
if (is.null(plot_kwargs$show_delta2)) plot_kwargs$show_delta2 <- FALSE
if (is.null(plot_kwargs$show_zero_dot)) plot_kwargs$show_zero_dot <- FALSE
if (is.null(plot_kwargs$show_baseline_ec)) plot_kwargs$show_baseline_ec <- FALSE
if (is.null(plot_kwargs$flow)) plot_kwargs$flow <- FALSE
  dabestr:::check_effectsize_object(dabest_effectsize_obj)
  custom_palette <- "d3"
  valid_palettes <- c(
  "npg", "aaas", "nejm", "lancet", "jama", "jco", "ucscgb", "d3",
  "locuszoom", "igv", "cosmic", "uchicago", "brewer", "ordinal",
  "viridis_d", "gryffindor", "slytherin", "ravenclaw", "hufflepuff"
)

  swarm_label <- dabest_effectsize_obj$raw_y_labels
  contrast_label <- dabest_effectsize_obj$delta_y_labels
  delta2_label <- NULL

  swarm_ylim <- NULL
  contrast_ylim <- NULL
  delta2_ylim <- NULL

  show_delta2 <- dabest_effectsize_obj$delta2
  show_mini_meta <- dabest_effectsize_obj$minimeta

  asymmetric_side <- "right"
  raw_marker_size <- 1.5
  raw_marker_alpha <- 1
  raw_marker_spread <- 2
  raw_marker_side_shift <- 0
  raw_flow_alpha <- 0.5
  raw_bar_width <- 0.3
  tufte_size <- 0.8
  es_marker_size <- 0.5
  es_line_size <- 0.8

  swarm_y_text <- 15
  swarm_x_text <- 11
  contrast_y_text <- 15
  contrast_x_text <- 11

  show_zero_dot <- TRUE
  show_baseline_ec <- FALSE
  show_legend <- TRUE

  sankey <- TRUE
  flow <- TRUE

  if (!(is.null(plot_kwargs$swarm_label))) {
    swarm_label <- plot_kwargs$swarm_label
  }
  if (!(is.null(plot_kwargs$contrast_label))) {
    contrast_label <- plot_kwargs$contrast_label
  }
  if (!is.null(plot_kwargs$custom_palette)) {
  if (plot_kwargs$custom_palette %in% valid_palettes) {
    custom_palette <- plot_kwargs$custom_palette
  } else {
    warning(paste("❗ Unrecognized custom_palette:", plot_kwargs$custom_palette))
  }
}
  if (!(is.null(plot_kwargs$swarm_ylim))) {
    swarm_ylim <- plot_kwargs$swarm_ylim
  }
  if (!(is.null(plot_kwargs$contrast_ylim))) {
    contrast_ylim <- plot_kwargs$contrast_ylim
  }
  if (!(is.null(plot_kwargs$delta2_ylim))) {
    delta2_ylim <- plot_kwargs$delta2_ylim
  }
  if (!(is.null(plot_kwargs$delta2_label))) {
    delta2_label <- plot_kwargs$delta2_label
  }
  if (!(is.null(plot_kwargs$show_delta2))) {
    show_delta2 <- plot_kwargs$show_delta2
  }
  if (!(is.null(plot_kwargs$show_mini_meta))) {
    show_mini_meta <- plot_kwargs$show_mini_meta
  }
  if (!(is.null(plot_kwargs$raw_marker_size))) {
    raw_marker_size <- plot_kwargs$raw_marker_size
  }
  if (!(is.null(plot_kwargs$raw_marker_alpha))) {
    raw_marker_alpha <- plot_kwargs$raw_marker_alpha
  }
  if (!(is.null(plot_kwargs$raw_marker_side_shift))) {
    raw_marker_side_shift <- plot_kwargs$raw_marker_side_shift
  }
  if (!(is.null(plot_kwargs$tufte_size))) {
    tufte_size <- plot_kwargs$tufte_size
  }
  if (!(is.null(plot_kwargs$es_marker_size))) {
    es_marker_size <- plot_kwargs$es_marker_size
  }
  if (!(is.null(plot_kwargs$es_line_size))) {
    es_line_size <- plot_kwargs$es_line_size
  }
  if (!(is.null(plot_kwargs$raw_bar_width))) {
    raw_bar_width <- plot_kwargs$raw_bar_width
  }
  if (!(is.null(plot_kwargs$raw_marker_spread))) {
    raw_marker_spread <- plot_kwargs$raw_marker_spread
  }
  if (!(is.null(plot_kwargs$sankey))) {
    sankey <- plot_kwargs$sankey
  }
  if (!(is.null(plot_kwargs$flow))) {
    flow <- plot_kwargs$flow
  }
  if (!(is.null(plot_kwargs$raw_flow_alpha))) {
    raw_flow_alpha <- plot_kwargs$raw_flow_alpha
  }
  if (!(is.null(plot_kwargs$swarm_y_text))) {
    swarm_y_text <- plot_kwargs$swarm_y_text
  }
  if (!(is.null(plot_kwargs$swarm_x_text))) {
    swarm_x_text <- plot_kwargs$swarm_x_text
  }
  if (!(is.null(plot_kwargs$contrast_y_text))) {
    contrast_y_text <- plot_kwargs$contrast_y_text
  }
  if (!(is.null(plot_kwargs$contrast_x_text))) {
    contrast_x_text <- plot_kwargs$contrast_x_text
  }
  if (!(is.null(plot_kwargs$show_zero_dot))) {
    show_zero_dot <- plot_kwargs$show_zero_dot
  }
  if (!(is.null(plot_kwargs$show_baseline_ec))) {
    show_baseline_ec <- plot_kwargs$show_baseline_ec
  }
  if (!(is.null(plot_kwargs$show_legend))) {
    show_legend <- plot_kwargs$show_legend
  }
  if (!(is.null(plot_kwargs$asymmetric_side))) {
    asymmetric_side <- plot_kwargs$asymmetric_side
  }
  contrast_bars <- TRUE
  if (!(is.null(plot_kwargs$contrast_bars))) {
    contrast_bars <- plot_kwargs$contrast_bars
  }
  # Swarm bars
  swarm_bars <- TRUE
  if (!(is.null(plot_kwargs$swarm_bars))) {
    swarm_bars <- plot_kwargs$swarm_bars
  }
  # Swarm bars kwargs
  default_params_swarm_bars <- list(
    color = NULL,
    alpha = 0.3
  )
  if (is.null(plot_kwargs$params_swarm_bars)) {
    # If user has not provided params_swarm_bars, use defaults
    params_swarm_bars <- default_params_swarm_bars
  } else {
    # If user has provided params_swarm_bars, update defaults with user values
    params_swarm_bars <- utils::modifyList(
      default_params_swarm_bars,
      plot_kwargs$params_swarm_bars
    )
  }
  # Contrast bars kwargs.
  default_params_contrast_bars <- list(
    color = NULL,
    alpha = 0.3
  )
  if (is.null(plot_kwargs$params_contrast_bars)) {
    # If user has not provided params_contrast_bars, use defaults
    params_contrast_bars <- default_params_contrast_bars
  } else {
    # If user has provided params_contrast_bars,
    # update defaults with user values
    params_contrast_bars <- utils::modifyList(
      default_params_contrast_bars,
      plot_kwargs$params_contrast_bars
    )
  }
  delta_text <- TRUE
  if (!(is.null(plot_kwargs$delta_text))) {
    delta_text <- plot_kwargs$delta_text
  }
  delta_dots <- TRUE
  if (!(is.null(plot_kwargs$delta_dots))) {
    delta_dots <- plot_kwargs$delta_dots
  }

  # Delta dots kwargs.
  default_params_delta_dots <- list(
    "pch" = 17, # default dot symbol is triangle
    "alpha" = 0.5,
    "cex" = 2,
    "size" = 2, # size 3 is too big
    "side" = "right"
  )
  if (is.null(plot_kwargs$params_delta_dots)) {
    params_delta_dots <- default_params_delta_dots
  } else {
    params_delta_dots <- utils::modifyList(
      default_params_delta_dots,
      plot_kwargs$params_delta_dots
    )
  }
  # Delta text kwargs.
  default_params_delta_text <- list(
    "color" = NULL,
    "alpha" = 1,
    "fontsize" = 10,
    "ha" = "center", # hjust
    "va" = "center", # vjust
    "rotation" = 0,
    "x_location" = "right",
    "x_coordinates" = NULL,
    "y_coordinates" = NULL,
    "x_adjust" = 0
  )
  if (is.null(plot_kwargs$params_delta_text)) {
    params_delta_text <- default_params_delta_text
  } else {
    params_delta_text <- utils::modifyList(
      default_params_delta_text,
      plot_kwargs$params_delta_text
    )
  }

  return(list(
    swarm_label = swarm_label,
    contrast_label = contrast_label,
    custom_palette = custom_palette,
    swarm_ylim = swarm_ylim,
    contrast_ylim = contrast_ylim,
    delta2_ylim = delta2_ylim,
    delta2_label = delta2_label,
    show_delta2 = show_delta2,
    show_mini_meta = show_mini_meta,
    raw_marker_size = raw_marker_size,
    raw_marker_alpha = raw_marker_alpha,
    raw_marker_spread = raw_marker_spread,
    raw_marker_side_shift = raw_marker_side_shift,
    raw_bar_width = raw_bar_width,
    tufte_size = tufte_size,
    es_marker_size = es_marker_size,
    es_line_size = es_line_size,
    sankey = sankey,
    flow = flow,
    raw_flow_alpha = raw_flow_alpha,
    swarm_y_text = swarm_y_text,
    swarm_x_text = swarm_x_text,
    contrast_y_text = contrast_y_text,
    contrast_x_text = contrast_x_text,
    show_zero_dot = show_zero_dot,
    show_baseline_ec = show_baseline_ec,
    show_legend = show_legend,
    asymmetric_side = asymmetric_side,
    contrast_bars = contrast_bars,
    params_contrast_bars = params_contrast_bars,
    swarm_bars = swarm_bars,
    params_swarm_bars = params_swarm_bars,
    delta_text = delta_text,
    params_delta_text = params_delta_text,
    delta_dots = delta_dots,
    params_delta_dots = params_delta_dots
  ))
}



# --- 2. Custom plot_raw with open circle mean markers (with CI‐aligned dots) ---
custom_plot_raw <- function(dabest_effectsize_obj, float_contrast = TRUE, ...) {

  print(">>> custom_plot_raw() STARTED")

  # 1) Capture any user‐supplied plot_kwargs
  plot_kwargs <- list(...)
  if (is.null(plot_kwargs$show_mini_meta))   plot_kwargs$show_mini_meta   <- FALSE
  if (is.null(plot_kwargs$show_delta2))      plot_kwargs$show_delta2      <- FALSE
  if (is.null(plot_kwargs$show_zero_dot))    plot_kwargs$show_zero_dot    <- FALSE
  if (is.null(plot_kwargs$show_baseline_ec)) plot_kwargs$show_baseline_ec <- FALSE
  if (is.null(plot_kwargs$flow))             plot_kwargs$flow             <- FALSE

  # 2) Call dabestr’s original raw plot and check the object
  raw_plot <- dabestr:::plot_raw(dabest_effectsize_obj, float_contrast, plot_kwargs)
  dabestr:::check_effectsize_object(dabest_effectsize_obj)

  # ─── STEP 1: Print a summary of the raw data ────────────────────────────────────
  raw_data <- dabest_effectsize_obj$raw_data
  print(">>> head(raw_data):")
  print(utils::head(raw_data))

  # 3) Pull out everything needed from dabestr object
  enquo_x      <- dabest_effectsize_obj$enquo_x   # grouping variable
  enquo_y      <- dabest_effectsize_obj$enquo_y   # numeric response
  raw_data     <- dabest_effectsize_obj$raw_data
  proportional <- dabest_effectsize_obj$proportional
  is_paired    <- dabest_effectsize_obj$is_paired
  is_colour    <- dabest_effectsize_obj$is_colour
  idx          <- dabest_effectsize_obj$idx

  # y‐range for raw plot (used for baseline/text placement)
  raw_y_range_vector <- dabest_effectsize_obj$ylim
  raw_y_min   <- raw_y_range_vector[1]
  raw_y_range <- diff(raw_y_range_vector)

  # 4) Determine what type of raw plot we are drawing
  plot_components  <- create_rawplot_components(proportional, is_paired, float_contrast)
  main_plot_type   <- plot_components$main_plot_type
  is_summary_lines <- plot_components$is_summary_lines
  is_tufte_lines   <- plot_components$is_tufte_lines

  # 5) Prepare sankey or proportion‐bar data if needed
  sankey_df            <- NULL
  sankey_bars          <- NULL
  df_for_proportion_bar <- NULL

  if (main_plot_type == "sankey") {
    if (!plot_kwargs$flow) {
      separated_idx <- dabestr:::separate_idx(idx, is_paired)
      raw_x_max     <- length(unlist(separated_idx))
      x_axis_raw    <- seq(2, raw_x_max * 2, 2)/2
      is_tufte_lines <- TRUE
    }
    sankey_bar_gap <- 0.025
    sankey_df <- dabestr:::create_dfs_for_sankey(
      float_contrast    = float_contrast,
      raw_data          = raw_data,
      proportional_data = dabest_effectsize_obj$proportional_data,
      enquo_x           = enquo_x,
      enquo_y           = enquo_y,
      enquo_id_col      = dabest_effectsize_obj$enquo_id_col,
      gap               = sankey_bar_gap,
      sankey            = plot_kwargs$sankey,
      idx               = idx,
      flow              = plot_kwargs$flow,
      N                 = dabest_effectsize_obj$Ns$n[1]
    )
    sankey_bars <- dabestr:::create_dfs_for_proportion_bar(
      sankey_df$sankey_bars$proportion_success,
      bar_width = plot_kwargs$raw_bar_width,
      gap       = sankey_bar_gap
    )
  }

  if (main_plot_type == "unpaired proportions") {
    if (float_contrast) {
      raw_y_max <- 1
      raw_y_min <- 0
    }
    df_for_proportion_bar <- dabestr:::create_dfs_for_proportion_bar(
      dabest_effectsize_obj$proportional_data$proportion_success,
      bar_width = plot_kwargs$raw_bar_width
    )
  }

  # 6) Initialize the “clean” raw_plot (drops many dabestr geoms, sets margins, etc.)
  output <- dabestr:::initialize_raw_plot(
    plot_kwargs           = plot_kwargs,
    plot_components       = plot_components,
    dabest_effectsize_obj = dabest_effectsize_obj,
    df_for_proportion_bar = df_for_proportion_bar,
    sankey_df             = sankey_df,
    sankey_bars           = sankey_bars,
    idx                   = idx,
    float_contrast        = float_contrast
  )

  raw_plot    <- output[[1]]
  raw_y_range <- output[[2]]
  raw_y_min   <- output[[3]]
  x_axis_raw  <- output[[4]]

  # 7) Remove dabestr’s rectangles if requested
  if (!isTRUE(plot_kwargs$swarm_bars)) {
    raw_plot$layers <- raw_plot$layers[
      !vapply(raw_plot$layers, function(L) inherits(L$geom, "GeomRect"), logical(1))
    ]
    print(">>> Removed GeomRect layers (swarm_bars = FALSE)")
  }

  # 8) If Tufte lines are requested, compute and draw them
  if (is_tufte_lines) {
    if (main_plot_type == "sankey") {
      tufte_gap_value <- sankey_bar_gap
      tufte_lines_df <- dabestr:::create_df_for_tufte(
        raw_data       = raw_data,
        enquo_x        = enquo_x,
        enquo_y        = enquo_y,
        proportional   = proportional,
        gap            = tufte_gap_value,
        effsize_type   = dabest_effectsize_obj$delta_y_labels
      )
      if (!plot_kwargs$flow) {
        tufte_lines_df <- dabestr:::create_dfs_for_nonflow_tufte_lines(
          tufte_lines_df, idx = dabestr:::separate_idx(idx, is_paired), enquo_x = enquo_x
        )
      }
    } else {
      tufte_lines_df <- dabestr:::create_df_for_tufte(
        raw_data       = raw_data,
        enquo_x        = enquo_x,
        enquo_y        = enquo_y,
        proportional   = proportional,
        gap            = 0,
        effsize_type   = dabest_effectsize_obj$delta_y_labels
      )
      tufte_gap_value <- ifelse(
        proportional,
        min(tufte_lines_df$mean)/20,
        raw_y_range/70
      )
      tufte_lines_df <- dabestr:::create_df_for_tufte(
        raw_data       = raw_data,
        enquo_x        = enquo_x,
        enquo_y        = enquo_y,
        proportional   = proportional,
        gap            = tufte_gap_value,
        effsize_type   = dabest_effectsize_obj$delta_y_labels
      )
    }

    tufte_side_adjust_value <- ifelse(proportional, 0, 0.05)
    row_num <- max(x_axis_raw)
    row_ref <- seq_len(row_num) +
      tufte_side_adjust_value * ifelse(plot_kwargs$asymmetric_side == "right", -1, 1) +
      tufte_side_adjust_value * plot_kwargs$raw_marker_side_shift

    if (!plot_kwargs$flow) {
      row_ref <- seq_len(length(unlist(idx))) +
        tufte_side_adjust_value * ifelse(plot_kwargs$asymmetric_side == "right", -1, 1) +
        tufte_side_adjust_value * plot_kwargs$raw_marker_side_shift
    }

    if (proportional || is_colour) {
      raw_plot <- raw_plot +
        ggplot2::geom_segment(
          data      = tufte_lines_df,
          linewidth = plot_kwargs$tufte_size,
          colour    = "black",
          ggplot2::aes(
            x    = row_ref,
            xend = row_ref,
            y    = y_top_start,
            yend = y_top_end,
            colour = !!enquo_x
          ),
          lineend = "square"
        ) +
        ggplot2::geom_segment(
          data      = tufte_lines_df,
          linewidth = plot_kwargs$tufte_size,
          colour    = "black",
          ggplot2::aes(
            x    = row_ref,
            xend = row_ref,
            y    = y_bot_start,
            yend = y_bot_end,
            colour = !!enquo_x
          ),
          lineend = "square"
        )
    } else {
      raw_plot <- raw_plot +
        ggplot2::geom_segment(
          data      = tufte_lines_df,
          linewidth = plot_kwargs$tufte_size,
          ggplot2::aes(
            x    = row_ref,
            xend = row_ref,
            y    = y_top_start,
            yend = y_top_end,
            colour = !!enquo_x
          ),
          lineend = "square"
        ) +
        ggplot2::geom_segment(
          data      = tufte_lines_df,
          linewidth = plot_kwargs$tufte_size,
          ggplot2::aes(
            x    = row_ref,
            xend = row_ref,
            y    = y_bot_start,
            yend = y_bot_end,
            colour = !!enquo_x
          ),
          lineend = "square"
        )
    }
  }

  # 9) Draw the “baseline” horizontal axis
  if (float_contrast) {
    raw_x_min <- 0.6
    raw_plot <- raw_plot +
      dabestr:::float_contrast_theme +
      ggplot2::geom_segment(
        linewidth = 0.4,
        color     = "black",
        ggplot2::aes(
          x    = raw_x_min,
          xend = length(unlist(idx)) + 0.2,
          y    = raw_y_min,
          yend = raw_y_min
        )
      )
  } else {
    if (main_plot_type == "sankey" && !plot_kwargs$flow) {
      idx_for_xaxis_redraw <- dabestr:::remove_last_ele_from_nested_list(idx)
      dfs_for_xaxis_redraw <- dabestr:::create_dfs_for_xaxis_redraw(idx_for_xaxis_redraw)
      df_for_line <- dfs_for_xaxis_redraw$df_for_line %>%
        dplyr::mutate(
          x    = x + 0.5 + (x - 1),
          xend = xend + 0.5 + (xend - 1)
        )
      df_for_ticks <- dfs_for_xaxis_redraw$df_for_ticks %>%
        dplyr::mutate(x = x + 0.5 + (x - 1))
    } else {
      idx_for_xaxis_redraw <- idx
      dfs_for_xaxis_redraw <- create_dfs_for_xaxis_redraw(idx_for_xaxis_redraw)
      df_for_line  <- dfs_for_xaxis_redraw$df_for_line
      df_for_ticks <- dfs_for_xaxis_redraw$df_for_ticks
    }

    raw_plot <- raw_plot +
      dabestr:::non_float_contrast_theme +
      ggplot2::geom_segment(
        data      = df_for_line,
        linewidth = 0.5,
        lineend   = "square",
        color     = "black",
        ggplot2::aes(
          x    = x,
          xend = xend,
          y    = raw_y_min + raw_y_range/40,
          yend = raw_y_min + raw_y_range/40
        )
      ) +
      ggplot2::geom_segment(
        data      = df_for_ticks,
        linewidth = 0.5,
        lineend   = "square",
        color     = "black",
        ggplot2::aes(
          x    = x,
          xend = x,
          y    = raw_y_min + raw_y_range/40,
          yend = raw_y_min
        )
      )
  }

  # 10) Add y‐axis label and adjust font sizes
  raw_plot <- raw_plot +
    ggplot2::labs(y = plot_kwargs$swarm_label) +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(size = plot_kwargs$swarm_x_text),
      axis.title.y = ggplot2::element_text(size = plot_kwargs$swarm_y_text)
    )

  print(">>> Computing group means (with CIs) for each level of x:")
  # ─── Compute mean ± 95% CI for each group ───────────────────────────────────────
  mean_df <- raw_data %>%
  dplyr::group_by(!!enquo_x) %>%
  dplyr::summarize(
    mean_value = mean(!!enquo_y, na.rm = TRUE),
    n          = sum(!is.na(!!enquo_y)),
    sd         = sd(!!enquo_y,   na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  dplyr::mutate(
    se      = sd / sqrt(n),
    ci_low  = mean_value - qt(0.975, df = n - 1) * se,
    ci_high = mean_value + qt(0.975, df = n - 1) * se,
    x_numeric = row_ref        # ← place means exactly between Tufte bars
  )


  print(mean_df)  # show mean, n, sd, CI, and numeric x position

  # ─── Add a vertical CI bar at each x_numeric ──────────────────────────────────
  #raw_plot <- raw_plot +
  #  ggplot2::geom_errorbar(
  #    data    = mean_df,
  #    mapping = ggplot2::aes(
  #      x    = x_numeric,
  #      ymin = ci_low,
  #      ymax = ci_high
  #    ),
  #    width = 0.1,  # half‐width of horizontal “caps”
  #    size  = 0.8,  # thickness of the CI line
  #    color = "black",
  #    inherit.aes = FALSE
  #  )

  # ─── Overlay a hollow‐circle at exactly the same x_numeric (the mean) ─────────
  raw_plot <- raw_plot +
    ggplot2::geom_point(
      data    = mean_df,
      mapping = ggplot2::aes(x = x_numeric, y = mean_value),
      shape   = 21,              # hollow circle
      fill    = "red",           # fill color inside the circle
      color   = "black",         # border of the circle
      size    = plot_kwargs$raw_marker_size + 2,
      stroke  = 1,
      inherit.aes = FALSE
    )

  print(">>> Added CI bars + aligned mean markers")

  # 11) Return the customized raw plot
  print(">>> custom_plot_raw() COMPLETED")
  return(raw_plot)
}



# --- 3. Placeholder for custom_plot_delta (unchanged for now) ---
#' Generates a ggplot object containing plot components for the deltaplot component
#' of an estimation plot.
#'
#' This function takes in a dabest_effectsize_obj object and applies the [create_deltaplot_components()]
#' function on the object. Plot components for the deltaplot are then produced and returned in the
#' form of a ggplot object.
#'
#' @param dabest_effectsize_obj A dabest_effectsize_obj created by loading in a
#' dabest_obj along with other specified parameters with the [effect_size()] function.
#' @param float_contrast Boolean. If TRUE, a Gardner-Altman plot will be produced.
#' If FALSE, a Cumming estimation plot will be produced.
#' @param plot_kwargs Adjustment parameters to control and adjust the appearance of the plot.
#' (list of all possible adjustment parameters can be found under [plot_kwargs])
#'
#' @return ggplot object containing plot components for the deltaplot.
#' @noRd

#' Generates a ggplot object containing plot components for the deltaplot component
#' of an estimation plot.
#'
#' This function takes in a dabest_effectsize_obj object and applies the [create_deltaplot_components()]
#' function on the object. Plot components for the deltaplot are then produced and returned in the
#' form of a ggplot object.
#'
#' @param dabest_effectsize_obj A dabest_effectsize_obj created by loading in a
#' dabest_obj along with other specified parameters with the [effect_size()] function.
#' @param float_contrast Boolean. If TRUE, a Gardner-Altman plot will be produced.
#' If FALSE, a Cumming estimation plot will be produced.
#' @param plot_kwargs Adjustment parameters to control and adjust the appearance of the plot.
#' (list of all possible adjustment parameters can be found under [plot_kwargs])
#'
#' @return ggplot object containing plot components for the deltaplot.
#' @noRdcustom_plot_delta <- function(dabest_effectsize_obj, float_contrast = TRUE, plot_kwargs) {

custom_plot_delta <- function(dabest_effectsize_obj, float_contrast = TRUE, plot_kwargs) {
  # --- FORCE contrast_bars = FALSE so dabestr’s default Δ‐distribution bars are suppressed --- #
  plot_kwargs$contrast_bars <- FALSE

  idx            <- dabest_effectsize_obj$idx
  separated_idx  <- idx
  proportional   <- dabest_effectsize_obj$proportional
  paired         <- dabest_effectsize_obj$paired

  delta_x_labels <- unlist(dabest_effectsize_obj$delta_x_labels)
  delta_y_labels <- plot_kwargs$contrast_label

  minimeta       <- plot_kwargs$show_mini_meta
  delta2         <- plot_kwargs$show_delta2

  is_colour      <- dabest_effectsize_obj$is_colour
  is_paired      <- dabest_effectsize_obj$is_paired

  raw_y_range_vector <- dabest_effectsize_obj$ylim
  raw_y_max          <- raw_y_range_vector[2]
  raw_y_min          <- raw_y_range_vector[1]

  control_summary <- dabest_effectsize_obj$control_summary
  test_summary    <- dabest_effectsize_obj$test_summary

  # Initialising x & y limits
  delta_x_max <- length(unlist(idx))
  delta_y_min <- .Machine$double.xmax
  delta_y_max <- .Machine$double.xmin

  # Obtain boot
  boot_result <- dabest_effectsize_obj$boot_result
  boots       <- boot_result$bootstraps

  # If multiple contrasts, fall back to Cumming plot
  if (length(unlist(idx)) >= 3) {
    float_contrast <- FALSE
  }

  #### Load in sizes of plot elements ####
  tufte_size       <- plot_kwargs$tufte_size
  es_marker_size   <- plot_kwargs$es_marker_size
  es_line_size     <- plot_kwargs$es_line_size
  flow             <- plot_kwargs$flow
  contrast_x_text  <- plot_kwargs$contrast_x_text
  contrast_y_text  <- plot_kwargs$contrast_y_text
  show_zero_dot    <- plot_kwargs$show_zero_dot
  show_baseline_ec <- plot_kwargs$show_baseline_ec

  #### Build “delta_plot_components” ####
  delta_plot_components <- create_deltaplot_components(
    proportional,
    is_paired,
    float_contrast,
    is_colour,
    delta2,
    show_zero_dot,
    flow,
    show_baseline_ec
  )
  main_violin_type <- delta_plot_components$main_violin_type
  is_summary_lines <- delta_plot_components$is_summary_lines
  is_bootci       <- delta_plot_components$is_bootci
  is_deltadelta   <- delta_plot_components$is_deltadelta
  is_zero_dot     <- delta_plot_components$is_zero_dot
  is_baseline_ec  <- delta_plot_components$is_baseline_ec

  raw_plot_components <- create_rawplot_components(proportional, is_paired, float_contrast)
  main_plot_type      <- raw_plot_components$main_plot_type

  #### Initialise the ggplot canvas ####
  if (minimeta || delta2) {
    # If showing mini‐meta or ΔΔ, tack on an extra “dummy” column.
    separated_idx <- c(separated_idx, list(c("minimeta", "deltadelta")))
    idx <- separated_idx
  }
  if (main_plot_type == "sankey" && !flow) {
    separated_idx <- dabestr:::separate_idx(idx, paired)
    delta_x_max   <- length(unlist(separated_idx))
    is_tufte_lines <- FALSE
  }

  violin_plot_components <- create_violinplot_components(
    boots,
    separated_idx,
    float_contrast,
    delta_y_max,
    delta_y_min,
    flow,
    show_zero_dot
  )

  df_for_violin     <- violin_plot_components$df_for_violin
  delta_y_min       <- violin_plot_components$delta_y_min
  delta_y_max       <- violin_plot_components$delta_y_max
  delta_y_mean      <- (delta_y_max - delta_y_min) / 2
  x_axis_breaks     <- violin_plot_components$x_axis_breaks
  zero_dot_x_breaks <- violin_plot_components$zero_dot_x_breaks

  if (main_plot_type == "sankey" && !flow) {
    x_axis_breaks <- x_axis_breaks - 0.5
  }

  delta_plot <- switch(main_violin_type,
    "multicolour" =
      ggplot2::ggplot() +
        custom_geom_halfviolin(
          na.rm = TRUE,
          data = df_for_violin,
          ggplot2::aes(x = y, y = x, fill = tag)
        ),
    "singlecolour" =
      ggplot2::ggplot() +
        custom_geom_halfviolin(
          na.rm = TRUE,
          data = df_for_violin,
          ggplot2::aes(x = y, y = x, group = tag)
        )
  )

  ## Add extra Δ labels ##
  if (minimeta) {
    delta_x_labels <- append(delta_x_labels, "Weighted\nDelta")
  }
  if (delta2) {
    delta_x_labels <- append(delta_x_labels, "delta-delta")
  }

  #### Add scaling (axes & theme) ####
  raw_ylim <- plot_kwargs$swarm_ylim
  raw_ylim <- if (is.null(raw_ylim)) c(raw_y_min, raw_y_max) else raw_ylim

  delta_dots      <- plot_kwargs$delta_dots
  show_delta_dots <- (is_paired && !proportional && delta_dots)
  if (show_delta_dots) {
    delta_dots_data <- dabestr:::create_delta_dots_data(dabest_effectsize_obj, x_axis_breaks)
    delta_y_min     <- min(delta_dots_data$y_var)
    delta_y_max     <- max(delta_dots_data$y_var)
  }
  summary_data        <- list(control_summary, test_summary)
  delta_x_axis_params <- list(delta_x_max, delta_x_labels, x_axis_breaks)
  delta_y_axis_params <- list(delta_y_min, delta_y_max, delta_y_mean, raw_ylim)

  output <- dabestr:::add_scaling_component_to_delta_plot(
    delta_plot,
    float_contrast,
    boot_result,
    delta_x_axis_params,
    delta_y_axis_params,
    summary_data,
    plot_kwargs
  )
  delta_plot     <- output[[1]]
  delta_x_max    <- output[[2]]
  delta_y_params <- output[[3]]
  min_y_coords   <- delta_y_params[[1]]
  delta_y_min    <- delta_y_params[[2]]
  delta_y_max    <- delta_y_params[[3]]
  delta_y_mean   <- delta_y_params[[4]]

  # ————————————————————————————————————————————————
  #  At this point, dabestr has drawn its “default” Δ‐violin in red.
  #  We want to remove that “default” layer and replace it with our own.
  #
  #  For a simple two‐group design, that red half‐violin always lives in layer 3.
  #  (You can verify by printing `delta_plot$layers[[3]]` if you like.)
  #
  #  So here we simply drop layer 3 before adding our own triangle + black half‐violin.
  # ————————————————————————————————————————————————
  # — remove any dabestr‐added (red) half‐violin, no matter whether it’s a GeomViolin or CustomGeomHalfViolin:
  delta_plot$layers <- delta_plot$layers[
    !vapply(delta_plot$layers, function(L) {
      inherits(L$geom,   "GeomViolin") ||
      inherits(L$geom,  "CustomGeomHalfViolin")
    }, logical(1))
  ]

  #### Now, re‐fetch the Δ bootstrap data and draw our own “vertical CI + triangle + black half‐violin” ####
  if (delta2 != dabest_effectsize_obj$delta2 || minimeta != dabest_effectsize_obj$minimeta) {
    # If a ΔΔ or weighted‐mean row was appended, drop that final bootstrap row
    boot_result <- boot_result[-nrow(boot_result), ]
  }
  ci_low     <- boot_result$bca_ci_low
  ci_high    <- boot_result$bca_ci_high
  difference <- boot_result$difference

  # ---- Debug prints (optional) ----
  print(">>> Entering Add bootci Component")
  print(paste0("   is_bootci  = ", is_bootci))
  print(paste0("   difference = ", paste(difference, collapse = ", ")))
  print(paste0("   ci_low     = ", paste(ci_low, collapse = ", ")))
  print(paste0("   ci_high    = ", paste(ci_high, collapse = ", ")))

  if (is_bootci) {
    print(">>> Inside if(is_bootci) block")

    # Build a small data.frame for mean + CI
    delta_sum_df <- data.frame(
      mean_delta = difference,
      ci_low     = ci_low,
      ci_high    = ci_high
    )
    print(">>> delta_sum_df:")
    print(delta_sum_df)

    delta_x <- x_axis_breaks[length(x_axis_breaks)]
    print(paste0("   delta_x (x‐position for Δ) = ", delta_x))

    # — (A) vertical CI at x = delta_x —
    delta_plot <- delta_plot +
      ggplot2::geom_errorbar(
        data    = delta_sum_df,
        mapping = ggplot2::aes(
          x    = delta_x,
          ymin = ci_low,
          ymax = ci_high
        ),
        width      = 0.1,
        size       = es_line_size,
        color      = "black",
        inherit.aes = FALSE
      )
    print(">>> Added vertical CI bar")

    # — (B) black filled triangle at (delta_x, mean_delta) —
    delta_plot <- delta_plot +
      ggplot2::geom_point(
        data    = delta_sum_df,
        mapping = ggplot2::aes(
          x = delta_x,
          y = mean_delta
        ),
        shape   = 17,                 # filled triangle
        fill    = "black",
        color   = "black",
        size    = es_marker_size, 
        stroke  = 0.5,
        inherit.aes = FALSE
      )
    print(">>> Added black Δ‐triangle")

    # — (C) draw *our* right‐handed half‐violin of the bootstraps —
    #    1) compute a kernel density of boots[[1]]
    dens_obj <- stats::density(boots[[1]], na.rm = TRUE)
    max_dens     <- max(dens_obj$y, na.rm = TRUE)
    normalized_y <- dens_obj$y / max_dens * 0.2  # scale max width to 0.2
    df_boot_violin <- data.frame(
      x   = normalized_y + delta_x,   # left edge at delta_x
      y   = dens_obj$x,               # vertical = bootstrap‐Δ value
      tag = rep(delta_x, length(dens_obj$x))
    )
    print(">>> df_boot_violin head:")
    print(utils::head(df_boot_violin))

    delta_plot <- delta_plot +
      custom_geom_halfviolin(
        data    = df_boot_violin,
        mapping = ggplot2::aes(x = x, y = y, group = tag),
        fill    = "black",
        alpha   = 0.3,
        colour  = NA
      )
    print(paste0(">>> Added right‐side half‐violin at x = ", delta_x))

  } else {
    print(">>> SKIPPING is_bootci block because is_bootci = FALSE")
  }
  # ---- end debug block ----

  #### Zero‐dot (if requested) ####
  if (delta2 || minimeta) {
    zero_dot_x_breaks <- zero_dot_x_breaks[-length(zero_dot_x_breaks)]
  }
  if (is_zero_dot) {
    delta_plot <- add_bootci_component_to_delta_plot(
      delta_plot,
      zero_dot_x_breaks,
      0, 0, 0,
      es_marker_size,
      es_line_size
    )
  }

  #### Baseline EC (if requested) ####
  if (is_baseline_ec) {
    delta_plot <- add_violinplot_component_to_delta_plot(
      delta_plot,
      dabest_effectsize_obj,
      main_violin_type,
      flow,
      float_contrast,
      zero_dot_x_breaks
    )
    baseline_ec_boot_result <- dabest_effectsize_obj$baseline_ec_boot_result
    baseline_ci_low        <- baseline_ec_boot_result$bca_ci_low
    baseline_ci_high       <- baseline_ec_boot_result$bca_ci_high
    baseline_difference    <- baseline_ec_boot_result$difference
    delta_plot <- add_bootci_component_to_delta_plot(
      delta_plot,
      zero_dot_x_breaks,
      baseline_ci_low,
      baseline_ci_high,
      baseline_difference,
      es_marker_size,
      es_line_size
    )
  }

  #### Summary lines (if requested) ####
  if (is_summary_lines) {
    delta_plot <- delta_plot +
      ggplot2::geom_segment(
        colour    = "black",
        linewidth = 0.3,
        ggplot2::aes(
          x    = 1.8,
          xend = delta_x_max + 0.4,
          y    = difference,
          yend = difference
        )
      ) +
      ggplot2::geom_segment(
        colour    = "black",
        linewidth = 0.3,
        ggplot2::aes(
          x    = 1.8,
          xend = delta_x_max + 0.4,
          y    = 0,
          yend = 0
        )
      )
  }

  #### Final formatting ####
  if (float_contrast) {
    delta_plot <- delta_plot +
      dabestr:::float_contrast_theme +
      ggplot2::geom_hline(
        linewidth  = 0.8,
        yintercept = min_y_coords
      )
  }
  if (!float_contrast) {
    zero_line_xend <- delta_x_max + 0.3
    if (is_deltadelta) {
      zero_line_xend <- zero_line_xend + 0.2
    }
    delta_plot <- delta_plot +
      ggplot2::geom_segment(
        colour    = "gray",
        linewidth = 0.3,
        ggplot2::aes(
          x    = 0.6,
          xend = zero_line_xend,
          y    = 0,
          yend = 0
        )
      )
  }

  delta_plot <- delta_plot +
    ggplot2::labs(y = delta_y_labels) +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(size = contrast_x_text),
      axis.title.y = ggplot2::element_text(size = contrast_y_text)
    )

  # (We already forced contrast_bars = FALSE, so we skip add_contrast_bars_to_delta_plot here)

  if (plot_kwargs$delta_text) {
    delta_plot <- dabestr:::add_delta_text_to_delta_plot(
      delta_plot,
      dabest_effectsize_obj,
      plot_kwargs,
      x_axis_breaks,
      difference,
      main_violin_type,
      float_contrast
    )
  }
  if (show_delta_dots) {
    delta_plot <- dabestr:::add_delta_dots_to_delta_plot(
      delta_plot,
      dabest_effectsize_obj,
      plot_kwargs,
      x_axis_breaks,
      main_violin_type,
      delta_dots_data
    )
  }

  return(list(
    delta_plot  = delta_plot,
    delta_range = c(delta_y_min - delta_y_mean / 10, delta_y_max)
  ))
}




###
###
###
# --- 4. Modified dabest_plot that uses our wrappers ---
custom_dabest_plot <- function(dabest_effectsize_obj, float_contrast = TRUE, ...) {
  dabestr:::check_effectsize_object(dabest_effectsize_obj)

  # (Step A) grab user’s plot_kwargs
  plot_kwargs <- list(...)
  plot_kwargs <- custom_assign_plot_kwargs(dabest_effectsize_obj, plot_kwargs)
  custom_palette <- plot_kwargs$custom_palette
  idx            <- dabest_effectsize_obj$idx

  if (length(unlist(idx)) >= 3) {
    float_contrast <- FALSE
  }

  message("✅ Building raw and delta plots…")

  # (Step B) Build the raw plot
  raw_plot <- do.call(
    custom_plot_raw,
    c(list(dabest_effectsize_obj, float_contrast), plot_kwargs)
  )

  # (Step C) Build the delta plot
  delta_plot_list <- custom_plot_delta(
    dabest_effectsize_obj,
    float_contrast,
    plot_kwargs
  )
  delta_plot <- delta_plot_list$delta_plot

  # (Step D) Override the raw‐plot’s x‐axis to show “Unexposed” / “PCE” at x = 1, 2:
  raw_plot <- raw_plot +
    scale_x_continuous(
      breaks = c(1, 2),
      labels = c("Unexposed", "PCE")
    )

  # (Step E) Override the Δ‐plot’s x‐axis so that one tick at x = 2 is labeled “|Unexposed – PCE|”
  delta_plot <- delta_plot +
    scale_x_continuous(
      breaks = 2,
      labels = expression(paste("\u25B2 ", "PCE - Unexposed"))
    )

# Make all Y‐axis tick labels (the numbers) larger:
  raw_plot   <- raw_plot   + theme(axis.text.y = element_text(size = 16))
  delta_plot <- delta_plot + theme(axis.text.y = element_text(size = 16))

  # (Step F) Apply the gryffindor palette
  message("✅ Applying color palette: ", custom_palette)
  raw_plot   <- custom_apply_palette(raw_plot,   custom_palette)
  delta_plot <- custom_apply_palette(delta_plot, custom_palette)

  # (Step G) [Optional] Print built data for debugging:
  message("✅ Inspecting raw_plot layers and data:")
  print(ggplot2::ggplot_build(raw_plot)$data)
  message("✅ Inspecting delta_plot layers and data:")
  print(ggplot2::ggplot_build(delta_plot)$data)

  # (Step H) Arrange side‐by‐side
  final_plot <- cowplot::plot_grid(
    plotlist   = list(
      raw_plot   + theme(legend.position = "none"),
      delta_plot + theme(legend.position = "none")
    ),
    nrow       = 1,
    ncol       = 2,
    rel_widths = c(0.75, 0.25),
    axis       = "lr",
    align      = "h"
  )

  return(final_plot)
}
