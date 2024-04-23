#' @title Extract carbon number and double bond information from lipid species 
#' 
#' @description
#' Extract carbon number and ouble bond information from lipid species name in 
#' Sciex Lipidyzer format.
#' 
#' @param data character vector with lipid species names.
#' 
#' @return data.frame containing lipid, lipid class, carbon number of tail 1, 
#'     carbon number of tail 2, double bond of tail 1, double bond of tail 2,
#'     total carbon number, total double bond.
#' 
#' @author Rico Derks
#' 
extract_feature <- function(data = NULL) {
  feature_data <- data.frame(lipid = data)
  
  tmp <- strsplit(x = feature_data$lipid,
                  split = " O-| P-| d| |/|_|-FA")
  
  feature_data$lipid_class <- sapply(tmp, function(x) {
    x[1]
  })
  
  feature_data$carbon_1 <- sapply(tmp, function(x) {
    as.numeric(strsplit(x = x[2],
                        split = ":")[[1]][1])
  })
  feature_data$unsat_1 <- sapply(tmp, function(x) {
    as.numeric(strsplit(x = x[2],
                        split = ":")[[1]][2])
  })
  
  feature_data$carbon_2 <- sapply(tmp, function(x) {
    as.numeric(strsplit(x = x[3],
                        split = ":")[[1]][1])
  })
  feature_data$unsat_2 <- sapply(tmp, function(x) {
    as.numeric(strsplit(x = x[3],
                        split = ":")[[1]][2])
  })
  
  feature_data$carbon_sum <- NA
  feature_data$unsat_sum <- NA
  for(a in 1:nrow(feature_data)) {
    feature_data$carbon_sum[a] <- ifelse(
      feature_data$lipid_class[a] %in% c("TG", "PA"),
      feature_data$carbon_1[a],
      sum(feature_data$carbon_1[a], feature_data$carbon_2[a], na.rm = TRUE)
    )
    feature_data$unsat_sum[a] <- ifelse(
      feature_data$lipid_class[a] %in% c("TG", "PA"),
      feature_data$unsat_1[a],
      sum(feature_data$unsat_1[a], feature_data$unsat_2[a], na.rm = TRUE)
    )
  }
  
  return(feature_data)
}


#' @title Calculations for the fatty acid tail composition comparisson
#' 
#' @description
#' Calculations to compare the fatty acid tail composition of a lipid class 
#' between two groups.
#' 
#' @param lipid_data data.frame containing all the data. Rows are samples, 
#'     columns are variables (lipids). The column names are the names of the 
#'     lipid species in the format of the Sciex Lipidyzer result file.
#' @param meta_data data.frame containing all the meta data of the samples.
#' @param group_column character(1), column name of the meta data containing the 
#'     group information.
#' @param selected_group character(1) vector with the name of one the groups 
#'     used in the comparison.
#' @param selected_lipidclass character(1) containing the name of the lipid 
#'     class.
#' 
#' @return data.frame in long format.
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' 
#' @author Rico Derks
#' 
calc_fa_composition <- function(lipid_data = NULL,
                                meta_data = NULL,
                                group_column = NULL,
                                selected_group = NULL,
                                selected_lipidclass = NULL) {
  # samples
  idx_samples <- meta_data$sampleID[meta_data[, group_column] %in% selected_group]
  
  # features 
  feature_data <- extract_feature(data = colnames(lipid_data)[-1])
  
  selected_features <- feature_data[feature_data$lipid_class == selected_lipidclass, ]
  # get the unique chain lengths and unsaturation
  uniq_carbon <- c(min(selected_features$carbon_sum, na.rm = TRUE), max(selected_features$carbon_sum, na.rm = TRUE))
  uniq_unsat <- c(min(selected_features$unsat_sum, na.rm = TRUE), max(selected_features$unsat_sum, na.rm = TRUE))
  
  ## calculations
  hm_data <- as.matrix(lipid_data[lipid_data$sampleID %in% idx_samples, -1])
  rownames(hm_data) <- idx_samples
  # initialize result matrix
  res <- as.data.frame(matrix(ncol = length(uniq_carbon[1]:uniq_carbon[2]),
                              nrow = length(uniq_unsat[1]:uniq_unsat[2])))
  colnames(res) <- uniq_carbon[1]:uniq_carbon[2]
  rownames(res) <- uniq_unsat[1]:uniq_unsat[2]
  for(a in rownames(res)) { # unsaturation
    for(b in colnames(res)) { # carbons
      idx_lipids <- selected_features$lipid[selected_features$carbon_sum == b &
                                              selected_features$unsat_sum == a]
      if(length(idx_lipids) > 0) {
        res[a, b] <- sum(hm_data[, idx_lipids], na.rm = TRUE)
      } else {
        res[a, b] <- 0
      }
    }
  }
  
  # calculate the proportion
  res <- res / sum(res)
  
  ## prepare for plotting
  # add the double bond
  res$double_bond <- rownames(res)
  
  # make long
  res <- res |> 
    tidyr::pivot_longer(
      cols = -.data$double_bond,
      names_to = "carbon_number",
      values_to = "proportion"
    )
  
  return(res)
}

#' @title Heatmap part of the fatty acid composition plot
#' 
#' @description
#' Heatmap part of the fatty acid composition plot.
#' 
#' @param hm_data data.frame with the data.
#' @param avg_double numeric(1), average of the total unsaturation.
#' @param avg_carbon numeric(1), average of the total carbon number.
#' @param color_limits numeric(2), minimum and maxium value for the colour bar.
#' @param y_pos_right logical(1), if TRUE the y-axis will be placed on the right
#'     side.
#' @param show_legend logical(1), show the legend
#'
#' @return Heatmap as ggplot2 object.
#' 
#' @importFrom ggplot2 ggplot aes geom_tile geom_hline geom_vline 
#'     scale_fill_gradientn scale_y_discrete labs guides guide_colourbar
#'     theme_minimal annotate
#' @importFrom rlang .data
#' @importFrom stats predict lm
#' @importFrom RColorBrewer brewer.pal
#' 
#' @author Rico Derks
#' 
fa_comp_heatmap <- function(hm_data = NULL,
                            avg_double = NULL,
                            avg_carbon = NULL,
                            color_limits = NULL,
                            y_pos_right = FALSE,
                            show_legend = FALSE) {
  # set the position of the y-axis
  if(y_pos_right) {
    y_pos <- "right"
  } else {
    y_pos <- "left"
  }
  
  # determine the position of the horizontal and vertical line
  vline_df <- data.frame(x = min(hm_data$carbon_number):max(hm_data$carbon_number),
                         y = 1:length(min(hm_data$carbon_number):max(hm_data$carbon_number)))
  vline <- stats::predict(
    stats::lm(y ~ x, data = vline_df),
    data.frame(x = avg_carbon)
  )
  
  hline_df <- data.frame(x = max(hm_data$double_bond):min(hm_data$double_bond),
                         y = 1:length(min(hm_data$double_bond):max(hm_data$double_bond)))
  hline <- stats::predict(
    stats::lm(y ~ x, data = hline_df),
    data.frame(x = avg_double)
  )
  
  # heatmap
  hm <- hm_data |> 
    ggplot2::ggplot(ggplot2::aes(x = .data$carbon_number,
                                 y = .data$double_bond,
                                 fill = .data$proportion)) +
    ggplot2::geom_tile(color = "white",
                       linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = vline,
                        linetype = 2,
                        color = "black") +
    ggplot2::geom_hline(yintercept = hline,
                        linetype = 2,
                        color = "black") +
    ggplot2::annotate(geom = "text",
                      x = max(hm_data$carbon_number),
                      y = hline + 0.1,
                      vjust = 0,
                      size = 3, 
                      label = sprintf("Avg. %0.1f", avg_double)) +
    ggplot2::annotate(geom = "text",
                      x = vline + 0.1,
                      y = max(hm_data$double_bond),
                      hjust = 0,
                      size = 3, 
                      label = sprintf("Avg. %0.1f", avg_carbon)) +
    ggplot2::scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 5, name = "Blues"),
                                  limits = color_limits) +
    ggplot2::scale_y_discrete(limits = rev,
                              position = y_pos) +
    ggplot2::labs(x = "Total carbon number",
                  y = "Total double bond") +
    ggplot2::guides(fill = ggplot2::guide_colourbar(title = "Proportion")) +
    ggplot2::theme_minimal()
  
  if(!show_legend) {
    hm <- hm +
      ggplot2::theme(legend.position = "none")
  }
  
  return(hm)
}


#' @title Prepare data for carbon number histogram
#' 
#' @description
#' Prepare the data to create the carbon number histogram
#' 
#' @param data data.frame
#' @param position character(1), prepare the data for the 'top' or 'side' 
#'     histogram.
#' 
#' @return data.frame in long format.
#' 
#' @author Rico Derks
#' 
prep_histogram <- function(data = NULL,
                           position = c("side", "top")) {
  column <- switch(
    position,
    "side" = "double_bond",
    "top" = "carbon_number"
  )
  bar_data <- data.frame(
    proportion = tapply(
      data[, c(column, "proportion")],
      data[, column],
      function(x) {
        sum(x["proportion"])
      }
    )
  )
  
  bar_data$x <- factor(
    x = rownames(bar_data),
    levels = sort(as.numeric(rownames(bar_data))),
    labels = sort(as.numeric(rownames(bar_data)))
  )
  
  return(bar_data)
}


#' @title Create the side histograms
#' 
#' @description
#' Create the side histograms for the fatty acid composition.
#' 
#' @param hist_data data.frame
#' @param position characer(1), position of the histogram. 'right' and 'left' 
#'     is for the side histograms.
#' @param title character(1), title of the histogram.
#' @param y_pos_right logical(1), if TRUE the y-axis will be placed on the right
#'     side.
#' 
#' @return Histogram as ggplot2 object.
#' 
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal theme 
#'     element_blank coord_flip scale_x_discrete scale_y_reverse 
#'     scale_y_continuous element_text
#' @importFrom rlang .data
#' 
#' @author Rico Derks
#' 
fa_histogram <- function(hist_data = NULL,
                         position = c("left", "right", "top"),
                         title = NULL,
                         y_pos_right = FALSE) {
  # set the position of the y-axis
  if(y_pos_right) {
    y_pos <- "right"
  } else {
    y_pos <- "left"
  }
  
  histogram <- hist_data |>  
    ggplot2::ggplot(ggplot2::aes(x = .data$x,
                                 y = .data$proportion)) +
    ggplot2::geom_col() 
  
  histogram <- switch(
    position,
    "left" = {
      histogram +
        ggplot2::scale_x_discrete(limits = rev) +
        ggplot2::scale_y_reverse() +
        ggplot2::coord_flip() +
        ggplot2::labs(y = "Proportion") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank())
    },
    "right" = {
      histogram +
        ggplot2::scale_x_discrete(limits = rev) +
        ggplot2::coord_flip() +
        ggplot2::labs(y = "Proportion") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank())
    },
    "top" = histogram +
      ggplot2::labs(y = "Proportion") +
      ggplot2::scale_y_continuous(position = y_pos) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank())
  )
  
  histogram <- histogram +
    ggplot2::labs(title = title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  return(histogram)
}


#' @title Compare the fatty acid tail composition
#' 
#' @description
#' Compare the fatty acid tail composition of a lipid class between two groups.
#' 
#' @param lipid_data data.frame containing all the data.
#' @param meta_data data.frame containing all meta data.
#' @param group_column character(1) column name (meta data) containing all 
#'     column information.
#' @param groups character(2) the name of the two groups for the comparison.
#' @param selected_lipidclass character(1) name of the lipid class of interest.
#' 
#' @return ggplot2 object.
#' 
#' @importFrom ggplot2 ggplot aes
#' @importFrom patchwork wrap_plots
#' @importFrom stats weighted.mean
#' 
#' @export
#' 
#' @author Rico Derks
#' 
plot_fa_composition <- function(lipid_data = NULL,
                                meta_data = NULL,
                                group_column = NULL,
                                groups = NULL,
                                selected_lipidclass = NULL) {
  #### Data
  ## left side
  # heatmap
  hm_left_data <- calc_fa_composition(lipid_data = lipid_data,
                                      meta_data = meta_data,
                                      group_column = group_column,
                                      selected_group = groups[1],
                                      selected_lipidclass = selected_lipidclass)
  # barplot top (carbon number)
  bar_top_left_data <- prep_histogram(data = hm_left_data,
                                      position = "top")
  avg_carbon_left <- round(stats::weighted.mean(x = as.numeric(as.character(bar_top_left_data$x)),
                                                w = bar_top_left_data$proportion),
                           digits = 1)
  
  # barplot side (double bond)
  bar_side_left_data <- prep_histogram(data = hm_left_data,
                                       position = "side")
  avg_unsat_left <- round(stats::weighted.mean(x = as.numeric(as.character(bar_side_left_data$x)),
                                               w = bar_side_left_data$proportion),
                          digits = 1)
  
  ## right side
  # heatmap
  hm_right_data <- calc_fa_composition(lipid_data = lipid_data,
                                       meta_data = meta_data,
                                       group_column = group_column,
                                       selected_group = groups[2],
                                       selected_lipidclass = selected_lipidclass)
  # barplot top (carbon number)
  bar_top_right_data <- prep_histogram(data = hm_right_data,
                                       position = "top")
  avg_carbon_right <- round(stats::weighted.mean(x = as.numeric(as.character(bar_top_right_data$x)),
                                                 w = bar_top_right_data$proportion),
                            digits = 1)
  
  # barplot side (double bond)
  bar_side_right_data <- prep_histogram(data = hm_right_data,
                                        position = "side")
  avg_unsat_right <- round(stats::weighted.mean(x = as.numeric(as.character(bar_side_right_data$x)),
                                                w = bar_side_right_data$proportion),
                           digits = 1)
  
  # get the min and max value for the heatmap colorbar
  min_value <- min(c(min(hm_left_data$proportion), min(hm_right_data$proportion)))
  max_value <- max(c(max(hm_left_data$proportion), max(hm_right_data$proportion)))
  
  #### Plots
  ## left
  hm_left <- fa_comp_heatmap(hm_data = hm_left_data,
                             avg_carbon = avg_carbon_left,
                             avg_double = avg_unsat_left,
                             color_limits = c(min_value, max_value),
                             show_legend = TRUE)
  hist_top_left <- fa_histogram(hist_data = bar_top_left_data,
                                position = "top",
                                title = groups[1])
  hist_side_left <- fa_histogram(hist_data = bar_side_left_data,
                                 position = "left")
  ## right
  hm_right <- fa_comp_heatmap(hm_data = hm_right_data,
                              avg_carbon = avg_carbon_right,
                              avg_double = avg_unsat_right,
                              color_limits = c(min_value, max_value),
                              y_pos_right = TRUE,
                              show_legend = TRUE)
  hist_top_right <- fa_histogram(hist_data = bar_top_right_data,
                                 position = "top",
                                 y_pos_right = TRUE,
                                 title = groups[2])
  hist_side_right <- fa_histogram(hist_data = bar_side_right_data,
                                  position = "right")
  
  p <- patchwork::wrap_plots(
    hist_top_left, hist_top_right,
    hist_side_left, hm_left, hm_right, hist_side_right,
    design = "#11112222#
              3444455556
              3444455556
              3444455556
              3444455556",
    guides = "collect"
  ) +
    patchwork::plot_annotation(
      title = paste0("Lipid class: ", selected_lipidclass),
      theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    )
  
  
  return(p)
}