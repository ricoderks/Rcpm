#' @titlte Calculations for the fatty acid analysis plot
#' 
#' @description
#' Calculations for the fatty acid analysis plot.
#' 
#' @param lipid_data data.frame
#' @param meta_data data.frame, with meta data information.
#' @param selected_lipidclass character vector, name of the lipid class(es) of
#'     interest.
#' 
#' @return data.frame in wide format.
#' 
#' @author Rico Derks
#' 
fa_analysis_calc <- function(
    lipid_data = NULL,
    meta_data = NULL,
    selected_lipidclass = c("CE", "Cer", "DG", "FA", "HexCER", "LPC", "LPE", 
                            "LacCER", "PA", "PC", "PE", "PG", "PI", "PS", "SM",
                            "TG", "All", "All_noTG")) {
  # check the given lipid classes
  selected_lipidclass <- match.arg(arg = selected_lipidclass,
                                   several.ok = TRUE)
  
  # need feature data
  feature_data <- extract_feature(data = colnames(lipid_data)[-1])
  
  # fix TG's
  idx_tg <- feature_data$lipid[feature_data$lipid_class == "TG"]
  lipid_data[, idx_tg] <- lipid_data[, idx_tg] / 3
  
  # get the species from the selected lipid classes
  if(selected_lipidclass == "All") {
    # all lipids, but remove PA
    sel_feat_idx <- feature_data$lipid[!(feature_data$lipid_class %in% c("PA"))]
  } else if(selected_lipidclass == "All_noTG") {
    # all lipids, but remove PA
    sel_feat_idx <- feature_data$lipid[!(feature_data$lipid_class %in% c("PA", "TG"))]
  } else {
    sel_feat_idx <- feature_data$lipid[feature_data$lipid_class %in% selected_lipidclass]
  }
  sel_feature_table <- feature_data[feature_data$lipid %in% sel_feat_idx, ]
  
  ## Data
  # select the correct data
  sel_data_table <- lipid_data[, sel_feat_idx]
  
  # get the unique chain lengths and unsaturation
  uniq_carbon <- sort(union(unique(sel_feature_table$carbon_1[sel_feature_table$lipid_class != "TG"]),
                            unique(sel_feature_table$carbon_2)))
  uniq_carbon <- uniq_carbon[uniq_carbon != 0]
  uniq_unsat <- sort(union(unique(sel_feature_table$unsat_1[sel_feature_table$lipid_class != "TG"]),
                           unique(sel_feature_table$unsat_2)))
  
  # Initialize results data.frame
  fa_chains <- expand.grid(uniq_unsat, uniq_carbon)
  fa_chains <- paste(fa_chains[, 2], fa_chains[, 1], sep = ":")
  res <- as.data.frame(matrix(ncol = length(fa_chains),
                              nrow = nrow(sel_data_table)))
  colnames(res) <- fa_chains
  rownames(res) <- rownames(sel_data_table)
  
  # do the calculations
  for(a in uniq_carbon) {
    for(b in uniq_unsat) {
      sel_fa_chain <- paste(a, b, sep = ":")
      sel_lipids <- sel_feature_table$lipid[(sel_feature_table$carbon_1 == a &
                                               sel_feature_table$unsat_1 == b) |
                                              (sel_feature_table$carbon_2 == a &
                                                 sel_feature_table$unsat_2 == b)]
      sel_lipids_double <- sel_feature_table$lipid[(sel_feature_table$carbon_1 == a &
                                                      sel_feature_table$unsat_1 == b) &
                                                     (sel_feature_table$carbon_2 == a &
                                                        sel_feature_table$unsat_2 == b)]
      
      res[, sel_fa_chain] <- `+`(
        rowSums(sel_data_table[, sel_lipids, drop = FALSE], na.rm = TRUE),
        rowSums(sel_data_table[, sel_lipids_double, drop = FALSE], na.rm = TRUE)
      )
    }
  }
  
  # remove empty columns
  empty_idx <- apply(res, 2, function(x) {
    all(x == 0)
  })
  res <- res[, !empty_idx]
  
  return(res)
}


#' @titlte Calculations for the fatty acid analysis plot
#' 
#' @description
#' Calculations for the fatty acid analysis plot.
#' 
#' @param lipid_data data.frame
#' @param meta_data data.frame, with meta data information.
#' @param selected_fa character vector, name of the fatty acid tails of
#'     interest.
#' 
#' @return data.frame in wide format.
#' 
#' @author Rico Derks
#' 
fa_analysis_calc_rev <- function(
    lipid_data = NULL,
    meta_data = NULL,
    selected_fa = NULL) {
  
  # need feature data
  feature_data <- extract_feature(data = colnames(lipid_data)[-1])
  
  # get the unique lipid classes, but not PA
  uniq_lipid_classes <- unique(feature_data$lipid_class[!(feature_data$lipid_class %in% c("PA"))])
  
  sel_feat_idx <- feature_data$lipid[!(feature_data$lipid_class %in% c("PA"))]
  sel_feature_table <- feature_data[feature_data$lipid %in% sel_feat_idx, ]
  
  ## Data
  # select the correct data
  sel_data_table <- lipid_data[, sel_feat_idx]
  
  # Initialize results data.frame
  res <- as.data.frame(matrix(ncol = length(uniq_lipid_classes),
                              nrow = nrow(sel_data_table)))
  colnames(res) <- uniq_lipid_classes
  rownames(res) <- rownames(sel_data_table)
  
  # do the calculations
  fa_norm_tot <- 0
  for(lipid_class in uniq_lipid_classes) {
    for(fa_tail in selected_fa) {
      split_fa <- as.numeric(unlist(strsplit(fa_tail,
                                             split = ":",
                                             fixed = TRUE)))
      sel_lipids <- sel_feature_table$lipid[sel_feature_table$lipid_class == lipid_class &
                                              ((sel_feature_table$carbon_1 == split_fa[1] &
                                                  sel_feature_table$unsat_1 == split_fa[2]) |
                                                 (sel_feature_table$carbon_2 == split_fa[1] &
                                                    sel_feature_table$unsat_2 == split_fa[2]))]
      sel_lipids_double <- sel_feature_table$lipid[sel_feature_table$lipid == lipid_class &
                                                     (sel_feature_table$carbon_1 == split_fa[1] &
                                                        sel_feature_table$unsat_1 == split_fa[2]) &
                                                     (sel_feature_table$carbon_2 == split_fa[1] &
                                                        sel_feature_table$unsat_2 == split_fa[2])]
      
      res[, lipid_class] <- rowSums(sel_data_table[, c(sel_lipids, sel_lipids_double), drop = FALSE], na.rm = TRUE)
    } # end selected_fa
  } # end lipid_class
  
  # fix the TG's
  res[, "TG"] <- res[, "TG"] / 3
  
  # remove empty columns
  empty_idx <- apply(res, 2, function(x) {
    all(x == 0)
  })
  res <- res[, !empty_idx]
  
  # get rid of the zero's
  res[res == 0] <- NA
  
  return(res)
}


#' @title Prepare the data for the fatty acid analysis plot
#' 
#' @description
#' Prepare the data for the fatty acid analysis plot, i.e. transform to long 
#' format.
#' 
#' @param fa_data data.frame.
#' @param meta_data data.frame, with meta data information.
#' @param group_column character(1), column name of the meta data containing the 
#'     group information.
#' 
#' @return data.frame in long format.
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr all_of summarise
#' @importFrom stats sd
#' @importFrom rlang .data
#' 
#' @author Rico Derks
#' 
fa_analysis_prep <- function(fa_data = NULL,
                             meta_data = NULL,
                             group_column = NULL) {
  
  fa_data <- cbind(
    meta_data[, c("sampleID", group_column)],
    fa_data
  )
  
  colnames(fa_data)[2] <- "groups"
  
  # get the colnames of fa_data, not belonging to lipid species
  
  fa_data <- fa_data |> 
    tidyr::pivot_longer(
      cols = -dplyr::all_of(c("sampleID", "groups")),
      names_to = "variable",
      values_to = "value"
    ) |> 
    dplyr::summarise(mean = mean(.data$value, na.rm = TRUE),
                     sd = stats::sd(.data$value, na.rm = TRUE),
                     .by = dplyr::all_of(c("variable", "groups")))
  
  return(fa_data)
}


#' @title Fatty acid analysis plot
#' 
#' @description
#' Fatty acid analysis plot. Show the amount of fatty acid tail per group.
#' 
#' @param lipid_data data.frame
#' @param meta_data data.frame, with meta data information.
#' @param selected_lipidclass character vector, name of the lipid class(es) of
#'     interest.
#' @param selected_fa character vector, fatty acid tails.
#' @param group_column character(1), column name of the meta data containing the 
#'     group information.
#' @param plot_title character(1), title for the plot.
#' @param y_axis_title character(1), title for the y-axis.
#' @param legend_title character(1), title for the legend.
#' @param rev logical(1), if TRUE x-axis will be lipid class.
#'     
#' @return Fatty acid analysis plot as ggplot2 object.
#' 
#' @importFrom ggplot2 ggplot aes geom_col theme_minimal labs theme guides
#'     guide_legend
#' @importFrom rlang .data
#' 
#' @export
#' 
#' @author Rico Derks
#' 
fa_analysis_plot <- function(
    lipid_data = NULL,
    meta_data = NULL,
    selected_lipidclass = NULL,
    selected_fa = NULL,
    group_column = NULL,
    plot_title = NULL,
    y_axis_title = NULL,
    legend_title = NULL,
    rev = FALSE) {
  
  if(!is.null(selected_lipidclass)) {
    selected_lipidclass <- match.arg(
      arg = selected_lipidclass,
      choices = c("CE", "Cer", "DG", "FA", "HexCER", "LPC", "LPE", 
                  "LacCER", "PA", "PC", "PE", "PG", "PI", "PS", "SM",
                  "TG", "All", "All_noTG"),
      several.ok = TRUE
    )
  }
  
  if(ref) {
    fa_data <- fa_analysis_calc_rev(lipid_data = lipid_data,
                                    meta_data = meta_data,
                                    selected_fa = selected_fa)
  } else {
    fa_data <- fa_analysis_calc(lipid_data = lipid_data,
                                meta_data = meta_data,
                                selected_lipidclass = selected_lipidclass)
  }
  
  plot_data <- fa_analysis_prep(fa_data = fa_data,
                                meta_data = meta_data,
                                group_column = group_column)
  
  p <- plot_data |> 
    ggplot2::ggplot(ggplot2::aes(x = .data$variable,
                                 y = .data$mean,
                                 fill = .data$groups)) +
    ggplot2::geom_col(position = "dodge2") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend_title)) +
    ggplot2::labs(
      x = ifelse(rev, "Lipid class" ,"Fatty acid tails"),
      y = y_axis_title,
      title = plot_title
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
    
    return(p)
}
