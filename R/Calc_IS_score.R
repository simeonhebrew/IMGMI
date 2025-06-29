

#Package code


#' Calculate Immunostimulatory Score
#'
#' This function merges relative abundance (`otu_table`), prevalence, and Ig binding score data (`binding_score`)
#' to compute an Immunostimulatory Score (IS_score) for each OTU (species).
#'
#' @param otu_table A matrix or data frame of relative abundances (species x samples).
#' @param binding_score A data frame containing inferred binding scores either using Kau Index, IgA probability score or IgA probability ratio (species x samples).
#' @param w_P Weight for prevalence in the IS score (0.3).
#' @param w_A Weight for relative abundance in the IS score (0.3).
#' @param w_I Weight for the binding score in the IS score (0.4).
#'
#' @return A data frame with Species, Sample, Binding_score, Prevalence, Rel_ab, and IS_score.
#' @export
#'
#' @examples
#' # result <- calculate_is_score(otu_table, binding_score)
calculate_is_score <- function(otu_table,
                               binding_score,
                               w_P = 0.3,
                               w_A = 0.3,
                               w_I = 0.4) {
  # check inputs
  if (!is.data.frame(otu_table) && !is.matrix(otu_table)) {
    stop("otu_table must be a data frame or matrix.")
  }
  if (!is.data.frame(binding_score)) {
    stop("binding_score must be a data frame.")
  }

  # reshape relative abundance dataframe
  otu_df <- tibble::rownames_to_column(as.data.frame(otu_table), var = "Species")
  melt_abund_df <- reshape2::melt(otu_df, id = "Species")
  colnames(melt_abund_df) <- c("Species", "Sample", "Rel_ab")

  # calculate prevalence
  prevalence <- rowSums(otu_table > 0) / ncol(otu_table)
  prevalence_df <- tibble::rownames_to_column(as.data.frame(prevalence), var = "Species")

  # reshape binding score dataframe
  melted_binding <- reshape2::melt(tibble::rownames_to_column(binding_score, var = "Species"), id = "Species")
  colnames(melted_binding) <- c("Species", "Sample", "Binding_score")

  # merge data
  merged_data <- dplyr::left_join(melted_binding, prevalence_df, by = "Species")
  merged_data <- dplyr::bind_cols(merged_data, Rel_ab = melt_abund_df$Rel_ab)

  # filter out zero prevalence since these means that these species aren't present at all
  merged_data <- dplyr::filter(merged_data, prevalence != 0)

  # compute IS_score
  merged_data$IS_score <- (w_P * merged_data$prevalence) +
    (w_A * merged_data$Rel_ab) +
    (w_I * merged_data$Binding_score)

  # rename column names
  colnames(merged_data) <- c("Species", "Sample", "Binding score", "Prevalence", "Rel_ab", "IS_score")

  return(merged_data)
}































