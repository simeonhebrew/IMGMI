#' Calculate IS Global Score from filtered merged data
#'
#' Given a data frame with IS_score per species and sample, this function
#' scales the IS_score, reshapes the data, and calculates the sample-level
#' Immunological Score Global Metric (IS_Global_Score).
#'
#' @param merged_df Data frame containing columns: Species, Sample, and IS_score.
#'
#' @return A data frame with samples as rownames and a column "IS_Global_Score".
#' @export
#'
#' @examples
#' # result <- calculate_is_global_score(merged_hc_prev_ab_prob_filt)
calculate_is_global_score <- function(merged_df) {
  if (!all(c("Species", "Sample", "IS_score") %in% colnames(merged_df))) {
    stop("Input data frame must contain columns: Species, Sample, IS_score")
  }

  # Scale IS_score between 0 and 1
  range_vals <- range(merged_df$IS_score, na.rm = TRUE)
  merged_df$IS_score_scaled <- (merged_df$IS_score - range_vals[1]) / (range_vals[2] - range_vals[1])

  # Select relevant columns
  df_isgm_index <- merged_df %>% dplyr::select(Species, Sample, IS_score_scaled)

  # Pivot wider: samples as rows, species as columns
  isgm_index_wide_df <- df_isgm_index %>%
    tidyr::pivot_wider(names_from = Species, values_from = IS_score_scaled)

  # Convert Sample column to rownames
  isgm_index_wide_df_table <- tibble::column_to_rownames(isgm_index_wide_df, var = "Sample")

  # Transpose twice (to keep the same structure as original)
  isgm_index_wide_df_table <- as.data.frame(t(as.data.frame(t(isgm_index_wide_df_table))))

  # Replace NAs with 0
  isgm_index_wide_df_table[is.na(isgm_index_wide_df_table)] <- 0

  # Presence matrix (1 if scaled score > 0, else 0)
  presence_matrix <- ifelse(isgm_index_wide_df_table > 0, 1, 0)

  # Sum of scores per sample
  sample_scores <- rowSums(isgm_index_wide_df_table)

  # Number of species present per sample
  num_present_species <- rowSums(presence_matrix)

  # Calculate IS Global Score per sample
  sample_iga_scaled <- sample_scores / num_present_species

  # Return as data frame
  sample_iga_scaled_df <- data.frame(IS_Global_Score = sample_iga_scaled)
  return(sample_iga_scaled_df)
}
