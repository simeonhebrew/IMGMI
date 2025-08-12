#' Calculate IM Global Score from filtered merged data
#'
#' Given a data frame with IM_score per species and sample, this function
#' scales the IS_score, reshapes the data, and calculates the sample-level
#' Immunolo Modulatory Global Metric (IM_Global_Score).
#'
#' @param merged_df Data frame containing columns: Species, Sample, and IM_score.
#'
#' @return A data frame with samples as rownames and a column "IM_Global_Score".
#' @export
#'
#' @examples
#' # result <- calculate_is_global_score(merged_hc_prev_ab_prob_filt)
calculate_im_global_score <- function(merged_df) {
  if (!all(c("Species", "Sample", "IM_score") %in% colnames(merged_df))) {
    stop("Input data frame must contain columns: Species, Sample, IM_score")
  }

  # Scale IS_score between 0 and 1
  range_vals <- range(merged_df$IM_score, na.rm = TRUE)
  merged_df$IM_score_scaled <- (merged_df$IM_score - range_vals[1]) / (range_vals[2] - range_vals[1])

  # Select relevant columns
  df_imgm_index <- merged_df %>% dplyr::select(Species, Sample, IM_score_scaled)

  # Pivot wider: samples as rows, species as columns
  imgm_index_wide_df <- df_imgm_index %>%
    tidyr::pivot_wider(names_from = Species, values_from = IM_score_scaled)

  # Convert Sample column to rownames
  imgm_index_wide_df_table <- tibble::column_to_rownames(imgm_index_wide_df, var = "Sample")

  # Transpose twice (to keep the same structure as original)
  imgm_index_wide_df_table <- as.data.frame(t(as.data.frame(t(imgm_index_wide_df_table))))

  # Replace NAs with 0
  imgm_index_wide_df_table[is.na(imgm_index_wide_df_table)] <- 0

  # Presence matrix (1 if scaled score > 0, else 0)
  presence_matrix <- ifelse(imgm_index_wide_df_table > 0, 1, 0)

  # Sum of scores per sample
  sample_scores <- rowSums(imgm_index_wide_df_table)

  # Number of species present per sample
  num_present_species <- rowSums(presence_matrix)

  # Calculate IS Global Score per sample
  sample_iga_scaled <- sample_scores / num_present_species

  # Return as data frame
  sample_iga_scaled_df <- data.frame(IM_Global_Score = sample_iga_scaled)
  return(sample_iga_scaled_df)
}
