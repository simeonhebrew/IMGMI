#' Combine multiple data frames and compute ISGM_index
#'
#' This function takes two or more data frames, each containing an 'IS_Global_Score' column,
#' binds them row-wise, scales (z-score) the combined IS_Global_Score, and returns
#' the combined data frame with an added 'Z_score' column.
#'
#' @param ... Two or more data frames, each with an 'IS_Global_Score' numeric column.
#'
#' @return A combined data frame with all rows and an added column 'Z_score'.
#' @export
#'
#' @examples
#' # combined_df <- combine_and_zscore(df1, df2, df3)
calculate_isgm_index <- function(...) {
  # Capture all input data frames into a list
  dfs <- list(...)

  # Check at least two inputs
  if (length(dfs) < 2) {
    stop("Please provide at least two data frames as input.")
  }

  # Check each has IS_Global_Score column
  for (i in seq_along(dfs)) {
    if (!("IS_Global_Score" %in% colnames(dfs[[i]]))) {
      stop(paste0("Input data frame ", i, " does not have an 'IS_Global_Score' column."))
    }
  }

  # Combine row-wise
  combined_df <- do.call(rbind, dfs)

  # Calculate z-score of IS_Global_Score
  z_scores <- scale(combined_df$IS_Global_Score)

  # Add Z_score column
  combined_df$Z_score <- as.numeric(z_scores)

  return(combined_df)
}
