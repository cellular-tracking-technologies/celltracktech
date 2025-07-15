#' Main function to process all CSV/CSV.GZ files in a directory
#'
#' @param input_dir
#' @param output_dir
#'
#' @returns
#' @export
#'
#' @examples
#' process_directory('./data/', './data/results/')
process_directory <- function(input_dir, output_dir) {
  # Get list of .csv and .csv.gz files
  files <- list.files(input_dir, pattern = "\\.(csv|csv\\.gz)$", full.names = TRUE)

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Process each file
  for (file in files) {
    process_file(file, output_dir)
    cat("Processed file:", basename(file), "\n")
  }
}
