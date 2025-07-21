
#' Function to process a single file
#'
#' @param file_path filename
#' @param output_dir output directory
#'
#' @returns csv with parsed battery voltage and temperature
#' @export
#'
#' @examples
#' process_file('./blu-data.csv', './')
process_file <- function(file_path, output_dir) {
  blu <- load_csv(file_path)

  # Identify the payload column
  payload_column <- if ("Payload" %in% colnames(blu)) {
    "Payload"
  } else if ("payload" %in% colnames(blu)) {
    "payload"
  } else {
    return(cat("No payload column found in", basename(file_path), "\n"))
  }

  # Apply the parseit function to each Payload and create new columns
  parsed <- t(sapply(blu[[payload_column]], parseit))
  blu$Battery_Voltage_V <- parsed[, 1]
  blu$Temperature_Celsius <- parsed[, 2]

  # Create output file name and save the updated CSV
  file_name <- basename(sub("\\.gz$", "", file_path)) # Remove directory and .gz if present

  print('file name')
  print(file_name)

  print('output directory')
  print(output_dir)
  # output_file <- file.path(output_dir, paste0(file_name, "_parsed.csv"))
  output_file <- file.path(output_dir, file_name)
  print('output file')
  print(output_file)

  write.csv(blu, output_file, row.names = FALSE)
}
