#' Load environment variables from a .env file
#'
#' Re-exported from the dotenv package. Reads key-value pairs from a .env
#' file and sets them as environment variables, accessible via Sys.getenv().
#'
#' @param file Path to the .env file (default: ".env")
#' @return Invisibly returns a named list of the variables set.
#' @export
#' @importFrom dotenv load_dot_env
#' @examples
#' \dontrun{
#' load_dot_env(file = ".env")
#' my_token <- Sys.getenv("MY_TOKEN")
#' }
load_dot_env <- dotenv::load_dot_env
