camel_to_snake <- function(string) {
# camel_case_string <- "Time"

# Insert an underscore before each uppercase letter that is not at the start of the string
snake_case_string <- gsub("([A-Z])", "_\\L\\1", string, perl = TRUE)

# Convert the entire string to lowercase and remove any leading underscore if the original string started with an uppercase letter
snake_case_string <- tolower(snake_case_string)
snake_case_string <- gsub("^_", "", snake_case_string)

print(snake_case_string)

return(snake_case_string)
}
