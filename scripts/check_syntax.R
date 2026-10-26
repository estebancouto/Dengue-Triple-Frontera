# Syntax only: no data reads and no analysis execution.
files <- list.files("scripts", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
failures <- character()
for (f in files) {
  result <- tryCatch({parse(f, encoding = "UTF-8"); NULL}, error = function(e) conditionMessage(e))
  if (!is.null(result)) failures <- c(failures, paste(f, result, sep = ": "))
}
if (length(failures)) stop(paste(failures, collapse = "\n"), call. = FALSE)
message("Parsed ", length(files), " R scripts. Statistical results remain unverified.")
