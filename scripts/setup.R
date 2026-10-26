# Run from the repository root. This creates directories but installs nothing.
stopifnot(file.exists("Dengue-Triple-Frontera.Rproj"))
paths <- readLines("config/local_directories.txt", warn = FALSE)
for (p in paths) dir.create(p, recursive = TRUE, showWarnings = FALSE)
packages <- readLines("config/packages.txt", warn = FALSE)
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) message("Missing/unresolved packages: ", paste(missing, collapse = ", "))
message("Review docs/REPRODUCIBILITY.md before executing analyses.")
