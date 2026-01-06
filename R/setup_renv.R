# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Initialize renv if not already initialized
if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE)
} else {
  renv::restore()
}

message("renv setup complete.")
