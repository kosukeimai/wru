## Upload eBISG model weights to GitHub release via piggyback
## Run this once to make the model weights available for download.
##
## Prerequisites:
##   - GITHUB_PAT environment variable set with write access to kosukeimai/wru
##   - The three .pt checkpoint files in DATA_DIR
##
## Usage:
##   Rscript inst/scripts/prepare-ebisg-piggyback.R

library(piggyback)

DATA_DIR <- "C:/Users/Noah/Desktop/Papers/surname_embeddings/data/2020"

# Create the release tag if it doesn't exist
tryCatch(
  pb_new_release(repo = "kosukeimai/wru", tag = "ebisg-v1.0"),
  error = function(e) message("Release may already exist: ", e$message)
)

# Upload model weight files
# Note: these are renamed from the original filenames for clarity
files_to_upload <- list(
  list(src = "mlp_surname_e5_tuned.pt", dst = "ebisg-surname-mlp.pt"),
  list(src = "mlp_firstname_e5_tuned.pt", dst = "ebisg-firstname-mlp.pt"),
  list(src = "mlp_fullname_e5_tuned.pt", dst = "ebisg-fullname-mlp.pt")
)

for (f in files_to_upload) {
  src_path <- file.path(DATA_DIR, f$src)
  dst_path <- file.path(tempdir(), f$dst)

  if (!file.exists(src_path)) {
    stop("Source file not found: ", src_path)
  }

  file.copy(src_path, dst_path, overwrite = TRUE)

  cat("Uploading", f$dst, "...\n")
  pb_upload(
    file = dst_path,
    repo = "kosukeimai/wru",
    tag = "ebisg-v1.0"
  )
}

cat("Done. Model weights uploaded to kosukeimai/wru release ebisg-v1.0\n")
