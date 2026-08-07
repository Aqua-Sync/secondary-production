# load list of models called updated_gams.rds. If it doesn't exist, then compile it from the individual brm models in "models/".
# This is needed b/c the main workflow uses updated_gams.rds, but that file is too large for github. Hence the individual models posted separately to github.

load_updated_gams <- function(model_dir = "models") {
  
  combined_path <- file.path(model_dir, "updated_gams.rds")
  
  if (file.exists(combined_path)) {
    return(readRDS(combined_path))
  }
  
  gam_files <- list.files(
    model_dir,
    "^updated_gams_[0-9]+\\.rds$"
  )
  
  if (length(gam_files) == 0) {
    stop("No updated_gams_*.rds files found in ", model_dir)
  }
  
  gam_files <- gam_files[
    order(as.numeric(sub("updated_gams_([0-9]+)\\.rds", "\\1", gam_files)))
  ]
  
  out <- lapply(file.path(model_dir, gam_files), readRDS)
  
  saveRDS(out, combined_path)
  
  out
}