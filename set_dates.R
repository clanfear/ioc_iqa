source("./_lectures/reference_date.R")   # defines reference_date

post_files <- list.files("posts", pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE)

for (f in post_files) {
  lines <- readLines(f, warn=FALSE)
  fences <- which(lines == "---")
  fm_range <- (fences[1] + 1):(fences[2] - 1)
  fm <- yaml::yaml.load(paste(lines[fm_range], collapse = "\n"))
  if (!is.null(fm$week)) {
    if(fm$week==0){
      new_date <- format(week_0_date, "%Y-%m-%d")
    } else {
      new_date <- format(reference_date + (fm$week - 1) * 7, "%Y-%m-%d")
    }
    new_line <- paste0("date: \"", new_date, "\"")
    
    date_idx <- grep("^date:", lines[fm_range])
    if (length(date_idx)) {
      lines[fm_range][date_idx] <- new_line
    } else {
      lines <- append(lines, new_line, after = fences[1])
    }
    writeLines(lines, f)
  }
}
