render_lecture <- function(x, purl = TRUE){
  input_paths <- sort(list.files("./_lectures/", 
                                 pattern = "^slides.*\\.[Qq]md$", 
                                 recursive = TRUE, 
                                 full.names = TRUE))[x + 1]
  
  if (purl) {
    r_out_paths <- stringr::str_replace(input_paths, "\\.[Qq]md$", ".R")
    unlink(r_out_paths, recursive = FALSE)
    purrr::walk2(.x = input_paths, .y = r_out_paths, 
                 ~ knitr::purl(.x, output = .y, documentation = 0))
  }
  
  purrr::walk(.x = input_paths, ~xfun::Rscript_call(
    quarto::quarto_render,
    list(input = .x)
  ))
}
render_lecture(0)

