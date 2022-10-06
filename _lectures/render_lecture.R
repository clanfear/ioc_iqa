render_lecture <- function(x, purl = TRUE){
  input_paths <-  sort(list.files("./_lectures/", 
                                  pattern = "^slides.*Rmd$", 
                                  recursive = TRUE, 
                                  full.names = TRUE))[x+1]
  r_out_paths <-  stringr::str_remove(input_paths, "md$")
  unlink(r_out_paths, recursive = FALSE)
  purrr::walk2(.x = input_paths, .y = r_out_paths, 
               ~ knitr::purl(.x, output = .y, documentation = 0))
  purrr::walk(.x = input_paths, ~rmarkdown::render(.x, encoding = "UTF-8"))
}
render_lecture(1)
