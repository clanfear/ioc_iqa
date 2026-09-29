reference_date <- as.Date("2026-10-14")  # date of week 1 (a Wednesday)
week_0_date <- as.Date("2026-10-08")
get_date <- function(week, raw = FALSE) {
  if(raw){
    reference_date + (week - 1) * 7
    } else {
    format(reference_date + (week - 1) * 7, '%d %b %Y')
  }
}
