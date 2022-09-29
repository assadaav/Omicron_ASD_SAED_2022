get_model_stats = function(x, precision=60) {
  
  # remember old number formatting function
  # (which would round and transforms p-values to formats like "<0.01")
  old_format_np = rms::formatNP
  # substitute it with a function which will print out as many digits as we want
  assignInNamespace("formatNP", function(x, ...) formatC(x, format="f", digits=precision), "rms")
  
  # remember old width setting
  old_width = options('width')$width
  # substitute it with a setting making sure the table will not wrap
  options(width=old_width + 4 * precision)
  
  # actually print the data and capture it
  cap = capture.output(print(x))
  
  # restore original settings
  options(width=old_width)
  assignInNamespace("formatNP", old_format_np, "rms")
  
  #model stats
  stats = c()
  stats$R2.adj = str_match(cap, "R2 adj\\s+ (\\d\\.\\d+)") %>% na.omit() %>% .[, 2] %>% as.numeric()
  
  #coef stats lines
  coef_lines = cap[which(str_detect(cap, "Coef\\s+S\\.E\\.")):(length(cap) - 1)]
  
  #parse
  coef_lines_table = suppressWarnings(readr::read_table(coef_lines %>% stringr::str_c(collapse = "\n")))
  colnames(coef_lines_table)[1] = "Predictor"
  
  list(
    stats = stats,
    coefs = coef_lines_table
  )
}