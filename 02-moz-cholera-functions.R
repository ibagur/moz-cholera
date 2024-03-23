# Using Jaro Winkler distance 
best_jw_match <- function(input_string, target_strings, p = 0.05, threshold = 0.7) {
  # Calculate jw distance
  distance <- stringdist::stringsim(input_string, target_strings, method="jw", p=p)
  best_match_index <- which.max(distance)
  max_distance <- max(distance)
  target <- ifelse(max_distance > threshold, target_strings[best_match_index], NA)
  return (c(target, max_distance))
}

# using Largest Common String
best_lcs_match <- function(input_string, target_strings) {
  # Calculate LCS lengths
  lcs_lengths <- stringdist::stringdist(input_string, target_strings, method = "lcs")
  best_match_index <- which.max(-lcs_lengths)
  max_lcs_length <- max(lcs_lengths)
  return(c(target_strings[best_match_index], max_lcs_length))
}

# Using fuzzywuzzyR
best_seq_match <- function(input_string, target_strings) {
  # get vector of ratios for each adm2 string
  init_ratio <- map_dbl(target_strings, ~fuzzywuzzyR::SequenceMatcher$new(string1 = input_string, string2 = .x)$ratio())
  # get the closest match per row
  best_match_index <- which.max(init_ratio) 
  max_ratio <- max(init_ratio)
  return(c(target_strings[best_match_index], max_ratio))
}

convert_date_to_days <- function(date_string, reference_date = "1899-12-30") {
  
  # Convert the input date string to a date object
  date <- ymd(date_string)
  
  # Convert the reference date to a date object
  ref_date <- ymd(reference_date)
  
  # Calculate the difference in days between the two dates
  days_diff <- as.character(difftime(date, ref_date, units = "days"))
  
  return(days_diff)
}
