
################################################################################
#                                                                              #
#                             Common use functions                             #
#                                                                              #
################################################################################

# Function to get file name without extension
filename <- function(filepath) {
  output <- tools::file_path_sans_ext(basename(filepath))
  return(output)
}

# Function to get index/es of specific column/s
get_col_indexes <- function(data, init_column, final_column = NULL) {
  init <- which(names(data) == init_column)
  final <- which(names(data) == final_column)
  index <- c(init,final)
  return(index)
}

# Function to get range from index/es of specific column/s
get_range_indexes <- function(data, init_column, final_column, s = 1) {
  init <- which(names(data) == init_column)
  final <- which(names(data) == final_column)
  range <- seq(init, final,by = s)
  return(range)
}

# format and round to 3 decimal
round_3 <- function(x) {
  round(x, 3)
}
# format and round to 2 decimal
round_2 <- function(x) {
  round(x, 2)
}

# format and round to 1 decimal
round_1 <- function(x) {
  round(x, 1)
}

# format and round to 0 decimal
round_0 <- function(x) {
  #format(round(x,1),nsmall = 1)
  #format(round(as.numeric(x), 1), nsmall=1, big.mark=",") 
  round(x, 0)
}


# add object to list container. 
# to get the object name as string use deparse/substitute
add_object <- function(l, obj) {
  
  i <- length(l)
  index <- i + 1
  l[[index]] <- obj
  names(l)[index] <- deparse(substitute(obj))
  return(l)
  
}

# clean global environment and keep functions and list containers
clean_env <- function() {
  
  rm(list = setdiff(ls(envir=.GlobalEnv), c(grep("list$", ls(envir=.GlobalEnv), value = TRUE), lsf.str(envir=.GlobalEnv))), envir = .GlobalEnv)
  
}

# get month name from integer number
month_name <- function(m) {
  name <- month.name[m]
  return(name)
}

# make Excel text separator for openxlsx 
make_row_text <- function(comment) {
  row_text <- as.data.frame(c(comment))
  names(row_text) <- NULL
  return(row_text)
}

# # type convert to numeric
# to_numeric <- function(x) {
#   type.convert(as.character(x), as.is = TRUE)
# }

# type convert to numeric
to_numeric <- function(x) {
  as.numeric(type.convert(as.character(x), as.is = TRUE))
}

# type convert to numeric only those columns which contain numeric values
type_convert <- function(x) {
  type.convert(as.character(x), as.is = TRUE)
}

# use type conver to soft coerce a whole data frame
soft_coerce <- function(df) {
  df[] <- lapply(df , type_convert)
  return(df)
}

# get total for a given numeric field in a dataframe
get_total <- function(df, field) {
  total <- sum(to_numeric(df[,field]), na.rm = TRUE)
  return(total)
}

#output a vector as a csv por cut and paste purposes
vector_to_csv <- function(v, path) {
  write_csv(as.data.frame(v), path)
}

#output column names of a dataframe as a csv por cut and paste purposes
names_to_csv <- function(df, path) {
  write_csv(as.data.frame(names(df)), path)
}

# regex match for a vector of different patterns
check_pattern_vector <- function(pattern_vector, target) {
 output <- mapply(grepl, pattern_vector, target)
 return(output)
}

# check if value is within range
check_in_range <- function(value, range) {
  check <- (value >= range[1] & value <= range[2])
  return(check)
}

# remove pattern from column names
remove_col_pattern <- function(df, pattern) {
  col_names <- gsub(pattern, "", names(df))
  names(df) <- col_names
  return(df)
}

# breakline words in labels
break_labels <- function(x) {
    gsub("\\s", "\n", x)
}

# integer percentage format
percent_0 <- function(df) {
  label_percent(accuracy = 1)(df)
}

# integer percentage format
percent_1 <- function(df) {
  label_percent(accuracy = 0.1)(df)
}

# remove dots from agregated data frame column names and change them by spaces
remove_col_dots <- function(df) {
  names(df) <- trimws(gsub("\\.", " ", names(df)))
  return(df)
}

# wrap labels at 20 character
break_20 <- function(label) {
  str_wrap(label, width = 20)
  return(label)
}

# remove dots from vector
remove_dots <- function(x) {
  out <- trimws(gsub("\\.", " ", x))
  return(out)
}

# format today suffix as YYYYMMDD
suffix_today <- function() {
  out <- str_replace_all(as.character(today()), "-", "")
  return(out)
}

# format in thousands comma
thousand_format <- function (x) {
  number_format(accuracy = 1, big.mark = ",")(x)
}