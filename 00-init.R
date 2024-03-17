
################################################################################
#                                                                              #
#                      CHANGE FIRST DIRECTORY FOLDER NAME                      #
#                                                                              #
################################################################################


# set tool name
tool_name <- gsub(".*/", "", dirname(rstudioapi::getSourceEditorContext()$path))

# rename template files
file_list <- list.files(path = here("R", tool_name), pattern = "template", full.names = TRUE)

if(length(file_list) > 0 && tool_name != "_template") {
  sapply(file_list, FUN = function(eachPath){
    file.rename(from = eachPath,to = sub(pattern = "template", replacement = tool_name, eachPath))})
}