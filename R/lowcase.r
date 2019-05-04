# lowcase function
lowcase <- function(df) {
  names(df) <- tolower(names(df)) %>% gsub("\\?|\\s+|\\.+|_+|\\(|\\)","",.) 
  df
}