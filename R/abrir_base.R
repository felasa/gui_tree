abrir_base <- function(filename) {
  if (is_valid_db(filename)) {
    con <<- dbConnect(SQLite(), dbname = filename )    
    menu_familia()
  }  
  else menu_inicial()
}

is_valid_db <- function(filename) {
  # PENDIENTE IMPLEMENTAR REGRESA TRUE SIEMPRE
  return(TRUE)
}