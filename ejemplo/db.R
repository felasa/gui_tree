require(DBI)
require(RSQLite)

to_char <- function(char) {
  char <- paste0("'",char,"'")
  return(char)
}

nuevo_pedigree <- function(expediente) {
  
  if (!is.character(expediente)) stop("expediente debe ser clase character")
  expediente <- to_char(expediente)
  # checar si ya existe 
  query <- paste("SELECT COUNT(*) FROM personas WHERE expediente =", expediente)
  cuenta  <- dbGetQuery(con, query)
  
  if (cuenta > 0) stop("ya existe pedigree con ese expediente")
  
  #Inserta el expediente en personas
  query <- paste0("INSERT INTO personas (expediente) VALUES (", expediente, ")")
  dbGetQuery(con, query)
  
  #obtiene el id
  query <- paste("SELECT rowid FROM personas WHERE expediente =", expediente)
  id1 <- dbGetQuery(con, query)
  
  #Inserta en pedigrees id correspondiente al probate
  query <- paste("INSERT INTO pedigrees (probate_id) VALUES (", id1, ")")
  dbGetQuery(con, query)
  
  #obtener id del pedigree
  query <- paste("SELECT rowid FROM pedigrees WHERE probate_id =", id1)
  id2 <- dbGetQuery(con, query)
  
  #Inserta en personas id correspondiente al pedigree
  query <- paste("UPDATE personas SET pedigree_id =", id2, "WHERE rowid = ", id1)  
  dbGetQuery(con, query)
  return(id2)
}

agrega_padre <- function(row_id) {
  
  #checar si ya existe padre.
  query <- paste0("SELECT padre_id from personas WHERE rowid = ", row_id)
  result <- dbGetQuery(con, query)
  if (!is.na(result$padre_id)) stop("Ya se registró padre")
  
  #ids correspondientes al individuo activo
  query <- paste0("SELECT pedigree_id FROM personas WHERE rowid = ", row_id )
  result <- dbGetQuery(con, query)
  ped_id <- result$pedigree_id
  
  #insertar entrada correspondiente al padre
  query <- paste0( "INSERT into personas (sexo, ref, pedigree_id) VALUES ('H', ", row_id, ",", ped_id, ")" )  
  dbGetQuery(con, query)
  
  query <- paste("SELECT rowid FROM personas WHERE ref = ", row_id )
  padre_id <- dbGetQuery(con, query)
  padre_id <- padre_id$rowid
  
  #BORRAR ref_id (necesario?)
  dbGetQuery(con, paste0("UPDATE personas SET ref = NULL WHERE rowid = ", padre_id) )  
  
  #Agregar referencia en individuos
  query <- paste0("UPDATE personas SET padre_id = ", padre_id, " WHERE rowid = ", row_id)
  dbGetQuery(con, query)
}

agrega_madre <- function(row_id) {
  query <- paste0("SELECT madre_id from personas WHERE rowid = ", row_id)
  result <- dbGetQuery(con, query)
  ## CHECAR SI YA EXISTE MADRE
  if (!is.na(result$madre_id)) stop("Ya se registró madre")
  #ids correspondientes al individuo activo
  query <- paste0("SELECT pedigree_id FROM personas WHERE rowid = ", row_id )
  result <- dbGetQuery(con, query)
  ped_id <- result$pedigree_id
  
  #insertar entrada correspondiente al padre
  query <- paste0( "INSERT into personas (sexo, ref, pedigree_id) VALUES ('M', ", row_id, ",", ped_id, ")" )  
  dbGetQuery(con, query)
  
  query <- paste("SELECT rowid FROM personas WHERE ref = ", row_id )
  madre_id <- dbGetQuery(con, query)
  madre_id <- madre_id$rowid
  
  #BORRAR ref_id (necesario?)
  dbGetQuery(con, paste0("UPDATE personas SET ref = NULL WHERE rowid = ", madre_id) )  
  
  #Agregar referencia en individuos
  query <- paste0("UPDATE personas SET madre_id = ", madre_id, " WHERE rowid = ", row_id)
  dbGetQuery(con, query)
}

#Checar si hay algun padre o madre en el individuo
tiene_padre <- function(id) {
  if (!is.numeric(id) | length(id) != 1 ) stop("Error")  
  query <- paste0("SELECT padre_id, madre_id FROM personas WHERE rowid = ", id)
  result <- dbGetQuery(con, query)
  return(!all(is.na(result)))
}


agrega_hermano <- function(id, sexo) {
  if (!tiene_padre(id)) stop("Registre padres") #Agregar automatico si no existen datos vacios?
  query <- paste0("SELECT padre_id, madre_id, pedigree_id FROM personas WHERE rowid = ", id)
  result <- dbGetQuery(con, query)
  padres <- as.vector(result[1, c("padre_id", "madre_id")])
  ped_id <- result$pedigree_id
  query <- paste0( "INSERT into personas (padre_id, madre_id, pedigree_id, sexo) VALUES (", padres[1],
                   ",", 
                   padres[2], 
                   ",", 
                   ped_id,
                   ",",
                   to_char(sexo),
                   ")" 
                   )
  dbGetQuery(con, query)  
}

agrega_hijo <- function(id, sexo) {
  query <- paste0("SELECT sexo FROM personas WHERE rowid =", id)
  result <- dbGetQuery(con, query)$sexo
  columna <- ifelse(result==1, "padre_id", "madre_id")
  sexo_padre <- 
  query <- paste0("INSERT INTO personas (", columna,
                  ",", 
                  "sexo",")", " VALUES (", id, sexo, ")")
  dbGetQuery(con, query)
  
}