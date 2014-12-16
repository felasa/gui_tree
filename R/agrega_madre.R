agrega_madre <- function(row_id, nombre, apellido, fecha_nacimiento, vive) {
  query <- paste0("SELECT madre_id from personas WHERE rowid = ", row_id)
  result <- dbGetQuery(con, query)
  ## CHECAR SI YA EXISTE MADRE
  if (!is.na(result$madre_id)) stop("Ya se registró madre")
  #ids correspondientes al individuo activo
  query <- paste0("SELECT pedigree_id FROM personas WHERE rowid = ", row_id )
  result <- dbGetQuery(con, query)
  ped_id <- result$pedigree_id
  
  #insertar entrada correspondiente a la madre
  
  query <- paste0( "INSERT into personas (sexo_rowid, ref, pedigree_id, nombre, apellido, fecha_nacimiento, vive_rowid) VALUES (2, ", 
                   row_id, ", ",
                   ped_id, ", ", 
                   to_char(nombre), ", ", 
                   to_char(apellido), ", ", 
                   to_char(fecha_nacimiento), ", ", vive,  ")" )  
  dbSendQuery(con, query)
  
  
  query <- paste("SELECT rowid FROM personas WHERE ref = ", row_id )
  madre_id <- dbGetQuery(con, query)
  madre_id <- madre_id$rowid
  
  #BORRAR ref_id (necesario?)
  dbGetQuery(con, paste0("UPDATE personas SET ref = NULL WHERE rowid = ", madre_id) )  
  
  #Agregar referencia en individuos
  query <- paste0("UPDATE personas SET madre_id = ", madre_id, " WHERE rowid = ", row_id)
  dbGetQuery(con, query)
}