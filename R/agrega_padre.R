agrega_padre <- function(row_id, nombre, apellido, fecha_nacimiento, vive) {
  
  #checar si ya existe padre.
  query <- paste0("SELECT padre_id from personas WHERE personas.rowid = ", row_id)
  result <- dbGetQuery(globals$con, query)
  
  if (!is.na(result$padre_id)) stop("Ya se registró padre")
  
  #ids correspondientes al individuo activo
  query <- paste0("SELECT pedigree_id FROM personas WHERE rowid = ", row_id )
  result <- dbGetQuery(globals$con, query)
  ped_id <- result$pedigree_id
  
  #insertar entrada correspondiente al padre
  query <- paste0( "INSERT into personas (sexo_rowid, ref, pedigree_id, nombre, apellido, fecha_nacimiento, vive_rowid) VALUES 
                   (1, ", 
                   row_id,
                   ", ",
                   ped_id,
                   ", ", 
                   to_char(nombre),
                   ", ", 
                   to_char(apellido),
                   ", ", 
                   to_char(fecha_nacimiento),
                   ", ", 
                   vive,
                   ")"
                   )  
  dbSendQuery(globals$con, query)
  
  query <- paste("SELECT rowid FROM personas WHERE ref = ", row_id )
  padre_id <- dbGetQuery(globals$con, query)
  padre_id <- padre_id$rowid
  
  #BORRAR ref_id (necesario?)
  dbGetQuery(globals$con, paste0("UPDATE personas SET ref = NULL WHERE rowid = ", padre_id) )  
  
  #Agregar referencia en individuos
  query <- paste0("UPDATE personas SET padre_id = ", padre_id, " WHERE rowid = ", row_id)
  dbSendQuery(globals$con, query)
}