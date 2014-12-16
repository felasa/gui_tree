require(DBI)
require(RSQLite)
agrega_hermano <- function(row_id, sexo,  nombre, apellido, fecha_nacimiento, vive) {
  query <- paste0("SELECT padre_id, madre_id, pedigree_id FROM personas WHERE rowid = ", row_id)
  result <- dbGetQuery(con, query)
  padres <- as.vector(result[1, c("padre_id", "madre_id")])
  ped_id <- result$pedigree_id
  query <- paste0( "INSERT into personas (padre_id, madre_id, pedigree_id, sexo_rowid, nombre, apellido, fecha_nacimiento, vive_rowid) VALUES (", 
                   padres[1],
                   ",", 
                   padres[2], 
                   ",", 
                   ped_id,
                   ",",
                   sexo,
                   ",",
                   to_char(nombre),
                   ",",
                   to_char(apellido),
                   ",",
                   to_char(fecha_nacimiento),
                   ",",
                   vive,
                   ")" 
  )
  dbSendQuery(con, query)    
}
