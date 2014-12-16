agrega_hijo <- function(row_id, nombre, apellido, sexo, fecha_nacimiento, vive, pid, mid, ped_id) {
  query <- paste0("SELECT sexo_rowid FROM personas WHERE rowid = ", row_id)
  result <- dbGetQuery(con, query)$sexo_rowid
  columna <- ifelse(result==1, "padre_id", "madre_id")
  query <- paste0("INSERT INTO personas (", columna,", sexo_rowid, nombre, apellido,fecha_nacimiento, vive_rowid, pedigree_id)  VALUES (", 
                  row_id,
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
                  ",",
                  ped_id,
                  ")")
  dbSendQuery(con, query)
  
}