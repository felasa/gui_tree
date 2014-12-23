## INSERTAR LO NECESARIO PARA TENER UNA NUEVA FAMILIA: UNA LINEA EN LA TABLA PEDIGREES Y
## OTRA CON DATOS DEL CASO PRINCIPAL EN LA TABLA PERSONAS
nuevo_pedigree <- function(expediente, nombre, apellido, sexo, vive, fecha_nacimiento ) {
  
  if (!is.character(expediente)) stop("expediente debe ser clase character")
  expediente <- to_char(expediente)
  # checar si ya existe 
  query <- paste("SELECT COUNT(*) FROM personas WHERE expediente =", expediente)
  cuenta  <- dbGetQuery(con, query)  
  if (cuenta > 0) stop("ya existe pedigree con ese expediente")
  
  #Inserta el expediente en personas
  query <- paste("INSERT INTO personas (expediente, nombre, apellido, sexo_rowid, vive_rowid, fecha_nacimiento) 
                 VALUES (", 
                 expediente,
                 ",",
                 to_char(nombre),
                 ",",
                 to_char(apellido),
                 ",", 
                 sexo,
                 ",",
                 vive,
                 ",", 
                 to_char(fecha_nacimiento),
                 ")")
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
