nueva_base <- function(filename) {
  
  if (class(filename) != "character") stop("Nombre de archivo mal")
  globals$con <- dbConnect(SQLite(), dbname = filename )
  dbSendQuery(globals$con, "CREATE TABLE personas
  (
  expediente varchar(255),
  nombre varchar(255),
  apellido varchar(255),
  sexo_rowid int,
  fecha_nacimiento TEXT,
  vive_rowid int,
  padre_id int,
  madre_id int,
  pedigree_id int,
  ref int,
  FOREIGN KEY(pedigree_id) REFERENCES pedigrees(rowid), 
  FOREIGN KEY(sexo_rowid) REFERENCES sexo(rowid),
  FOREIGN KEY(vive_rowid) REFERENCES vive(rowid)
  );")
  
  dbSendQuery(globals$con, "CREATE TABLE pedigrees (
           probate_id int,
           FOREIGN KEY(probate_id) REFERENCES personas(rowid));")
  
  dbSendQuery(globals$con, "CREATE TABLE sexo
  (
  sexo varchar(255)
  );")
  
  dbSendQuery(globals$con, "INSERT INTO sexo (sexo) VALUES ('Hombre')")
  dbSendQuery(globals$con, "INSERT INTO sexo (sexo) VALUES ('Mujer')")
  
  dbSendQuery(globals$con, "CREATE TABLE vive
  (
  vive varchar(255)
  );")
  
  
  dbSendQuery(globals$con, "INSERT INTO vive (vive) VALUES ('Si')")
  dbSendQuery(globals$con, "INSERT INTO vive (vive) VALUES ('No')")
  
 

dbSendQuery(globals$con, "CREATE TABLE condiciones
  (
  condicion varchar(255)
  );")

dbSendQuery(globals$con, "CREATE TABLE condind
  (
  individuo_id int,
  condicion_id int,
  FOREIGN KEY(individuo_id) REFERENCES personas(rowid), 
  FOREIGN KEY(condicion_id) REFERENCES condiciones(rowid)
  );")

  #sqliteCloseConnection(globals$con)
  dbDisconnect(globals$con)

}