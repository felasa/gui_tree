agregar_condiciones <- function(rowid) {
  lista <- ""
  condiciones_ventana <- gwindow("Condiciones", visible=FALSE)
  condiciones_grupo  <- ggroup(cont = condiciones_ventana)
  "SELECT condicion FROM condiciones" -> query
  
  res <- dbGetQuery(globals$con, query)$condicion
  if (length(res >0 )) lista <- c("",res)
  condiciones_combo <- gcombobox(lista, cont=condiciones_grupo)
  glabel("Otra:", cont=condiciones_grupo)
  campo_otra <- gedit("", initial.msg="Otra condicion", cont=condiciones_grupo)
  condiciones_ok <- gbutton("OK", cont = condiciones_grupo)
  visible(condiciones_ventana) <- TRUE
  
  addHandlerClicked(condiciones_ok, handler = function(h, ...) {
    condicion1 <- svalue(condiciones_combo)
    condicion2 <- svalue(campo_otra)
    
    if (condicion1 != "") {
      #query1 <- paste("INSERT INTO condiciones (condicion) VALUES (", to_char(condicion1), ")" )
      #dbSendQuery(con, query1)
      query1<- paste("SELECT rowid FROM condiciones WHERE condicion =", to_char(condicion1))
      cond_id <- dbGetQuery(globals$con, query1)$rowid
      query_mez <- paste("SELECT rowid FROM condind WHERE individuo_id =", rowid, "AND condicion_id =", cond_id)
      dbGetQuery(globals$con, query_mez) -> res
      if (dim(res)[1]==0) {
        query2 <- paste("INSERT INTO 
                        condind (individuo_id, condicion_id) 
                        VALUES (",
                        rowid,
                        ",",
                        cond_id, ")")
        dbSendQuery(globals$con, query2)
        #sqliteCloseResult(dbListResults(con)[[1]])
      }
    }
    
    if (condicion2 != "") {
      #FALTARIA REVISAR QUE NO ESTE REPETIDA
      query1 <- paste("INSERT INTO condiciones (condicion) VALUES (", to_char(condicion2), ")" )
      dbSendQuery(globals$con, query1)
      query_mez <- paste("SELECT rowid FROM condiciones WHERE condicion=", to_char(condicion2))
      cond_id <- dbGetQuery(globals$con, query_mez)$rowid
      query2 <- paste("INSERT INTO 
                      condind (individuo_id, condicion_id) 
                      VALUES (",
                      rowid,
                      ",",
                      cond_id, ")")
      dbSendQuery(globals$con, query2)
      #sqliteCloseResult(dbListResults(con)[[1]])
    }

    dispose(condiciones_ventana)
  })
}