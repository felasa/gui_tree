require(gWidgets)
nuevo_individuo <- function(pariente_rowid, tipo, origen) {
  # SI existen datos
  #query <- paste0("SELECT expediente, nombre, apellido, fecha_nacimiento, sexo, vive FROM personas WHERE rowid =", 1)
  #datos <- dbGetQuery(con, query)
  window_ficha <- gwindow("FICHA", visible=FALSE, horizontal=TRUE)  
  paned <- gpanedgroup(cont = window_ficha, horizontal=TRUE)
  grupo1 <- ggroup(cont = paned, horizontal=FALSE)
  #   
  #   glabel("Expediente:", cont = grupo1, anchor = c(-1, 0) ) 
  #   campo_expediente <- gedit("", initial.msg = "Expediente",
  #                         cont= grupo1, expand=TRUE)
  
  glabel("Nombre(s):", cont = grupo1, anchor = c(-1, 0) ) 
  campo_nombre <- gedit("", initial.msg = "Nombre",
                        cont= grupo1, coerce.with=toupper)
  glabel("Apellido:", cont = grupo1, anchor = c(-1, 0) ) 
  campo_apellido <- gedit("", initial.msg = "Apellido",
                          cont= grupo1, coerce.with=toupper)
  
  glabel("Fecha de nacimiento:", cont = grupo1, anchor = c(-1, 0) ) 
  campo_fecha_nacimiento <- gedit("", initial.msg = "Fecha Nacimiento",
                                  cont = grupo1, coerce.with=valida_fecha)
  
  glabel("Sexo", cont = grupo1, anchor = c(-1, 0) ) 
  combo_sexo <- gcombobox ( c ( "Hombre","Mujer", NA ) , cont = grupo1, 
                            selected = switch(tipo,
                                              padre = 1,
                                              madre = 2, 1) )  
  
  glabel("Vive", cont = grupo1, anchor = c(-1, 0) ) 
  combo_vive <- gcombobox ( c ( "Si","No", NA ) , cont = grupo1 )  
  addSpring(grupo1)
  boton_actualizar <- gbutton("OK", cont=grupo1)
  tipo = tipo
  rowid = pariente_rowid
  origen = origen
  
  addHandlerChanged(boton_actualizar, cont = window,
                    handler = function(h,  ...) { 
                      
                      valores <- list(nombre=svalue(campo_nombre), apellido=svalue(campo_apellido),
                                      fecha_nacimiento=svalue(campo_fecha_nacimiento), sexo=ifelse(svalue(combo_sexo)=="Hombre",1,2),
                                      vive = ifelse(svalue(combo_vive)=="Si",1,2))
                      #print(valores)
                      if (tipo == "padre") { 
                        agrega_padre(row_id = rowid, nombre = valores[["nombre"]], 
                                     apellido = valores[["apellido"]], fecha_nacimiento = valores[["fecha_nacimiento"]],
                                     vive = valores[["vive"]])
                        dispose(window_ficha)                        
                      }
                      
                      if (tipo == "madre") {
                        agrega_madre(row_id = rowid, nombre = valores[["nombre"]], 
                                     apellido = valores[["apellido"]], fecha_nacimiento = valores[["fecha_nacimiento"]],
                                     vive = valores[["vive"]])
                        dispose(window_ficha)                      
                      }
                      
                      if (tipo == "hermano") {
                        agrega_hermano(row_id = rowid, nombre = valores[["nombre"]], 
                                       apellido = valores[["apellido"]], sexo=valores[["sexo"]],
                                       fecha_nacimiento = valores[["fecha_nacimiento"]],
                                       vive = valores[["vive"]])    
                        dispose(window_ficha)
                      }
                      
                      if (tipo == "hijo") {
                        agrega_hijo(row_id = rowid, nombre = valores[["nombre"]], 
                                    apellido = valores[["apellido"]], sexo=valores[["sexo"]],
                                    fecha_nacimiento = valores[["fecha_nacimiento"]],
                                    vive = valores[["vive"]], ped_id=origen)                        
                      }                    
                      
                      dispose(window_ficha)
                    })
  
  visible(window_ficha) <- TRUE
}