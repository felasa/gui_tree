dialogo_nuevo <- function() {
  window <- gwindow("Nueva Familia", visible = FALSE, horizontal = TRUE)
  glabel("Expediente:", cont = window)
  campo_expediente <- gedit("", initial.msg="Expediente...", cont = window)
  glabel("Nombre:", initial.msg="Nombre", cont = window)
  campo_nombre <- gedit("", initial.msg="Nombre...", cont = window)
  glabel("Apellido", cont = window)
  campo_apellido <- gedit("", initial.msg = "Apellido", cont = window)
  
  group <- ggroup(con=window)
  glabel("Fecha de Nacimiento: \n (dd/mm/aaaa)", cont = group)
  campo_fnacimiento <- gedit("",initial.msg="dd/mm/aaaa", cont = group)
  addSpring(group)
  glabel("Sexo", cont = group) 
  combo_sexo <- gcombobox ( c ( "Hombre","Mujer", NA ) , cont = group )  
  addSpring(group)
  boton_crear <- gbutton("OK", cont = window)  
  visible(window) <- TRUE
  
  addHandlerChanged(boton_crear, handler = function(h, ...) {
    # if(validar())
    expediente <- svalue(campo_expediente)
    nombre <- svalue(campo_nombre)
    apellido <- svalue(campo_apellido)
    fecha_nacimiento <- svalue(campo_fnacimiento)
    sexo <- ifelse(svalue(combo_sexo)=="Hombre",1,2)
    #cat(expediente, nombre, apellido, fecha_nacimiento, sexo)
    id <- nuevo_pedigree(expediente, nombre, apellido, sexo, vive=1, fecha_nacimiento)
    abrir_familia(id)
  })
  
}