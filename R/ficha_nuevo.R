require(gWidgets)
ficha_nuevo <- function(expediente) {
  #VENTANA MODAL?
  window <- gwindow("NUEVO CASO", visible = FALSE, horizontal=TRUE) 
  ##CAMPOS
  paned <- gpanedgroup(cont=window, horizontal=FALSE)
  grupo <- ggroup(cont = paned, horizontal=FALSE)
  glabel("Nombre", cont=grupo, anchor=c(-1,0))
  campo_nombre <- gedit("", initial.msg="Nombre", cont=grupo)
  glabel("Apellido", cont=grupo)
  
  ##BOTONES ACEPTAR CANCELAR
  ##LLAMAR FUNCION nuevo_pedigree
  
}