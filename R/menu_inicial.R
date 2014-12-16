#MENU: SELECCIONAR UNA CARPETA Y UNA BASE DE DATOS

menu_inicial <- function() {
  require(gWidgets)
  require(DBI)
  require(RSQLite)
  ##
  window <- gwindow( "Abrir BD", visible= FALSE, horizontal = FALSE )
  
  ## Seleccion archivo. o Nuevo
  group <- ggroup(cont = window , horizontal = FALSE )
  glabel( "Archivo BD:", cont = group, anchor = c(-1, 0) )
  start_dir <- gfilebrowse(text = "Seleccione archivo" ,
                           quote = FALSE,
                           type = "open", 
                           cont = group )
  boton_abrir <- gbutton("Abrir" , cont = group )
  #addSpring(group)
  #group2 <- ggroup(cont = window , horizontal = FALSE )
  glabel( "Crear Nuevo:", cont = group, anchor = c(-1, 0) )
  new_button <- gbutton("Nuevo \n (pendiente)", cont= group)
  #addSpring(group2)  
  
  addHandlerChanged( boton_abrir , handler = function(h , ... ) {  
    db_file_name <<- svalue(start_dir)
    dispose(window)
    abrir_base(db_file_name)
  } )
  ## hacer visible la ventana
  visible( window ) <- TRUE
}
