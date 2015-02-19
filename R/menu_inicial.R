#MENU: SELECCIONAR UNA CARPETA Y UNA BASE DE DATOS

menu_inicial <- function() {
  require(gWidgets)
  require(DBI)
  require(RSQLite)
  ##
  menu_window <- gwindow( "Abrir BD", visible= FALSE, horizontal = FALSE )
  paned <- gpanedgroup(container = menu_window, horizontal=FALSE)
  ## Seleccion archivo. o Nuevo
  group <- ggroup(cont = paned , horizontal = FALSE )
  glabel( "Archivo BD:", cont = group, anchor = c(-1, 0) )
  start_dir <- gfilebrowse(text = "Seleccione archivo" ,
                           quote = FALSE,
                           type = "open", 
                           cont = group )
  boton_abrir <- gbutton("Abrir" , cont = group )
  #addSpring(group)
  group2 <- ggroup(cont = paned , horizontal = FALSE )
  glabel( "Crear Nuevo:", cont = group2, anchor = c(-1, 0) )
  browse_button <- gfilebrowse(text="Elija ubicacion ... ", quote = FALSE, type = "selectdir", container = group2)
  glabel("Nombre", container = group2, anchor = c(-1,0))
  campo_filename <- gedit("", initial.msg="Nombre de archivo", container=group2)
  #addSpring(group2)  
  boton_nuevo <-gbutton("Nueva base", container=group2)
  
  addHandlerChanged( boton_abrir , handler = function(h , ... ) {  
    globals$db_file_name <- svalue(start_dir)
    dispose(menu_window)
    abrir_base(globals$db_file_name)
  } )
  
  addHandlerClicked(boton_nuevo, handler = function(h, ...) {
    globals$db_file_name <- paste0(svalue(browse_button),"/", svalue(campo_filename),".sqlite")
    #cat(globals$db_file_name)
    nueva_base(globals$db_file_name)
    dispose(menu_window)
    abrir_base(globals$db_file_name)
    
  })
  ## hacer visible la ventana
  visible( menu_window ) <- TRUE
}
