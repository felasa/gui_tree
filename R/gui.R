require(gWidgets)
#options(guiToolkit="tcltk")
#options(guiToolkit="RGtk2")

##UI


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


#MENU: NUEVA FAMILIA, ABRIR FAMILIA

menu_familia <- function() {
  #OBTIENE DE ANTEMANO UNA LISTA DE EXPEDIENTES
  query <- "SELECT expediente, pedigrees.rowid as 'ped_id' from personas join pedigrees on personas.rowid = pedigrees.probate_id"
  lista_pedigrees <- dbGetQuery(con , query)
  
  ## CREAR VENTANA, ETC
  window <- gwindow("Inicio", visible=FALSE)  
  paned <- gpanedgroup( cont = window )
  group <- ggroup(cont = paned, horizontal = FALSE)
  glabel("Expediente:", cont=group, anchor=c(-1,0))
  campo_expediente <- gedit("", initial.msg = "Expediente",
                            cont= group)
  boton_abrir <- gbutton("Abrir", cont = group)
  #addSpring(group)
  boton_nuevo <- gbutton("Nuevo", cont = group)
  visible(window) <- TRUE
  
  ## ACCION AL APRETAR BOTON
  addHandlerChanged(boton_abrir, 
                    handler = function(h, ...) {
                      expediente <- svalue(campo_expediente)
                      ## SI YA EXISTE ESE REGISTRO EN LA BASE
                      if (expediente %in% lista_pedigrees$expediente) {
                        ind <- which(lista_pedigrees$expediente == expediente)
                        pedigree_id <- lista_pedigrees$ped_id[ind]
                        dispose(window)
                        abrir_familia(pedigree_id)
                      ## SI NO EXISTE EXPEDIENTE
                      } else {
                        ficha_nuevo()
                        id_nuevo_pedigree <- nuevo_pedigree(expediente)
                        dispose(window)
                        abrir_familia(id_nuevo_pedigree)
                      }
                      
    
  })
  
  addHandlerChanged(boton_nuevo,
                    handler = function(h, ...) {
                      dialogo_nuevo()
                    })
}


### NUEVA FAMILIA: CAMPO EXPEDIENTE, DATOS BÁSICOS, BOTON CREAR, ABRIR FAMILIA


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
    sexo <- svalue(combo_sexo)
    cat(expediente, nombre, apellido, fecha_nacimiento, sexo)
    id <- nuevo_pedigree(expediente, nombre, apellido, sexo, vive=1, fecha_nacimiento)
    abrir_familia(id)
  })
  
}



### FAMLIA ABIERTA: LISTAR MIEMBROS, DIBUJAR PEDIGREE.

abrir_familia <- function(pedigree_id) {
    
    #env_abrir_familia <- new.env(parent = emptyenv())
    paste0("SELECT personas.rowid, *  
    FROM 
    personas
    JOIN
    vive
    ON vive_rowid = vive.rowid
    JOIN
    sexo
    ON
    sexo_rowid = sexo.rowid
    WHERE pedigree_id = ", pedigree_id) -> query
    
    
    #query <- paste0("SELECT rowid, * FROM PERSONAS WHERE pedigree_id =", pedigree_id)
    personas <- dbGetQuery(con , query)
    
    
    window <- gwindow("Familia", visible=FALSE, horizontal = FALSE)
    frame <- gframe(cont = window, horizontal=FALSE)
    paned <- gpanedgroup( cont = frame )
    
    #individuos <- gtable(personas[ ,c("nombre","apellido","sexo","vive","brca","p53","cancer") ], cont=window)
    individuos <- gtable(personas[ ,c("rowid","nombre","apellido","sexo","fecha_nacimiento" ,"vive") ], cont = frame, expand=TRUE, visible=FALSE)
    group <- ggroup(cont = frame, horizontal = TRUE)
    boton_agregar_padre <- gbutton("Agregar padre", cont=group)
    boton_agregar_madre <- gbutton("Agregar madre", cont= group)
    boton_agregar_hijo <- gbutton("Agregar hijo", cont=group)
    boton_agregar_hermano <- gbutton("Agregar hermano(a)", cont=group)
    boton_agregar_pareja <- gbutton("Agregar pareja", cont=group)
    boton_grafica_arbol <- gbutton("Ver árbol", cont=group)
    boton_regresar <- gbutton("Regresar", cont=group)
    
    
    #img <- tkrplot(getToolkitWidget(frame), fun = function() {
    #  plot.pedigree(pedigree, col=c("red3", rep("black",dim(pedigree)-1)))
    #  #pedigree.legend(pedigree, location="bottomleft", radius=.2)
    #} )
    #add(frame, img)
    visible(window) <- TRUE
    visible(individuos)<-TRUE
    
    ##AGREGAR PADRE 
    
    addHandlerClicked(boton_agregar_padre, handler=function(h, selected=svalue(individuos), origen = pedigree_id,  ...) {
      
      nuevo_individuo(pariente_rowid=selected, tipo="padre", origen)
      dispose(window)      
   
    })
    
    addHandlerClicked(boton_agregar_madre, handler=function(h, selected=svalue(individuos), origen = pedigree_id,  ...) {
      
      nuevo_individuo(pariente_rowid=selected, tipo="madre", origen)
      dispose(window)      
      
    })
    
    addHandlerClicked(boton_agregar_hermano, handler=function(h, selected=svalue(individuos), origen = pedigree_id,  ...) {      
      nuevo_individuo(pariente_rowid=selected, tipo="hermano", origen)
      dispose(window)      
      
    })
    
    addHandlerChanged(boton_grafica_arbol, 
                      handler = function(h, ...) {
                        #add(frame, img)
                        #tkrreplot(img)
                        require(kinship2)
                        pedigree <- pedigree(id = personas$rowid, 
                                             dadid = personas$padre_id,
                                             momid = personas$madre_id,
                                             sex = personas$sexo_rowid,
                                             famid = personas$pedigree_id,
                                             status = (personas$vive_rowid-1)
                                             ##affected = as.matrix(personas[,c("brca","p53","cancer")])                              
                        )[1]
                        x11()
                        plot.pedigree(pedigree, col=c("red3", rep("black",dim(pedigree)-1)))
                        pedigree.legend(pedigree, location="bottomleft", radius=.2) 
                      })
    addHandlerChanged(boton_regresar, 
                      handler = function(h, ...) {
                        #dev.off()
                        dispose(window)
                        menu_familia()                        
                      }) 
    addHandlerChanged(individuos, handler=function(h,...) {
      cat(svalue(individuos))
    })
    
}

### FUNCIONES AUXILIARES

##CONECTARSE A LA BBDD
abrir_base <- function(filename) {
  if (is_valid_db(filename)) {
    con <<- dbConnect(SQLite(), dbname = filename )    
    menu_familia()
  }  
  else menu_inicial()
}

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
        cont= grupo1)
  glabel("Apellido:", cont = grupo1, anchor = c(-1, 0) ) 
  campo_apellido <- gedit("", initial.msg = "Apellido",
                      cont= grupo1)
  
  glabel("Fecha de nacimiento:", cont = grupo1, anchor = c(-1, 0) ) 
  campo_fecha_nacimiento <- gedit("", initial.msg = "Fecha Nacimiento",
                                  cont = grupo1)
  
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
                      print(valores)
                      if (tipo == "padre") { 
                        agrega_padre(row_id = rowid, nombre = valores[["nombre"]], 
                                    apellido = valores[["apellido"]], fecha_nacimiento = valores[["fecha_nacimiento"]],
                                    vive = valores[["vive"]])
                      }
                      
                      if (tipo == "madre") {
                        agrega_madre(row_id = rowid, nombre = valores[["nombre"]], 
                                     apellido = valores[["apellido"]], fecha_nacimiento = valores[["fecha_nacimiento"]],
                                     vive = valores[["vive"]])
                        
                      }
                      
                      if (tipo == "hermano") {
                        agrega_hermano(row_id = rowid, nombre = valores[["nombre"]], 
                                     apellido = valores[["apellido"]], sexo=valores[["sexo"]],
                                     fecha_nacimiento = valores[["fecha_nacimiento"]],
                                     vive = valores[["vive"]])
                        
                      }
                      abrir_familia(origen)
                      dispose(window_ficha)
                      
  })
  
  
  visible(window_ficha) <- TRUE
}

## REVISAR SI EL ARCHVOS ES UNA BASE DE DATOS SQLite
is_valid_db <- function(filename) {
  # PENDIENTE IMPLEMENTAR REGRESA TRUE SIEMPRE
  return(TRUE)
}