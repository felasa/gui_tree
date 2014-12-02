require(gWidgets)
##UI


#MENU: SELECCIONAR UNA CARPETA Y UNA BASE DE DATOS

menu_inicial <- function() {
  require(gWidgets)
  require(DBI)
  require(RSQLite)
  ##
  window <- gwindow( "Abrir BD" , visible= FALSE )
  #paned <- gpanedgroup( cont = window )
  
  ## Seleccion archivo. o Nuevo
  group <- ggroup( cont = window , horizontal = FALSE )
  glabel( "Archivo BD:" , cont = group, anchor = c(-1, 0) )
  start_dir <- gfilebrowse(text = "Seleccione archivo" ,
                           quote = FALSE,
                           type = "open", 
                           cont = group )
  boton_abrir <- gbutton("Abrir" , cont = group )
  addSpring(group)
  group2 <- ggroup( cont = window , horizontal = FALSE )
  new_button <- gbutton("   Nuevo    \n (pendiente)", cont= group2)
  addSpring(group2)
  
  
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
  boton_ir <- gbutton("Crear / Abrir" , cont = group )
  addSpring(group)
  visible(window) <- TRUE
  
  ## ACCION AL APRETAR BOTON
  addHandlerChanged(boton_ir, 
                    handler = function(h, ...) {
                      expediente <- svalue(campo_expediente)
                      if (expediente %in% lista_pedigrees$expediente) {
                        ind <- which(lista_pedigrees$expediente == expediente)
                        pedigree_id <- lista_pedigrees$ped_id[ind]
                        dispose(window)
                        abrir_familia(pedigree_id)
                      } else {
                        id_nuevo_pedigree <- nuevo_pedigree(expediente)
                        dispose(window)
                        abrir_familia(id_nuevo_pedigree)
                      }
                      
    
  })
}


### NUEVA FAMILIA: CAMPO EXPEDIENTE, DATOS BÁSICOS, BOTON CREAR, ABRIR FAMILIA



### FAMLIA ABIERTA: LISTAR MIEMBROS, DIBUJAR PEDIGREE.

abrir_familia <- function(pedigree_id) {
    require(tkrplot)
    require(kinship2)
    query <- paste0("SELECT rowid, * FROM PERSONAS WHERE pedigree_id =", pedigree_id)
    personas <- dbGetQuery(con , query)
    pedigree <- pedigree(id = personas$rowid, 
                         dadid = personas$padre_id,
                         momid = personas$madre_id,
                         sex = ifelse(personas$sexo=="H",1,2),
                         famid = personas$pedigree_id,
                         status = personas$vive,
                         affected = as.matrix(personas[,c("brca","p53","cancer")])                              
                         )[1]
    
    window <- gwindow("Familia", visible=FALSE)
    individuos <- gtable(personas[ ,c("nombre","apellido","sexo","vive","brca","p53","cancer") ], cont=window)
    paned <- gpanedgroup( cont = window )
    group <- ggroup(cont = paned, horizontal = TRUE)
    boton_agregar_padre <- gbutton("Agregar padre", cont=group)
    boton_agregar_madre <- gbutton("Agregar madre", cont= group)
    boton_agregar_hijo <- gbutton("Agregar hijo", cont=group)
    boton_agregar_hermano <- gbutton("Agregar hermano(a)", cont=group)
    boton_agregar_pareja <- gbutton("Agregar pareja", cont=group)
    boton_grafica_arbol <- gbutton("Ver árbol", cont=group)
    boton_regresar <- gbutton("Regresar", cont=group)
    
    frame <- gframe(cont=window)
    img <- tkrplot(getToolkitWidget(frame), fun = function() {
      plot.pedigree(pedigree, col=c("red3", rep("black",dim(pedigree)-1)))
      #pedigree.legend(pedigree, location="bottomleft", radius=.2)
    } )
    add(frame, img)
    visible(window) <- TRUE
    addHandlerChanged(boton_grafica_arbol, 
                      handler = function(h, ...) {
                        add(frame, img)
                        tkrreplot(img)
                        x11()
                        plot.pedigree(pedigree, col=c("red3", rep("black",dim(pedigree)-1)))
                        pedigree.legend(pedigree, location="bottomleft", radius=.2) 
                      })
    addHandlerChanged(boton_regresar, 
                      handler = function(h, ...) {
                        dev.off()
                        dispose(window)
                        menu_familia()                        
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

## REVISAR SI EL ARCHVOS ES UNA BASE DE DATOS SQLite
is_valid_db <- function(filename) {
  # PENDIENTE IMPLEMENTAR REGRESA TRUE SIEMPRE
  return(TRUE)
}