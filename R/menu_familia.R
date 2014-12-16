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