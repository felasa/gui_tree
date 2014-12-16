### FAMLIA ABIERTA: LISTAR MIEMBROS, DIBUJAR PEDIGREE.
require(kinship2)
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
  individuos <- gtable(personas[ ,c("rowid","nombre","apellido","sexo","fecha_nacimiento" ,"vive") ], 
                       cont = frame, expand=TRUE, visible=FALSE, multiple = FALSE)
  #sexo_sel <- svalue(individuos, drop=FALSE)$sexo
  #individuos <- gdf(personas[ ,c("rowid","nombre","apellido","sexo","fecha_nacimiento" ,"vive") ], 
  #                     cont = frame, expand=TRUE)    
  
  group <- ggroup(cont = frame, horizontal = TRUE)
  boton_agregar_padre <- gbutton("Agregar padre", cont=group)
  boton_agregar_madre <- gbutton("Agregar madre", cont= group)
  boton_agregar_hijo <- gbutton("Agregar hijo", cont=group)
  boton_agregar_hermano <- gbutton("Agregar hermano(a)", cont=group)
  #boton_agregar_pareja <- gbutton("Agregar pareja", cont=group)
  boton_grafica_arbol <- gbutton("Ver árbol", cont=group)
  boton_regresar <- gbutton("Regresar", cont=group)
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
    if (!tiene_padre(selected)) {
      gmessage("Registre padres antes de ingresar hermanos") 
      #Agregar automatico si no existen datos vacios?
    } else {
    nuevo_individuo(pariente_rowid=selected, tipo="hermano", origen)
    dispose(window)      
    }
  })
  
  # Un caso en el cual esto no sirve es si se agrega 
  # un hijo sin agregar a la pareja y despues se agrega un hermano u otro hijo del mismo padre
  addHandlerClicked(boton_agregar_hijo, handler=function(h, selected=svalue(individuos), origen = pedigree_id, ...) {
    # Existen hijos?       
    query <- paste("SELECT rowid FROM personas WHERE padre_id = ", selected, " OR madre_id = ", selected)
    result <- dbGetQuery(con, query)
    if (nrow(result) > 0) {
      # Supone que comparten ambos padres
      nuevo_individuo(pariente_rowid=result$rowid[1], tipo="hermano", origen)
      dispose(window)
    } else {
      faltante <- switch(sexo_sel, Hombre = "la madre", Mujer = "el padre")
      #cat(faltante)
      #gmessage(paste("Ingrese datos de ", tipo))
      nuevo_individuo(pariente_rowid = selected, tipo = "hijo", origen)
      #gmessage(paste("Debe Ingresar ", tipo))
      dispose(window)
      gmessage("Recuerde agregar el padre/madre faltante ")
    }
    
  })
  
  addHandlerChanged(boton_grafica_arbol, 
                    handler = function(h, ...) {
                      #add(frame, img)
                      #tkrreplot(img)                      
                      pedigree <- pedigree(id = personas$rowid, 
                                           dadid = personas$padre_id,
                                           momid = personas$madre_id,
                                           sex = personas$sexo_rowid,
                                           famid = personas$pedigree_id,
                                           status = (personas$vive_rowid-1)
                                           ##affected = as.matrix(personas[,c("brca","p53","cancer")])                              
                      )[1]                     
                      plot.pedigree(pedigree, id=personas$nombre, col=c("red3", rep("black",dim(pedigree)-1)))
                      #pedigree.legend(pedigree, location="bottomleft", radius=.2) 
                    })
  addHandlerChanged(boton_regresar, 
                    handler = function(h, ...) {
                      #dev.off()
                      dispose(window)
                      menu_familia()                        
                    }) 
  addHandlerChanged(individuos, handler=function(h,...) {
    cat(svalue(individuos))
    sexo_sel <<- svalue(individuos, drop=FALSE)$sexo    
    cat(sexo_sel)
  })
  
}


tiene_padre <- function(id) {
  if (!is.numeric(id) | length(id) != 1 ) stop("Error")  
  query <- paste0("SELECT padre_id, madre_id FROM personas WHERE rowid = ", id)
  result <- dbGetQuery(con, query)
  return(!all(is.na(result)))
}
