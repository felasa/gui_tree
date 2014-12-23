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
  n <- nrow(personas)
  m <- dbGetQuery(con, "SELECT COUNT(DISTINCT(condicion)) FROM condiciones")[[1]]
  
  query_aff <- paste("SELECT personas.rowid, condicion 
                     FROM personas 
                     JOIN condind 
                     ON personas.rowid = condind.individuo_id
                     JOIN
                     condiciones 
                     ON condiciones.rowid = condind.condicion_id 
                     WHERE pedigree_id = ", pedigree_id )
  
  fooi <- dbGetQuery(con, query_aff)
  affected <- matrix(0, nrow=n, ncol=m)
  rownames(affected) <- personas$rowid
  colnames(affected) <- unique(fooi$condicion)
  
  for (i in 1:nrow(fooi)) {
    affected[as.character(fooi$rowid[i]), fooi$condicion[i]] <- 1
  }
  
  
  window <- gwindow("Familia", visible=FALSE, horizontal = FALSE)
  frame <- gframe(cont = window, horizontal=FALSE)
  paned <- gpanedgroup( cont = frame )
  
  #individuos <- gtable(personas[ ,c("nombre","apellido","sexo","vive","brca","p53","cancer") ], cont=window)
  individuos <- gtable(personas[ ,c("rowid","nombre","apellido","sexo","fecha_nacimiento" ,"vive") ], 
                       cont = paned, expand=TRUE, visible=FALSE, multiple = FALSE)
  #sexo_sel <- svalue(individuos, drop=FALSE)$sexo
  #individuos <- gdf(personas[ ,c("rowid","nombre","apellido","sexo","fecha_nacimiento" ,"vive") ], 
  #                     cont = frame, expand=TRUE)
  selected <- svalue(individuos)
  pane2 <- gpanedgroup( cont = frame, horizontal=FALSE )
  glabel("Seleccion:", container=pane2, anchor=c(-1,0))
  campo_seleccion <- gedit( paste(personas$nombre[personas$rowid==1], personas$apellido[personas$rowid==1] ), container=pane2, expand=TRUE)
  #visible(campo_seleccion) <- FALSE
  
  group <- ggroup(cont = pane2, horizontal = TRUE)
  
  boton_agregar_padre <- gbutton("Agregar padre", cont = group)
  boton_agregar_madre <- gbutton("Agregar madre", cont = group)
  boton_agregar_hijo <- gbutton("Agregar hijo", cont = group)
  boton_agregar_hermano <- gbutton("Agregar hermano(a)", cont = group)
  #boton_agregar_pareja <- gbutton("Agregar pareja", cont=group)
  boton_grafica_arbol <- gbutton("Ver arbol", cont = group)
  boton_regresar <- gbutton("Regresar", cont = group)
  boton_actualizar <- gbutton("Actualizar", cont = group)
  boton_condicion <- gbutton("Agregar condicion", cont=group)
  visible(window) <- TRUE
  visible(individuos) <- TRUE
  
  ##AGREGAR PADRE 
  
  addHandlerClicked(boton_agregar_padre, handler=function(h, selected=svalue(individuos), origen = pedigree_id,  ...) {
    
    nuevo_individuo(pariente_rowid=selected, tipo="padre", origen)
    #dispose(window)
          
    
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
                                           status = (personas$vive_rowid-1),
                                           affected = affected)[1]
                      
                      plot.pedigree(pedigree, id=personas$nombre, col=c("red3", rep("black",dim(pedigree)-1)))
                      pedigree.legend(pedigree, location="bottomleft", radius=.2) 
                    })
  addHandlerChanged(boton_regresar, 
                    handler = function(h, ...) {
                      #dev.off()
                      dispose(window)
                      menu_familia()                        
                    }) 
  addHandlerClicked(individuos, handler=function(h, ...) {
    selected = svalue(individuos)
    #cat(svalue(individuos))
    sexo_sel <<- svalue(individuos, drop=FALSE)$sexo    
    cat(sexo_sel)
    svalue(campo_seleccion) <- paste(personas$nombre[personas$rowid==selected], personas$apellido[personas$rowid==selected])
  })
  
  addHandlerClicked(boton_actualizar, handler=function(h, ...) {
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
    personas <- dbGetQuery(con , query)
    individuos[,] <- personas[ ,c("rowid", "nombre", "apellido", "sexo", "fecha_nacimiento", "vive") ]
  }
  )
  
  addHandlerClicked(boton_condicion, handler=function(h,...) {
    agregar_condiciones(rowid=svalue(individuos))
  })
  
  
}


tiene_padre <- function(id) {
  if (!is.numeric(id) | length(id) != 1 ) stop("Error")  
  query <- paste0("SELECT padre_id, madre_id FROM personas WHERE rowid = ", id)
  result <- dbGetQuery(con, query)
  return(!all(is.na(result)))
}


