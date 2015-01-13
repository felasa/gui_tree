valida_fecha <- function(string) {
  require(gWidgets)
  re <- "((0[1-9])|([1-2][0-9])|(3[0-1]))/((0[1-9])|(1[0-2]))/((19|20)[[:digit:]]{2})"
  if (grepl(re, string)) {
    return(string)
  } else {
    on.exit(expr=return(""), add=TRUE )
    on.exit(expr=gmessage("Fecha invalida") )    
    stop("Fecha Invalida")
    }
}

obtener_datos <- function(connection, pedigree_id) {
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
  personas <- dbGetQuery(connection , query)
  
  n_personas <- nrow(personas)
  
  
  query_aff <- paste("SELECT personas.rowid, condicion 
                     FROM personas 
                     JOIN condind 
                     ON personas.rowid = condind.individuo_id
                     JOIN
                     condiciones 
                     ON condiciones.rowid = condind.condicion_id 
                     WHERE personas.pedigree_id = ", pedigree_id )
  
  fooi <- dbGetQuery(connection, query_aff)
  m <- length(unique(fooi$condicion))
  
  if (m == 0) {
    affected <- matrix(0, nrow=n_personas)
  } else { 
    affected <- matrix(0, nrow=n_personas, ncol=m)  
    rownames(affected) <- personas$rowid
    colnames(affected) <- unique(fooi$condicion)
    if (nrow(fooi) > 0) {
      for (i in 1:nrow(fooi)) {
        affected[as.character(fooi$rowid[i]), fooi$condicion[i]] <- 1
      }    
    }
  }
  return(list(personas=personas, affected=affected))
}