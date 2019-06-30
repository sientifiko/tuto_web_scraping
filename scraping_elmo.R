# packages a utilzar
library(selectr); library(xml2); library(rvest);library(stringr)

# si no los tienen y quieren instalarlos todos de una descomenten y corran
# esta línea
# sapply(c("selectr","xml2", "rvest", "stringr"), install.packages)


# al interior de cada función está su descripción
get_links <- function(url){
  # Esta función devuelve los links de cada hoja de noticias
  # los devuelve en formato vector
  
  # pego esto adrede en cada función para alentar las consultas, 
  # ya que estos scripts estresan el sitio scrapeado  
  # lo que puede llevar a que se cierre la conexión, incluso
  # en sitios pequeños, inducir un ataque DDoS
  tryCatch({
    read_html(url)
  },
  error=function(e){
    Sys.sleep(2)
    }
  )
  
  # capturo los links de cada noticia y las devuelvo
  pages <- read_html(url)
  
  links <- html_attr(html_nodes(pages, 
                                "h2.title.title-sm a"), 
                     "href")
  rm(pages)
  return(links)
}# fin get_links

mes_a_num <- function(mes){
  # retorna un mes escrito en español
  # retorna su equivalente en número
  return(switch(mes,
                "enero"=1,
                "febrero"=2,
                "marzo"=3,
                "abril"=4,
                "mayo"=5,
                "junio"=6,
                "julio"=7,
                "agosto"=8,
                "septiembre"=9,
                "octubre"=10,
                "noviembre"=11,
                "diciembre"=12
  ) )
} # fin mes_a_num

get_date <- function(link){
  # función retorna la fecha de cada noticia
  # y la retorna en formateada
  
  tryCatch({
    read_html(url)
  },
  error=function(e){
    Sys.sleep(2)
  }
  )
  
  # leo la noticia
  news <- read_html(link)
  # extraemos la fecha de cada noticia
  fecha <- html_text(html_node(news, "span.col.col-6 p.pull-right" )  )
  # le quitamos la coma
  aux_a <- str_replace(fecha, ",", "")
  # separamos cada componente 
  aux_b <- strsplit(aux_a, " ")[[1]]
  # convertimos el més a un valor entero
  aux_c <- str_replace(aux_b, aux_b[2], as.character(mes_a_num(aux_b[2])))
  # retorno la fecha completa
  rm(news)
  return( paste(aux_c, collapse = "-")  )
}# fin get_date

get_tags <- function(link){
  #Esta función extrae los tags de cada noticia y los devuelve en formato 
  # vector
  
  tryCatch({
    read_html(url)
  },
  error=function(e){
    Sys.sleep(2)
  }
  )
  
  
  #leo la noticia
  news <- read_html(link)
  # le saco el nodo con los tags
  aux_a <- html_text(html_nodes(news, 
                                "div.tags-noticias ul li" )  )
  # creo un vector vacío
  aux_b <- c()
  # le saco la basura
  for(i in 1:length(aux_a)){
    aux_b <- append(aux_b, str_trim( str_replace_all( aux_a[i],
                                                      "[\r\n\t]" , 
                                                      "")) )
  }
  # retorno el vector
  rm(news)
  return(aux_b)
}# fin get_tags



#creo un data frame vacío
df_news <- data.frame()
# capturo el enlace madre
url <- "https://m.elmostrador.cl/dia/page/"
# contador
p <- 1

# en caso de caerse la sesión, si corres de nuevo el script 
# partirá desde la página "p" donde quedó y puedes volver a
# correrlo. Si quieres evitar duplicados, el codigo de abajo es 
# una manera de borrar filas de un data frame entre determinado 
# intérvalo x:y
#df_news<- df_news[-c(x:y),]

# loop del scraping
while (T) {
  print(paste("pasando a página de noticia", p))
  # extraigo los links
  links <- get_links( paste(url, p, sep = "" ) )
  for (j in 1:length(links)) {
    print(paste("extrayendo link de noticia",j))
    # genero un df vacío o que voy reseteando
    df_aux <- data.frame()
    #recorro extrayendo los tags de cada link
    aux_tags <- get_tags(links[j])
    for (z in 1:length(aux_tags)) {
      print( paste("extrayendo tag",z) )
      # recorro extrayendo cada tag sumandolo al df auxiliar
      df_aux[z,1] <- links[j]
      df_aux[z,2] <- aux_tags[z]
      df_aux[z,3] <- get_date(links[j] )
    }
    df_news <- rbind(df_news, df_aux)
  }
  p <- p + 1
}# Fin del while


# descomenta y ejecuta esta línea para guardar lo recogido en formato .csv
#write.csv(df_news, "df_news.csv")

