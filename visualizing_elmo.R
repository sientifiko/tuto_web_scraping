
##=============== VISUALIZAR COMO NUBES DE PALABRAS ======================

# truco para instalar todas las librerías de una
#sapply(c("RCurl", "tm", "wordcloud"), install.packages)

library(RCurl);library(tm);library(wordcloud)

# convertimos la columna en fecha en tipo de dato
# fecha. Notar como está especificado el patrón del formato
# y el patrón de la variable
df_news$date <- as.Date(df_news$date, format="%d-%m-%Y")

# filtramos solo noticias del último mes
last_month <- df_news[df_news$date >= "2019-06-01",]

# exportamos los tags a un archivo txt
write(paste( last_month$tag), "last_mont.txt")

# poner acá la ruta del texto, puede ser una URL con texto
# también
texto <-  "last_mont.txt" 

# hacemos que R "lea" el texto
txt <- readLines(texto ,encoding="UTF-8")
txt = iconv(txt, to="ASCII//TRANSLIT") # <- esto es pa encodificar
                                       # putas tildes!!!

# construye un corpus de texto
corpus <- Corpus(VectorSource(txt))

# lleva a minísculas
d  <- tm_map(corpus, tolower)

# quita espacios en blanco
d  <- tm_map(d, stripWhitespace)

# remueve la puntuación
d <- tm_map(d, removePunctuation)

# remueve números
d <- tm_map(d, removeNumbers)

# remueve palabras específicas que quieras
#vector <- c("","")  # ponlas en este vector
# d <- tm_map(d, removeWords, vector) # corre esta linea


# remueve palabras vacías genericas
d <- tm_map(d, removeWords, stopwords("spanish"))

# esto trae un conjunto de conectores comúnes del español y los encodifica
sw <- readLines("https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt", encoding="UTF-8")
sw = iconv(sw, to="ASCII//TRANSLIT")

# remueve esos conectores
d <- tm_map(d, removeWords, sw)

# crea matriz de terminos
tdm <- TermDocumentMatrix(d)

# las convierte en una matriz normal
m <- as.matrix(tdm)

# crea una matriz con las frecuencias de cada palabra
v <- sort(rowSums(m),decreasing=TRUE)

# convierte a data frame
df <- data.frame(word = names(v),freq=v)

# correr este comando para ajustar la resolución del lienzo
# a la hora de plotear
# par(mar = rep(1,1))
# par(mfrow = c(2,2)) 

set.seed(1234) # esto es para garantizar replicabilidad del gráfico o de un modelo en general
               # se usa cuando incluyen componentes aleatorios
               

#finalmente hacemos nuestra nube de palabras
wordcloud(df$word, df$freq, min.freq= 1,
          #  max.words = x, 
          random.order = F,
          rot.per = 0.2,
          #    fixed.asp = T ,
          #   use.r.layout = T,
          colors = brewer.pal(8, "Dark2"))


##=============== VISUALIZAR COMO SERIES DE TIEMPO ======================

# las librerías
library(ggplot2); library(dplyr); library(ggthemes);library(stringr)


# importamos nuestro dataset
df_news <- read.csv("df_news.csv")

# eliminamos la primera columna
df_news <- df_news[,-1]

# le cambiamos el nombre a nuestras variables
colnames(df_news) <- c("link", "tag","date")

# especificamos qué tag contiene la palabra que buscamos
df_news$guaido <- str_count(df_news$tag, pattern = "Guaidó")

# utilizamos la librería dplyr para crear una tabla más pequeña
# solo con los datos que nos interesa, en este caso la fecha
# y si hablan de Guaidó. Las agrupamos por fechas
tbl <- df_news %>% 
  select(date, guaido) %>%
  group_by(date) %>%
  summarize(count = sum(guaido, na.rm = T))

# eliminamos la primera fila
tbl <- tbl[-1,]

# descomentar para convertir en fecha, por si algo
# llegara a fallar
# tbl$date <- as.Date(tbl$date, format="%d-%m-%Y")

# graficamos usando la librería ggplot
ggplot(tbl, aes(date, count)) +
  geom_line(size=2) +
  labs(y="atención golpe Guaidó", x="") +
  theme_base() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 30)) 
