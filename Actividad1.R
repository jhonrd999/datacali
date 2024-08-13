# Instalar el paquete desde GitHub
devtools::install_github("centromagis/paqueteMODELOS", force = TRUE)
install.packages("ggplot2")

# Cargar paquetes
library(paqueteMODELOS)
library(ggplot2)
library(dplyr)
library(knitr)

# Cargar el conjunto de datos
data("vivienda")

# Verificar la estructura del data.frame
str(vivienda)

# Ver las primeras filas del data.frame
head(vivienda)

# Resumen estadístico de las variables numéricas
summary(vivienda)

write.csv(data, "data_limpia.csv", row.names = FALSE)

# Dimensiones del data.frame
dim(vivienda)

# Nombres de las columnas
names(vivienda)

# Tipo de dato de cada columna
sapply(vivienda, class)

## Análisis de Variables Categóricas

# Crear tablas
zona_table <- table(vivienda$zona)
piso_table <- table(vivienda$piso)
tipo_table <- table(vivienda$tipo)
barrio_table <- table(vivienda$barrio)

# Mostrar tablas
kable(zona_table, caption = "Frecuencia por Zona")
kable(piso_table, caption = "Frecuencia por Piso")
kable(tipo_table, caption = "Frecuencia por Tipo")
kable(barrio_table, caption = "Frecuencia por Barrio")

# Gráfico de barras para la variable 'zona'
ggplot(data.frame(zona = names(zona_table), frecuencia = as.numeric(zona_table)), aes(x = reorder(zona, frecuencia), y = frecuencia)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Frecuencia por Zona", x = "Zona", y = "Frecuencia")

# Gráfico de barras para la variable 'piso'
ggplot(data.frame(piso = names(piso_table), frecuencia = as.numeric(piso_table)), aes(x = reorder(piso, frecuencia), y = frecuencia)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Frecuencia por Piso", x = "Piso", y = "Frecuencia")

# Gráfico de barras para la variable 'tipo'
ggplot(data.frame(tipo = names(tipo_table), frecuencia = as.numeric(tipo_table)), aes(x = tipo, y = frecuencia)) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencia por Tipo", x = "Tipo", y = "Frecuencia")

# Mostrar solo las categorías más frecuentes
top_barrio <- head(sort(barrio_table, decreasing = TRUE), 10)
ggplot(data.frame(barrio = names(top_barrio), frecuencia = as.numeric(top_barrio)), aes(x = reorder(barrio, frecuencia), y = frecuencia)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Frecuencia por Barrio (Top 10)", x = "Barrio", y = "Frecuencia")


## Análisis de Variables Númericas
# Resumen estadístico para las variables numéricas
summary(vivienda[, c("id", "estrato", "preciom", "areaconst", "parqueaderos", "banios", "habitaciones", "longitud", "latitud")])

#Distribución de Variables Numéricas

# Cargar la librería ggplot2 
library(ggplot2)

# Definir las variables numéricas
variables_numericas <- c("estrato", "preciom", "areaconst", "parqueaderos", "banios", "habitaciones", "longitud", "latitud")

# Recorrer cada variable y crear gráficos
for (var in variables_numericas) {
  # Histograma
  p_histogram <- ggplot(vivienda, aes_string(x = var)) + 
    geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
    ggtitle(paste("Distribución de", var)) +
    xlab(var) +
    ylab("Frecuencia") +
    theme_minimal()
  print(p_histogram)
  
  # Gráfico de densidad
  p_density <- ggplot(vivienda, aes_string(x = var)) +
    geom_density(fill = "blue", alpha = 0.7) +
    ggtitle(paste("Densidad de", var)) +
    xlab(var) +
    ylab("Densidad") +
    theme_minimal()
  print(p_density)
}

# Imprimir resumen de datos
summary(vivienda[, variables_numericas])


#Limpieza de datos [categóricos]
# Cargar librerías necesarias
library(dplyr)
library(stringr)
library(stringi)

# Normalización de la variable 'barrios'

# Diccionario de mapeo para normalización
mapa_barrios <- c(
  "20 de julio" = "20 de julio",
  "3 de julio" = "3 de julio",
  "aguablanca" = "aguablanca",
  "alameda del río" = "alameda del río",
  "alférez real" = "alférez real",
  "alfonso lópez" = "alfonso lópez",
  "altos de guadalupe" = "altos de guadalupe",
  "alta de santa" = "alta de santa",
  "antonio nariño" = "antonio nariño",
  "aranjuez" = "aranjuez",
  "arboleda campestre candelaria" = "arboleda campestre",
  "atanasio girardot" = "atanasio girardot",
  "autopista sur" = "autopista sur",
  "bajo aguacatal" = "bajo aguacatal",
  "barrio 7de agosto" = "7 de agosto",
  "barrio el recuerdo" = "el recuerdo",
  "barrio eucarístico" = "eucarístico",
  "barrio obrero" = "obrero",
  "base aérea" = "base aérea",
  "belalcazar" = "belalcazar",
  "belisario caicedo" = "belisario caicedo",
  "bella suiza alta" = "bella suiza alta",
  "bellavista" = "bellavista",
  "benjamín herrera" = "benjamín herrera",
  "berlin" = "berlin",
  "bloques del limonar" = "bloques del limonar",
  "bochalema" = "bochalema",
  "bolivariano" = "bolivariano",
  "bosques de alboleda" = "bosques de alboleda",
  "bosques del limonar" = "bosques del limonar",
  "boyacá" = "boyacá",
  "bretaña" = "bretaña",
  "brisas de guadalupe" = "brisas de guadalupe",
  "brisas del guabito" = "brisas del guabito",
  "bueno madrid" = "bueno madrid",
  "buenos aires" = "buenos aires",
  "caldas" = "caldas",
  "cali" = "cali",
  "calibella" = "calibella",
  "calicanto viii" = "calicanto viii",
  "calima" = "calima",
  "calimio norte" = "calimio norte",
  "calipso" = "calipso",
  "cambulos" = "cambulos",
  "camino real" = "camino real",
  "campestre" = "campestre",
  "caney" = "caney",
  "cañasgordas" = "cañasgordas",
  "cañaverales" = "cañaverales",
  "capri" = "capri",
  "cascajal" = "cascajal",
  "ceibas" = "ceibas",
  "centenario" = "centenario",
  "cerro cristales" = "cerro cristales",
  "cerros de guadalupe" = "cerros de guadalupe",
  "champagnat" = "champagnat",
  "chapinero" = "chapinero",
  "chiminangos" = "chiminangos",
  "chipichape" = "chipichape",
  "ciudad 2000" = "ciudad 2000",
  "ciudad antejardin" = "ciudad antejardin",
  "ciudad bochalema" = "ciudad bochalema",
  "ciudad capri" = "ciudad capri",
  "ciudad córdoba" = "ciudad córdoba",
  "ciudad del campo" = "ciudad del campo",
  "ciudad jardín" = "ciudad jardín",
  "ciudad jardín pance" = "ciudad jardín pance",
  "ciudad los álamos" = "ciudad los álamos",
  "ciudad melendez" = "ciudad melendez",
  "ciudad modelo" = "ciudad modelo",
  "ciudad pacifica" = "ciudad pacifica",
  "ciudadela comfandi" = "ciudadela comfandi",
  "ciudadela del río" = "ciudadela del río",
  "ciudadela pasoancho" = "ciudadela pasoancho",
  "colinas de menga" = "colinas de menga",
  "colinas del sur" = "colinas del sur",
  "colon" = "colon",
  "colseguros" = "colseguros",
  "comfenalco" = "comfenalco",
  "compartir" = "compartir",
  "conjunto gibraltar" = "conjunto gibraltar",
  "cristales" = "cristales",
  "cristóbal colón" = "cristóbal colón",
  "cuarto de legua" = "cuarto de legua",
  "departamental" = "departamental",
  "ed benjamin herrera" = "benjamin herrera",
  "el bosque" = "el bosque",
  "el caney" = "el caney",
  "el castillo" = "el castillo",
  "el cedro" = "el cedro",
  "el dorado" = "el dorado",
  "el gran limonar" = "el gran limonar",
  "el guabal" = "el guabal",
  "el guabito" = "el guabito",
  "el ingenio" = "el ingenio",
  "el jardín" = "el jardín",
  "el jordán" = "el jordán",
  "el lido" = "el lido",
  "el limonar" = "el limonar",
  "el nacional" = "el nacional",
  "el paraíso" = "el paraíso",
  "el peñon" = "el peñon",
  "el refugio" = "el refugio",
  "el rodeo" = "el rodeo",
  "el sena" = "el sena",
  "el trébol" = "el trébol",
  "el troncal" = "el troncal",
  "el vallado" = "el vallado",
  "eucarístico" = "eucarístico",
  "evaristo garcía" = "evaristo garcía",
  "fenalco kennedy" = "fenalco kennedy",
  "flora" = "flora",
  "flora industrial" = "flora industrial",
  "floralia" = "floralia",
  "fonaviemcali" = "fonaviemcali",
  "francisco eladio ramirez" = "francisco eladio ramirez",
  "fuentes de la" = "fuentes de la",
  "gaitan" = "gaitan",
  "gran limonar" = "gran limonar",
  "granada" = "granada",
  "guadalupe" = "guadalupe",
  "guadalupe alto" = "guadalupe alto",
  "guaduales" = "guaduales",
  "guayaquil" = "guayaquil",
  "hacienda alferez real" = "alférez real",
  "ingenio" = "ingenio",
  "jamundi" = "jamundi",
  "jorge eliecer gaitán" = "jorge eliecer gaitán",
  "jorge isaacs" = "jorge isaacs",
  "jose manuel marroquín" = "jose manuel marroquín",
  "juanambu" = "juanambu",
  "junin" = "junin",
  "la alborada" = "la alborada",
  "la alianza" = "la alianza",
  "la arboleda" = "la arboleda",
  "la base" = "la base",
  "la buitrera" = "la buitrera",
  "la campiña" = "la campiña",
  "la cascada" = "la cascada",
  "la ceibas" = "la ceibas",
  "la esmeralda" = "la esmeralda",
  "la flora" = "la flora",
  "la floresta" = "la floresta",
  "la fortaleza" = "la fortaleza",
  "la gran colombia" = "la gran colombia",
  "la hacienda" = "la hacienda",
  "la independencia" = "la independencia",
  "la libertad" = "la libertad",
  "la luisa" = "la luisa",
  "la merced" = "la merced",
  "la morada" = "la morada",
  "la nueva base" = "la nueva base",
  "la playa" = "la playa",
  "la portada al" = "la portada al",
  "la primavera" = "la primavera",
  "la reforma" = "la reforma",
  "la rivera" = "la rivera",
  "la riviera" = "la riviera",
  "la selva" = "la selva",
  "la villa del" = "la villa del",
  "laflora" = "la flora",
  "lares de comfenalco" = "lares de comfenalco",
  "las acacias" = "las acacias",
  "las américas" = "las américas",
  "las camelias" = "las camelias",
  "las ceibas" = "las ceibas",
  "las delicias" = "las delicias",
  "las granjas" = "las granjas",
  "las quintas de" = "las quintas de",
  "las vegas" = "las vegas",
  "libertadores" = "libertadores",
  "los alamos" = "los álamos",
  "los alcázares" = "los alcázares",
  "los andes" = "los andes",
  "los cambulos" = "los cambulos",
  "los cristales" = "los cristales",
  "los farallones" = "los farallones",
  "los guaduales" = "los guaduales",
  "los jockeys" = "los jockeys",
  "los parques barranquilla" = "los parques barranquilla",
  "los robles" = "los robles",
  "lourdes" = "lourdes",
  "mamellan" = "mamellan",
  "manzanares" = "manzanares",
  "mariano ramos" = "mariano ramos",
  "marroquín iii" = "marroquín iii",
  "mayapan las vegas" = "mayapan las vegas",
  "meléndez" = "meléndez",
  "menga" = "menga",
  "metropolitano del norte" = "metropolitano del norte",
  "miradol del aguacatal" = "miradol del aguacatal",
  "miraflores" = "miraflores",
  "morichal de comfandi" = "morichal de comfandi",
  "multicentro" = "multicentro",
  "municipal" = "municipal",
  "napoles" = "napoles",
  "normandia" = "normandia",
  "norte la flora" = "norte la flora",
  "nueva base" = "nueva base",
  "nueva floresta" = "nueva floresta",
  "nueva tequendama" = "nueva tequendama",
  "oasis de comfandi" = "oasis de comfandi",
  "occidente" = "occidente",
  "pacara" = "pacara",
  "palmas del ingenio" = "palmas del ingenio",
  "pampa linda" = "pampa linda",
  "panamericano" = "panamericano",
  "pance" = "pance",
  "parcelaciones pance" = "parcelaciones pance",
  "parque residencial el" = "parque residencial el",
  "paseo de los" = "paseo de los",
  "paso del comercio" = "paso del comercio",
  "pasoancho" = "pasoancho",
  "poblado campestre" = "poblado campestre",
  "ponce" = "ponce",
  "popular" = "popular",
  "portada de comfandi" = "portada de comfandi",
  "portales de comfandi" = "portales de comfandi",
  "porvenir" = "porvenir",
  "prados de oriente" = "prados de oriente",
  "prados del limonar" = "prados del limonar",
  "prados del norte" = "prados del norte",
  "prados del sur" = "prados del sur",
  "primavera" = "primavera",
  "primero de mayo" = "primero de mayo",
  "primitivo crespo" = "primitivo crespo",
  "puente del comercio" = "puente del comercio",
  "puente palma" = "puente palma",
  "quintas de don" = "quintas de don",
  "quintas de salomia" = "quintas de salomia",
  "rafael uribe uribe" = "rafael uribe uribe",
  "refugio" = "refugio",
  "república de israel" = "república de israel",
  "rincón de salomia" = "rincón de salomia",
  "riveras del valle" = "riveras del valle",
  "rozo la torre" = "rozo la torre",
  "saavedra galindo" = "saavedra galindo",
  "salomia" = "salomia",
  "samanes" = "samanes",
  "san antonio" = "san antonio",
  "san bastian" = "san bastian",
  "san basilio" = "san basilio",
  "san bartolomé" = "san bartolomé",
  "san felipe" = "san felipe",
  "san francisco" = "san francisco",
  "san joaquin" = "san joaquin",
  "san luis" = "san luis",
  "san marcos" = "san marcos",
  "san mateo" = "san mateo",
  "san rafael" = "san rafael",
  "san sebastian" = "san sebastian",
  "san vicente" = "san vicente",
  "santa maría" = "santa maría",
  "santa monica" = "santa monica",
  "santa teresa" = "santa teresa",
  "santiago de cali" = "santiago de cali",
  "santo domingo" = "santo domingo",
  "santuario" = "santuario",
  "suroccidente" = "suroccidente",
  "timbio" = "timbio",
  "torres del oeste" = "torres del oeste",
  "torres del valle" = "torres del valle",
  "tres de julio" = "tres de julio",
  "tulipanes" = "tulipanes",
  "urbanizacion" = "urbanizacion",
  "valdivia" = "valdivia",
  "valle del lili" = "valle del lili",
  "valle del pacífico" = "valle del pacífico",
  "valle pacifico" = "valle pacifico",
  "valle real" = "valle real",
  "vereda el" = "vereda el",
  "villa camila" = "villa camila",
  "villa del rosario" = "villa del rosario",
  "villa natalia" = "villa natalia",
  "villa ricardo" = "villa ricardo",
  "villa san luis" = "villa san luis",
  "villa santa clara" = "villa santa clara",
  "villa victoria" = "villa victoria",
  "zapata" = "zapata",
  "zona franca" = "zona franca",
  "zona industrial" = "zona industrial"
)

# Normalizar y mapear los barrios
vivienda <- vivienda %>%
  mutate(
    barrio_normalizado = stri_trans_general(barrio, "Latin-ASCII") %>%  # Eliminar acentos
      tolower() %>%                                  # Convertir a minúsculas
      str_trim() %>%                                 # Eliminar espacios adicionales
      recode(!!!mapa_barrios)                        # Aplicar el mapeo
  )

# Verificar los resultados
head(vivienda$barrio_normalizado)

# Ver los barrios únicos después de la normalización
unique(vivienda$barrio_normalizado)

#Manejo de valores nulos
# Contar valores nulos por variable
valores_nulos <- sapply(vivienda, function(x) sum(is.na(x)))

# Separar las variables categóricas y numéricas
variables_categoricas <- c("zona", "piso", "tipo", "barrio", "barrio_normalizado")
variables_numericas <- c("id", "estrato", "preciom", "areaconst", "parqueaderos", "banios", "habitaciones", "longitud", "latitud")

# Contar valores nulos en variables categóricas
nulos_categoricas <- valores_nulos[variables_categoricas]
print("Valores nulos en variables categóricas:")
print(nulos_categoricas)

# Contar valores nulos en variables numéricas
nulos_numericas <- valores_nulos[variables_numericas]
print("Valores nulos en variables numéricas:")
print(nulos_numericas)


# Eliminar las filas que tienen valores nulos en la columna 'id'
vivienda <- vivienda[!is.na(vivienda$id), ]

# Contar valores nulos por variable después de la eliminación
valores_nulos <- sapply(vivienda, function(x) sum(is.na(x)))

# Mostrar los valores nulos por variable
print(valores_nulos)

library(dplyr)

# Imputación basada en la moda de 'piso' dentro de cada 'zona'
vivienda <- vivienda %>%
  group_by(zona) %>%
  mutate(piso = ifelse(is.na(piso), as.character(names(sort(table(piso), decreasing = TRUE)[1])), piso)) %>%
  ungroup()

# Verificar los valores nulos restantes en 'piso'
sum(is.na(vivienda$piso))

library(dplyr)

# Imputación basada en la moda de 'parqueaderos' dentro de cada 'zona'
vivienda <- vivienda %>%
  group_by(zona) %>%
  mutate(parqueaderos = ifelse(is.na(parqueaderos), as.numeric(names(sort(table(parqueaderos), decreasing = TRUE)[1])), parqueaderos)) %>%
  ungroup()

# Verificar los valores nulos restantes en 'parqueaderos'
sum(is.na(vivienda$parqueaderos))

#Detección y Manejo de Outliers

# Crear un boxplot para cada variable numérica en una disposición de gráficos
numeric_vars <- c("estrato", "preciom", "areaconst", "parqueaderos", "banios", "habitaciones", "longitud", "latitud")

# Establecer la disposición de la ventana gráfica para mostrar múltiples gráficos
par(mfrow = c(2, 4))  # 2 filas, 4 columnas (ajustar según el número de variables)

# Generar boxplots
for (var in numeric_vars) {
  boxplot(vivienda[[var]], main = paste("Boxplot de", var), ylab = var)
}

# Restablecer la disposición de la ventana gráfica a la predeterminada
par(mfrow = c(1, 1))


# Escalamiento de las variables numéricas
# Seleccionar las variables numéricas relevantes para el ACP, excluyendo 'longitud' y 'latitud'
variables_numericas <- c("estrato", "preciom", "areaconst", "parqueaderos", "banios", "habitaciones")

# Escalar las variables numéricas
vivienda_scaled <- scale(vivienda[, variables_numericas])
head(vivienda_scaled)  # Verificar los primeros registros

# Realizar el Análisis de Componentes Principales (ACP)
res.pca <- prcomp(vivienda_scaled, center = TRUE, scale. = TRUE)

# Resumen del ACP
summary(res.pca)

# Interpretar los resultados
# Desviaciones estándar de los componentes principales
res.pca$sdev

# Valores propios (lambda)
valores_propios <- res.pca$sdev^2

# Porcentaje de variabilidad explicada por cada componente
var_explicada <- valores_propios / sum(valores_propios) * 100

# Mostrar el porcentaje de variabilidad explicada
var_explicada

# Instalar el paquete factoextra desde CRAN si no está instalado
if (!require(factoextra)) {
  install.packages("factoextra")
}
library(factoextra)

# Gráfico de porcentaje de variabilidad explicada
fviz_eig(res.pca, addlabels = TRUE)

# Interpretación de las cargas y coordenadas de los componentes
# Cargas de las variables en los componentes principales
res.pca$rotation

# Coordenadas de los individuos en el espacio de componentes principales
scores <- res.pca$x
head(scores)

nrow_vivienda <- nrow(vivienda)
nrow_pca <- nrow(res.pca$x)

cat("Número de filas en vivienda:", nrow_vivienda, "\n")
cat("Número de filas en res.pca$x:", nrow_pca, "\n")

str(res.pca)

if (nrow_pca == nrow_vivienda) {
  rownames(res.pca$x) <- rownames(vivienda)
} else {
  cat("Error: el número de filas no coincide. No se puede asignar los nombres de las filas.\n")
}


# Visualización de los resultados

# Circulo de correlación (sin 'longitud' y 'latitud')
fviz_pca_var(res.pca,
             col.var = "contrib", # Color por contribución a los componentes principales
             gradient.cols = c("#FF7F00",  "#034D94"), # Gradiente de colores
             repel = TRUE     # Evitar superposición de texto
)

# Nube de individuos
fviz_pca_ind(res.pca, col.ind = "blue")

# Verifica la estructura del objeto res.pca
str(res.pca)

# Asignar nombres a las filas de res.pca$x si es necesario
rownames(res.pca$x) <- rownames(vivienda)

# Intentar generar el biplot nuevamente
fviz_pca_biplot(res.pca, repel = TRUE)


#Análisis de Conglomerados
install.packages("FactoMineR")
library(FactoMineR)


# Excluir latitud y longitud del análisis
variables_cuantitativas <- c("estrato", "preciom", "areaconst", "parqueaderos", "banios", "habitaciones")

# Escalar las variables cuantitativas
vivienda_scaled <- scale(vivienda[, variables_cuantitativas])

# Realizar el ACP en las variables escaladas
res.ACP <- PCA(vivienda_scaled, graph = FALSE)

# Realizar el clustering jerárquico sobre los componentes principales
Cluster.Vivienda <- HCPC(res.ACP, nb.clust = -1)

# Visualizar los clusters
plot(Cluster.Vivienda)

# Visualización en 2 dimensiones
fviz_cluster(Cluster.Vivienda,
             show.clust.cent = TRUE, 
             palette = "jco",         
             ggtheme = theme_minimal(),
             main = "Clustering de Viviendas basado en ACP"
)

# Descripción de las variables que contribuyen más a cada cluster
Cluster.Vivienda$desc.var

# Resumen descriptivo por cluster
Base_Final <- cbind(vivienda[, variables_cuantitativas], Cluster = Cluster.Vivienda$data.clust$clust)

summary_by_cluster <- Base_Final %>%
  group_by(Cluster) %>%
  summarise_all(list(mean = mean, sd = sd))
summary_by_cluster


# Crear gráficos de densidad para cada variable por cluster
install.packages("tidyr")
library(tidyr)

Base_Final %>%
  pivot_longer(-Cluster, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = Cluster)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Densidad de Variables por Cluster") +
  theme_minimal()

# Crear boxplots para las variables por cluster
Base_Final %>%
  gather(key = "variable", value = "value", -Cluster) %>%
  ggplot(aes(x = Cluster, y = value, fill = Cluster)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distribución de Variables por Cluster") +
  theme_minimal()

# ACM
# Se seleccionan las variables categóricas relevantes para el análisis
variables_cualitativas <- vivienda[, c("tipo", "zona", "estrato")]

# Convertir las variables a factores para asegurar que se manejen como categóricas
variables_cualitativas$tipo <- as.factor(variables_cualitativas$tipo)
variables_cualitativas$zona <- as.factor(variables_cualitativas$zona)
variables_cualitativas$estrato <- as.factor(variables_cualitativas$estrato)

# Realizar el Análisis de Correspondencias Múltiples (ACM)
# Se ejecuta el ACM para explorar las relaciones entre las variables categóricas seleccionadas
resultado_acm <- MCA(variables_cualitativas, graph = FALSE)

# Visualización de los resultados del ACM
# Se presenta un gráfico de las categorías que más contribuyen a la explicación de la inercia
fviz_mca_var(resultado_acm, 
             select.var = list(contrib = 15),  # Se seleccionan las 15 categorías con mayor contribución
             repel = TRUE)                      # Se ajustan las etiquetas para evitar superposiciones

# Adicionalmente, se muestra la contribución total de las variables a las dos primeras dimensiones
fviz_contrib(resultado_acm, choice = "var", axes = 1, top = 15)  # Contribuciones en la primera dimensión
fviz_contrib(resultado_acm, choice = "var", axes = 2, top = 15)  # Contribuciones en la segunda dimensión

# Visualización de la calidad de representación (cos2) de las categorías
fviz_mca_var(resultado_acm, 
             col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)




