### Dto_Indice Complejidad 2019    ###
### INE-DERFE-COC-DE-EVALUACION DEMOGRAFICA ###
# Autor: Miguel David Alvarez Hernández
# Ultima versión : 22/03/2019

#### Paquetes ####

library(pacman)
library(dplyr)
p_load(tidyverse,
       ggthemes,
       ggfortify,
       RColorBrewer,
       colorRamps,
       corrplot,
       openxlsx,
       readxl,
       Hmisc,
       cluster,
       factoextra,
       magrittr,
       FactoMineR,
       PerformanceAnalytics)


#### Setup ####

options(scipen=999) # Prevenir notación científica


#### Datos (orange) PCA y clusters por Dto ####

#se cargan los datos csv obtenidos con orange (dos conjuntos de variables analizados con dos metodos)

dto_Variables_originales_PCA_K_means_orange <- read_csv("C:/Users/miguel.alvarez/Google Drive/INE/DERFE/META_Indice Complejidad_2019/Dto/dto_Variables originales_PCA_K-means_orange.csv", 
                                                        col_types = cols(PC1 = col_double(), 
                                                                         PC2 = col_double(), Dto = col_double(),Silhouette = col_skip()))

#eliminamos los primeros dos renglones
dto_Variables_originales_PCA_K_means_orange <- dto_Variables_originales_PCA_K_means_orange[-c(1,2),]

###

dto_Variables_originales_PCA_Hierc_Cluster_orange <- read_csv("C:/Users/miguel.alvarez/Google Drive/INE/DERFE/META_Indice Complejidad_2019/Dto/dto_Variables originales_PCA_Hierc Cluster_orange.csv", 
                                                              col_types = cols(PC1 = col_double(), 
                                                                               PC2 = col_double(), Dto = col_double(),Selected = col_skip()))
dto_Variables_originales_PCA_Hierc_Cluster_orange <- dto_Variables_originales_PCA_Hierc_Cluster_orange[-c(1,2),]

###

dto_Variables_nuevas_PCA_K_means_orange <- read_csv("C:/Users/miguel.alvarez/Google Drive/INE/DERFE/META_Indice Complejidad_2019/Dto/dto_Variables nuevas_PCA_K-means_orange.csv", 
                                                    col_types = cols(PC1 = col_double(), 
                                                                     PC2 = col_double(), Dto = col_double(),Silhouette = col_skip()))
dto_Variables_nuevas_PCA_K_means_orange <- dto_Variables_nuevas_PCA_K_means_orange[-c(1,2),]

###

dto_Variables_nuevas_PCA_Hierc_Cluster_orange <- read_csv("C:/Users/miguel.alvarez/Google Drive/INE/DERFE/META_Indice Complejidad_2019/Dto/dto_Variables nuevas_PCA_Hierc Cluster_orange.csv", 
                                                          col_types = cols(PC1 = col_double(), 
                                                                           PC2 = col_double(), Dto = col_double(),Selected = col_skip()))
dto_Variables_nuevas_PCA_Hierc_Cluster_orange <- dto_Variables_nuevas_PCA_Hierc_Cluster_orange[-c(1,2),]


#unimos los tibbles para tener los dos clusterings para las variables originales

datos_dto_orange <- merge(dto_Variables_originales_PCA_K_means_orange,dto_Variables_originales_PCA_Hierc_Cluster_orange,by=c("PC1","PC2","Tipologia 2017","Edo","Dto","Edo_Dto"))

datos_dto_orange <- datos_dto_orange[order(datos_dto_orange$Edo_Dto),] #ordenamos 

datos_dto_orange <- merge(datos_dto_orange,dto_Variables_nuevas_PCA_K_means_orange,by=c("Tipologia 2017","Edo","Dto","Edo_Dto"))

datos_dto_orange <- datos_dto_orange[order(datos_dto_orange$Edo_Dto),] #ordenamos 

datos_dto_orange <- merge(datos_dto_orange,dto_Variables_nuevas_PCA_Hierc_Cluster_orange,by=c("Tipologia 2017","Edo","Dto","Edo_Dto"))

#eliminamos columnas repetidas
datos_dto_orange <- datos_dto_orange[,-c(12,13)]

datos_dto_orange <- datos_dto_orange[order(datos_dto_orange$Edo_Dto),] #ordenamos 

#cambiamos los rownames por la columna Edo_Dto
datos_dto_orange2 <- datos_dto_orange %>% 
  remove_rownames %>% 
  column_to_rownames(var="Edo_Dto")

#añadimos nuevamente la columna Edo_Dto
datos_dto_orange2$"Edo_Dto" <- datos_dto_orange$Edo_Dto

#ordenamos las columnas
datos_dto_orange2 <- datos_dto_orange2[c(2,3,1,4,5,6,7,8,9,10,11,12)]

#cambiamos el nombre las columnas
datos_dto_orange2 <- datos_dto_orange2 %>% 
  rename(
    PC1_orig = PC1.x,
    PC2_orig = PC2.x,
    PC1_nuev = PC1.y,
    PC2_nuev = PC2.y,
    Cluster.Kmeans_orig = Cluster.x,
    Cluster.HC_orig = Cluster.y,
    Cluster.Kmeans_nuev = Cluster.x.1,
    Cluster.HC_nuev = Cluster.y.1)

#se transforma el tibble a un dataframe
#data_PCA_Kmeans_distrito <- as.data.frame(datos_dto_orange2)


#### Variables (sin analizar) por Dto ####

### Se carga la base de datos de todas las variables del índice (sin procesar)

Dto_All_variables_ind2019 <- read_excel("C:/Users/miguel.alvarez/Google Drive/INE/DERFE/META_Indice Complejidad_2019/Dto/Dto_All variables_ind2019.xlsx", 
                                        col_types = c("blank", "text", "numeric", 
                                                      "text", "text", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric"))

#cambiamos los rownames por la columna Edo_Dto
Dto_All_variables_ind2019_2 <- Dto_All_variables_ind2019 %>% 
  remove_rownames %>% 
  column_to_rownames(var="mS#Edo_Dto")

#añadimos nuevamente la columna Edo_Dto
Dto_All_variables_ind2019_2$"Edo_Dto" <- Dto_All_variables_ind2019$"mS#Edo_Dto"

colnames(Dto_All_variables_ind2019_2)

#cambiamos el nombre de las columnas
Dto_All_variables_ind2019_2 <- Dto_All_variables_ind2019_2 %>% 
  rename(
    Estado = "mS#Edo",
    Distrito = "mS#Dto",
    Tipologia_2017 = "c#Tipologia 2017",
    Tiempo_Prom = "C#Tiempo Promedio (Minutos)",
    Superficie = "C#Superficie (km2)",
    Densidad = "C#Densidad (Ciudadanos en PE por km2)",
    Poblacion_menos_18 = "C#Poblacion menos de 18",
    Poblacion_mas_60 = "C#Poblacion 60 o mas",
    Tasa_crecimiento = "C#Tasa crecimiento PE",
    PE_JUL2018 = "C#Padron JUL-18",
    PE_FEB2019 = "C#Padron FEB-19",
    PE_Loc_Rurales = "D#Padron Localidades Rurales",
    Secc_Rurales = "D#Numero secciones rurales",
    Participacion_Electoral = "C#Participacion Ciudadana 2018 (presidente)",
    Viviendas_Internet = "C#Viviendas con Internet",
    Viviendas_Celular = "C#Viviendas con Celular",
    Prom_Solic_Equi_MACfijo = "C#Prom Solic-Equip MAC fijo",
    Prom_Solic_Equi_MACmovil = "C#Prom Solic-Equip MAC movil",
    Dias_disp_CV = "C#Dias prom dispo CV",
    Prom_CV_disp = "C#CV disp prom" 
    )


#se transforma el tibble a un dataframe
#data_Allvariables <- as.data.frame(Dto_All_variables_ind2019_2)



#### Analisis de correlacion (para Dto_All_variables_ind2019_2) ####

#se crea un dataframe sin columnas con categorías o texto, 
Dto_All_variables_ind2019_3 <- Dto_All_variables_ind2019_2[, -c(1,2,3)]

#cambiamos los rownames por la columna Edo_Dto
Dto_All_variables_ind2019_3 <- Dto_All_variables_ind2019_3 %>% 
  remove_rownames %>% 
  column_to_rownames(var="Edo_Dto")

#se crea un dataframe con las variables originales usadas en la tipologia de 2017, 
Dto_All_variables_ind2019_orig <- Dto_All_variables_ind2019_3[, -c(4,5,6,7,8,11:17)]
Dto_All_variables_ind2019_orig$"Edo_Dto" <- Dto_All_variables_ind2019_2$"Edo_Dto" #añadimos nuevamente la columna

#cambiamos los rownames por la columna Edo_Dto
Dto_All_variables_ind2019_orig <- Dto_All_variables_ind2019_orig %>% 
  remove_rownames %>% 
  column_to_rownames(var="Edo_Dto")

#Resumen estadistico para todas las variables; para la variable continua obtiene medidas descriptivas de los datos
summary(Dto_All_variables_ind2019_3)

#matriz de correlacion 
# ver https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

res <- cor(Dto_All_variables_ind2019_3)
res <- round(res, 2) #se redondea a dos cifras
View(res)

#p-values
res2 <- rcorr(as.matrix(Dto_All_variables_ind2019_3))
res2


# se crea una funcion para mostrar simultaneamente la matrix de correlación y los p-valores
# flattenCorrMatrix
# ver http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#se imprime el dataframe con la comparacion de las correlaciones
res3 <- flattenCorrMatrix(res2$r, res2$P)
View(res3)

#gráfica de correlaciones (solo variables originales)
res <- cor(Dto_All_variables_ind2019_orig)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


# Insignificant correlations are leaved blank
res2 <- rcorr(as.matrix(Dto_All_variables_ind2019_orig))
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")

#display a chart of a correlation matrix
# ver http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
chart.Correlation(Dto_All_variables_ind2019_3, histogram=TRUE, pch=19)
chart.Correlation(Dto_All_variables_ind2019_orig, histogram=TRUE, pch=19)



#### PCA para variables originales ####

#aplicamos PCA con centrado y normalizacion de variables
variables_orig.pca <- prcomp(Dto_All_variables_ind2019_orig, center = TRUE, scale = TRUE)

#resumen de PCA y contribucion de variables a cada PC
summary(variables_orig.pca)
print(variables_orig.pca)

#grafica de varianza explicada
plot(variables_orig.pca, type = "l")
fviz_eig(variables_orig.pca)

#graficaS de PC1 y PC2

plot(variables_orig.pca$x[,1:2])

autoplot(variables_orig.pca,label = TRUE, data =  Dto_All_variables_ind2019_orig , label.size = 3)

fviz_pca_ind(variables_orig.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE)     # Avoid text overlapping

# Resultados por dto
res.ind <- get_pca_ind(variables_orig.pca)
pca_voriginales <- res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 


####Clusters para PC obtenidos con variables originales ####

#calculo y visualizacion de las distancias usadas para el clustering

#euclidiana
dist_ori_euc <- get_dist(datos_dto_orange2[,c(4,5)],method = "euclidean")
fviz_dist(dist_ori_euc, lab_size = 4, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#manhattan
dist_ori_man <- get_dist(datos_dto_orange2[,c(4,5)],method = "manhattan")
fviz_dist(dist_ori_man, lab_size = 4, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))



# K means =====

#calculo del numero optimo de clusters con el metodo gap para K-means
fviz_nbclust(datos_dto_orange2[,c(4,5)], kmeans, method = "gap_stat") #señala que con 9 clusters se pueden agrupar los datos

# Visualización de K-means con 2 clusters
set.seed(123)
km.res <- kmeans(datos_dto_orange2[,c(4,5)], 2, nstart = 25)
fviz_cluster(km.res, data = datos_dto_orange2[,c(4,5)],
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal()) +
  labs(subtitle = "Método k-means (óptimo)")

#gráficas que muestran el numero optimo de clusters con diferentes metodos
#ver https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/

# Elbow method
fviz_nbclust(datos_dto_orange2[,c(4,5)], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(datos_dto_orange2[,c(4,5)], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(datos_dto_orange2[,c(4,5)], kmeans, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")


# Visualización con 3 clusters
set.seed(123)
km.res4 <- kmeans(datos_dto_orange2[,c(4,5)], 3, nstart = 25)
fviz_cluster(km.res4, data = datos_dto_orange2[,c(4,5)],
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal()) +
  labs(subtitle = "Método k-means (subóptimo)")


# PAM (Partitioning (clustering) of the data into k clusters “around medoids”, 
# a more robust version of K-means.)
pam.res <- pam(datos_dto_orange2[,c(4,5)], 3) #con 3 clusters
# Visualize
fviz_cluster(pam.res)



# Hierarchical clustering =====

# Elbow method (señala que el óptimo son dos clusters)
fviz_nbclust(datos_dto_orange2[,c(4,5)], hcut, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method (señala que el óptimo son tres clusters)
fviz_nbclust(datos_dto_orange2[,c(4,5)], hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic (señala que el óptimo son dos clusters)
set.seed(123)
fviz_nbclust(datos_dto_orange2[,c(4,5)], hcut, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")


# Compute gap statistic for kmeans
# we used B = 10 for demo. Recommended value is ~500
gap_stat <- clusGap(datos_dto_orange2[,c(4,5)], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 500)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)


# Compute hierarchical clustering
res.hc <- datos_dto_orange2[,c(4,5)] %>%
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)



# Compute hierarchical clustering on principal components

res.hcpc <- HCPC(datos_dto_orange2[,c(4,5)], graph = FALSE)
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

#graph
fviz_cluster(res.hcpc,
             repel = FALSE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

#muestra los primeros 10 renglones con el dato correspondiente del cluster
head(res.hcpc$data.clust, 10)





#### PCA para variables nuevas ####

#se crea un dataframe con las variables nuevas usadas en la tipologia 
Dto_All_variables_ind2019_nuevas <- Dto_All_variables_ind2019_3[, -c(6,7,11:13)]
Dto_All_variables_ind2019_nuevas$"Edo_Dto" <- Dto_All_variables_ind2019_2$"Edo_Dto" #añadimos nuevamente la columna

#cambiamos los rownames por la columna Edo_Dto
Dto_All_variables_ind2019_nuevas <- Dto_All_variables_ind2019_nuevas %>% 
  remove_rownames %>% 
  column_to_rownames(var="Edo_Dto")


#aplicamos PCA con centrado y normalizacion de variables
variables_nuevas.pca <- prcomp(Dto_All_variables_ind2019_nuevas, center = TRUE, scale = TRUE)

#resumen de PCA y contribucion de variables a cada PC
summary(variables_nuevas.pca)
print(variables_nuevas.pca)

#grafica de varianza explicada
plot(variables_nuevas.pca, type = "l")
fviz_eig(variables_nuevas.pca)

#graficaS de PC1 y PC2

plot(variables_nuevas.pca$x[,1:2])

autoplot(variables_nuevas.pca,label = TRUE, data =  Dto_All_variables_ind2019_orig , label.size = 3)

fviz_pca_ind(variables_nuevas.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE)     # Avoid text overlapping

# Resultados por dto
res.ind2 <- get_pca_ind(variables_nuevas.pca)
pca_vnuevas <- res.ind2$coord          # Coordinates
res.ind2$contrib        # Contributions to the PCs
res.ind2$cos2           # Quality of representation 





#### Clusters para PC obtenidos con variables nuevas ####

# K means =====

#calculo del numero optimo de clusters con el metodo gap para K-means
fviz_nbclust(datos_dto_orange2[,c(8,9)], kmeans, method = "gap_stat") #señala que con 4 clusters se pueden agrupar los datos

# Visualización de K-means con 2 clusters
set.seed(123)
km.res <- kmeans(datos_dto_orange2[,c(8,9)], 2, nstart = 25)
fviz_cluster(km.res, data = datos_dto_orange2[,c(8,9)],
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal()) +
  labs(subtitle = "Método k-means (óptimo)")


#gráficas que muestran el numero optimo de clusters con diferentes metodos
# ver https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/

# Elbow method
fviz_nbclust(datos_dto_orange2[,c(8,9)], kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(datos_dto_orange2[,c(8,9)], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(datos_dto_orange2[,c(8,9)], kmeans, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")


# Visualización con 3 clusters
set.seed(123)
km.res4 <- kmeans(datos_dto_orange2[,c(8,9)], 3, nstart = 25)
fviz_cluster(km.res4, data = datos_dto_orange2[,c(8,9)],
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())


# PAM (Partitioning (clustering) of the data into k clusters “around medoids”, 
# a more robust version of K-means.)
pam.res <- pam(datos_dto_orange2[,c(8,9)], 9) #con 9 clusters
# Visualize
fviz_cluster(pam.res)


# Hierarchical clustering =====

# Compute hierarchical clustering
res.hc <- datos_dto_orange2[,c(8,9)] %>%
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 9, # Cut in 9 groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)



# Compute hierarchical clustering on principal components

res.hcpc <- HCPC(datos_dto_orange2[,c(8,9)], graph = FALSE)
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

#graph
fviz_cluster(res.hcpc,
             repel = FALSE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)





# Referencias ####

#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

#https://www.datacamp.com/community/tutorials/pca-analysis-r

#https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html

#https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/

#https://datasciencelab.wordpress.com/tag/gap-statistic/

#https://stats.stackexchange.com/questions/95290/how-should-i-interpret-gap-statistic

#https://uc-r.github.io/kmeans_clustering
