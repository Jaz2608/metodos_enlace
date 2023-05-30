pacman::p_load(NbClust,ClustGeo,ggplot2,readxl,purrr,
               cluster,factoextra,tidyr,dplyr,scales,
               mlbench,showtext,paletteer)

font_add_google(c("Poppins"))
showtext_auto()


metodos <- c("single","complete","average","centroid","ward")

# esta función obtiene la agrupación para los métodos:
# single, complete, average, centroid y ward
obten_clusters <- function(data, k){
  
  distance <- dist(scale(data), method = 'euclidean')
  complete <- hclust(distance, method = 'complete')
  data$complete <- cutree(complete, k = k)
  
  average <- hclust(distance, method = 'average')
  data$average <- cutree(average, k = k)
  
  single <- hclust(distance, method = 'single')
  data$single <- cutree(single, k = k)
  
  centroid <- hclust(distance, method = 'centroid')
  data$centroid <- cutree(centroid, k = k)
  
  ward <- hclust(distance, method = 'ward.D')
  data$ward <- cutree(ward, k = k)
  
  data <- data%>%
    mutate(complete = as.factor(complete),
           average = as.factor(average),
           single = as.factor(single),
           centroid = as.factor(centroid),
           ward = as.factor(ward))
  return(data)
}

# función para crear las gráficas con distintos métodos 
obten_Grafica <- function(data,metodo_enlace,tamanio_punto){
  # la variable x debe llamarse V1 
  # la variable y debe llamarse V2
  G <- ggplot(data,aes(x=V1,y=V2))+
    theme_void()+
    theme(text=element_text(family="Poppins", size=10),
          plot.title = element_text(face="bold", size=10),
          legend.title = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none")
  
  if(metodo_enlace=="single"){
    G <- G+geom_point(aes(color=factor(single)),size=tamanio_punto)+
      labs(title="Clusters creados con el método single")
    return(G)
  }
  else if(metodo_enlace=="complete"){
    G <- G+geom_point(aes(color=factor(complete)),size=tamanio_punto)+
      labs(title="Clusters creados con el método complete")
    return(G)
  }
  else if(metodo_enlace=="average"){
    G <- G+geom_point(aes(color=factor(average)),size=tamanio_punto)+
      labs(title="Clusters creados con el método average")
    return(G)
  }
  else if(metodo_enlace=="centroid"){
    G <- G+geom_point(aes(color=factor(centroid)),size=tamanio_punto)+
      labs(title="Clusters creados con el método cetroid")
    return(G)
  }
  
  else if(metodo_enlace=="ward"){
    G <- G+geom_point(aes(color=factor(ward)),size=tamanio_punto)+
      labs(title="Clusters creados con el método ward")
    return(G)
  }else{
    G <- paste(metodo_enlace, "No es un método válido.",
      "Elige un método válido: single, complete, centroid, ward o average")
    
  }
  return(print(G))
  
}

# paletas de colores:
paletteer_d("ggthemes::excel_Vapor_Trail")
paletteer_d("ggthemes::excel_Depth")
paletteer_d("ggthemes::excel_Gallery")


setwd("G:/Mi unidad/Tesis/imagenes/métodos de enlace")

# Tres grupos con ruido
set.seed(6)
data <- data.frame(V1=c(rnorm(100,5,2),
                        rnorm(100,1,1),
                        rnorm(100,-2,1)),
                   
                   V2=c(rnorm(100,5,1),
                        rnorm(100,1,1),
                        rnorm(100,5,1)))
plot(data)
data_with_clusters <- obten_clusters(data,3)

for(m in metodos){
  g <- obten_Grafica(data_with_clusters,m,8)+
    scale_color_manual(values=c("#FFE082","#54A021","#FF8F00"))+
    labs(title="")
  plot(g)
  ggsave(g,filename = paste0("tres_clusters_",m,".png"),
         device = "png",width = 5,height = 5, dpi=150)
}


# triangulos
base <- mlbench.shapes(n=800)
data <- as.data.frame(base$x)%>%
  rename(V1=x4)%>%
  filter(V1>0,
         V2>0)

data <- rbind(data%>%
                mutate(V2=V2-.5),
              data%>%
                mutate(V2=(V2-.5)*-1))
plot(data)

data_with_clusters <- obten_clusters(data,2)
for(m in metodos){
  g <- (obten_Grafica(data_with_clusters,m,8)+
         scale_color_manual(values=c("#FF8F00","#54A021")))+
    labs(title="")
  plot(g)
  ggsave(g,filename = paste0("triangulos_",m,".png"),
         device = "png",width = 5,height = 5, dpi=150)
}


# dos clusters de diferentes tamaños (uno muy grande)
genera_circulos <- function(r,a){
  set.seed(6)
  theta = runif(1000, 0,2*pi)
  x = cos(theta) + rnorm(100, 0, 0.03)
  y = sin(theta) + rnorm(100, 0, 0.03)
  data <- data.frame(V1=x,
                     V2=y)
  
  d<-data*r
  plot(d)
  set.seed(6)
  circulo <- rbind(data.frame(V1=runif(1000,-a,a),
                                     V2=runif(1000,-a,a)),d)
  return(circulo)
}
data <- rbind(genera_circulos(10,8),
              genera_circulos(2,1.8)-11)
plot(data)

data_with_clusters <- obten_clusters(data,2)
for(m in metodos){
  g <- obten_Grafica(data_with_clusters,m,8)+
    scale_color_manual(values=c("#FF8F00","#54A021"))+
    labs(title="")
  plot(g)
  ggsave(g,filename = paste0("dos_clusters_",m,".png"),
         device = "png",width = 5,height = 5, dpi=150)
}


# Dos circulos uno dentro y otro fuera
theta = runif(1000, 0,2*pi)
x = cos(theta) + rnorm(100, 0, 0.03)
y = sin(theta) + rnorm(100, 0, 0.03)
data <- data.frame(V1=x,
           V2=y)

data <- rbind(data, data*0.5)
plot(data)
data_with_clusters <- obten_clusters(data,2)
for(m in metodos){
  g <- (obten_Grafica(data_with_clusters,m,3)+
         scale_color_manual(values=c("#FF8F00","#54A021")))+
    labs(title="")
  
  plot(g)
  ggsave(g,filename = paste0("circulos_",m,".png"),
         device = "png",width = 5,height = 5, dpi=150)
}
