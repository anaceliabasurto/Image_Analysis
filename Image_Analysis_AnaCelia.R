## Analisis de imagen de frutos
library(EBImage)
library(Momocs)
library(ggplot2)
library(gridExtra)

## directorio que contiene todas las fotos
dir<-"~/AnaCelia_MSc/FOTOS/CHILES_2019_F3/CHILES_2019_COSECHA2/"


## nos vamos al directorio de interes
setwd(dir)


## encuentra todas las fotos en el directorio, con el patron .tif
allpictures<-list.files(dir,'.tif')

# Se crea una lista vacia para insertar todos los datos
data<-list()

# Se crea un loop 
for (ghgh in 1:length(allpictures)){
  
  
  ## foto para analizar en este loop
  pic<-allpictures[ghgh]
  
  
  ## leer imagen
  A<-readImage(pic)
  display(A)
  dims<-round(dim(A)[1:2]*.5)
  ##View(dims)
  A<-EBImage::resize(A,dims[1],dims[2])
  
  ## exploramos los canales de color
  B <- channel(A, mode = 'red')
  display(B)
  
  B2 <- channel(A, mode = 'green')
  display(B2)
  
  B3 <- channel(A, mode = 'blue')
  display(B3)
  
  R <- (B < .4)
  display(R)
  
  R2 <- (B2 <.4)
  display(R2)
  
  R3 <- (B3 >.4)
  display(R3)
  
  R4<-R+R2+R3
  display(R4)
  
  
  ## se hace esta lista para obtener los datos de color
  D <- list(r = A[, , 1], g = A[, , 2], b = A[, , 3])
  ##View(D)
  
  ## Se rellenan los huecos y se genera una imagen binaria
  ope0<-round(nrow(R4)/100)
  C <- EBImage::thresh(R4,ope0,ope0, 0.05)
  C <- EBImage::opening(R4, EBImage::makeBrush(3, shape = "disc"))
  binaryImage <- EBImage::fillHull(C)
  
  
  ## reconoce cada objeto en la imagen
  labeledImage <- EBImage::bwlabel(binaryImage)
  
  display(labeledImage)
  
  ## se cuentan los pixeles por objeto en la imagen
  tt<-table(labeledImage)
  
  
  
  
  ## mediciones de color
  intensityMeasurements <- lapply(D, computeFeatures.basic, 
                                  x = labeledImage, properties = F, basic.quantiles = 0.05)
  
  ## poner datos de color en un dataframe, con los colnames correctos
  int<-do.call(cbind,intensityMeasurements)
  colnames(int)<-c('r.mean','r.sd','r.mod','r.q005','g.mean','g.sd','g.mod','g.q005','b.mean','b.sd','b.mod','b.q005')
  int<-as.data.frame(int)
  
  
  ## SE USA MOMOCS PARA ANALISIS DE FORMA  
  
  ## extraer contornos
  bondCont<-ocontour(labeledImage)
  
  
  superX<-Out(bondCont)
  
  area<-coo_area(superX)
  len<-coo_length(superX)
  wid<-coo_width(superX)
  per<-unlist(coo_perim(superX))
  
  
  data<-data.frame(int,area,len,wid,per,pic)
  
  data[[ghgh]]<-x
  
}

hola<-do.call(rbind,todo) 
hola$tipo<-'fruto'
f<-which(hola$b.mean>.6)
hola$tipo[f]<-'papelito'

table(hola$tipo)
tmp<-data.frame(id=hola$pic,len=hola$len,wid=hola$wid,area=hola$area,r=hola$r.mean,g=hola$g.mean,b=hola$b.mean)
tmp$lw<-tmp$len/tmp$wid

uni_fotos<-unique(tmp$id)
View(uni_fotos)

ttt<-numeric()
for (i in 1:length(uni_fotos)){
  x<-tmp[which(tmp$id==uni_fotos[i]),]
  ttt<-c(ttt,c(rep('fruto',nrow(x)-1),'ref'))  
}
tmp$type<-ttt
tmp2<-tmp


## normalizar
uni_fotos<-unique(tmp2$id)
normdata<-numeric()
for (i in 1:length(uni_fotos)){
  x<-tmp2[which(tmp2$id==uni_fotos[i]),]
  
  refvalue<-x[which(x$type=='ref'),]
  fruitvalue<-x[which(x$type=='fruto'),]
  
  len<-(fruitvalue$len*2)/refvalue$len
  wid<-fruitvalue$wid/refvalue$wid
  area<-(fruitvalue$area*2)/refvalue$area
  
  normdata<-rbind(normdata,cbind(uni_fotos[i],len,wid,area,fruitvalue[,5:8]))
  
}

colnames(normdata)[1]<-'id'
View(normdata)

write.csv(normdata, file="datos_frutos_cosecha2.csv")






