#setwd("/media/grosati/Elements/PEN/KINGSTON/PEN2/PIMSA/Investigaciones/Boostrap Jorrat/")

#' Realiza el formateo de la base de datos, generando la variable de identificación del estrato.
#' @param path string que contiene la ruta al archivo que contiene la base.
#' @return un dataframe formateado
#' @export


format_dataset <- function(path){
        df <- foreign::read.spss(path, to.data.frame = TRUE)
        df$aglomerado1<-as.integer(df$aglomerado)
        df$tamano1<-as.integer(df[['tama\u00f1o']])
        id <- c(1,2,3,4,5,6,7,10)
        df$forzoso <- as.integer(df$aglomerado) %in% id

        df$estrato_e1<-NA
        df$estrato_e1[df$aglomerado1==1] <- 'AMBA'
        df$estrato_e1[df$aglomerado1==2] <- 'Gran CBA'
        df$estrato_e1[df$aglomerado1==3] <- 'Gran Rosario'
        df$estrato_e1[df$aglomerado1==4] <- 'Gran Mendoza'
        df$estrato_e1[df$aglomerado1==5] <- 'Gran La Plata'
        df$estrato_e1[df$aglomerado1==6] <- 'Gran SM Tucuman'
        df$estrato_e1[df$aglomerado1==7] <- 'Gran Mar del Plata'
        df$estrato_e1[df$aglomerado1==10] <- 'Gran Santa Fe'
        df$estrato_e1[df$urbrur=='Urbano' & df$tamano1==2 & df$forzoso==0] <- "Aglo > 500000"
        df$estrato_e1[df$urbrur=='Urbano' & df$tamano1==3 & df$forzoso==0] <- "Aglo > 50000 y < 500000"
        df$estrato_e1[df$urbrur=='Urbano' & df$tamano1==4 & df$forzoso==0] <- "Aglo > 50000 y < 500000"
        df$estrato_e1[df$urbrur=='Urbano' & df$tamano1==5 & df$forzoso==0] <- "Aglo < 2000"
        df$estrato_e1[df$urbrur=='Urbano' & df$tamano1==6 & df$forzoso==0] <- "Aglo > 2000 y < 50000"
        df$estrato_e1[df$urbrur=='Rural'] <- 'Rural'

        df$estrato_e1<-as.factor(df$estrato_e1)
        df$estrato_e1<-as.integer(df$estrato_e1)

        return(df)
}