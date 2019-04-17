#setwd("/media/grosati/Elements/PEN/KINGSTON/PEN2/PIMSA/Investigaciones/Boostrap Jorrat/")

#' Realiza el formateo de la base de datos, generando la variable de identificación del estrato.
#' @param path string que contiene la ruta al archivo que contiene la base.
#' @return un dataframe formateado
#' @export

format_dataset <- function(path){
        df <- foreign::read.spss(path, to.data.frame = TRUE, use.value.labels = TRUE)


        idf <- c('AMBA + GCba', 'Gran Rosario', 'Gran Mendoza', 'Gran La Plata',
                 'Gran SM Tucuman', 'Gran Mar del Plata', 'Gran Santa Fe')

        df$base<-droplevels(df$base)

        df$estrato_e1<-NA
        df$estrato_e1[df$aglomerado=='\u00c1rea Metropolitana de Buenos Aires'] <- 'AMBA + GCba'
        df$estrato_e1[df$aglomerado=='Gran C\u00f3rdoba'] <- 'AMBA + GCba'
        df$estrato_e1[df$aglomerado=='Gran Rosario'] <- 'Gran Rosario'
        df$estrato_e1[df$aglomerado=='Gran Mendoza'] <- 'Gran Mendoza'
        df$estrato_e1[df$aglomerado=='Gran La Plata'] <- 'Gran La Plata'
        df$estrato_e1[df$aglomerado=='Gran San Miguel de Tucum\u00e1n'] <- 'Gran SM Tucuman'
        df$estrato_e1[df$aglomerado=='Gran Mar del Plata'] <- 'Gran Mar del Plata'
        df$estrato_e1[df$aglomerado=='Gran Santa Fe'] <- 'Gran Santa Fe'

        df$forzoso <- df$estrato_e1 %in% idf

        ## REVISAR
        df$estrato_e1[df$urbrur=='Urbano' & df[['tama\u00f1o']]=='Aglomerados con m\u00e1s de medio mill\u00f3n de habitantes (no AMBA)' & df$forzoso==FALSE] <- "Aglo > 500.000"
        df$estrato_e1[df$urbrur=='Urbano' & df[['tama\u00f1o']]=='Aglomerados con menos de medio mill\u00f3n de habitantes y m\u00e1s de cien mil' & df$forzoso==FALSE] <- "Aglo > 100.000 y < 500.000"
        df$estrato_e1[df$urbrur=='Urbano' & df[['tama\u00f1o']]=='Aglomerados con menos de cien mil habitantes y m\u00e1s de cincuenta mil' & df$forzoso==FALSE] <- "Aglo > 50.000 y < 100.000"
        df$estrato_e1[df$urbrur=='Urbano' & df[['tama\u00f1o']]=='Aglomerados con menos de cincuenta mil habitantes y m\u00e1s de dos mil' & df$forzoso==FALSE] <- "Algo > 2.000 y < 50.000"
        df$estrato_e1[df$urbrur=='Urbano' & df[['tama\u00f1o']]=='Aglomerados con menos de dos mil habitantes y poblaci\u00f3n rural' & df$forzoso==FALSE] <- "Aglo < 2.000 y rural"
        df$estrato_e1[df$urbrur=='Rural'] <- 'Aglo < 2.000 y rural'

        df$estrato_final <- paste(df$base, df$estrato_e1)
        df$estrato_final<-as.factor(df$estrato_final)
        #df$estrato_e1<-as.integer(df$estrato_e1)

        return(df)
}

