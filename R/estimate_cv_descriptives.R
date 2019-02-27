#' Realiza la estimación del coeficiente de variación y los intervalos de confianza sobre una medida descriptiva
#' -por ahora, \code{bmean}, \code{bquantile}, \code{sd}-.
#' @param data Un dataframe con los datos de la encuesta
#' @param col variable sobre la que se busca calcular la media, Debe pasarse entre comillas
#' @param statistic estadísticos obre el que se quiere calcular el cv y cint
#' @param R cantidad de repeticiones bootstrap
#' @param strata variable con la identificación de los estratos
#' @return una lista
#' @export


boot_descriptive <- function(data, col, statistic, R, strata){
        dd <- data[, col, drop=FALSE]
        bo <- boot::boot(dd, statistic=statistic, R=R, strata=strata)
        return(bo)
        #AGREGAR COEFICINETE DE VARIACION
        #DEBERIA DEVOLVER UNA LISTA CON EL CINT y el CV
}




#' Wrapper para utilizar la función \code{mean()} como base para calcular intervalos de confienza mediante bootstrap
#' @param data Un dataframe con los datos de la encuesta
#' @param indices argument interno para realizar el bootstrap
#' @return un \code{numeric} con la media
#' @export

bmean <- function(data, indices){
        d <- data[indices, ]
        return(mean(d))
}

#' Wrapper para utilizar la función \code{median()} como base para calcular intervalos de confienza mediante bootstrap
#' @param data Un dataframe con los datos de la encuesta
#' @param indices argument interno para realizar el bootstrap
#' @return un \code{vector} con los cuantiles especificados
#' @export

bmedian <- function(data, indices){
        d <- data[indices, ]
        med <- stats::median(d)
        return(med)
}

#' NO FUNCIONAL Wrapper para utilizar la función \code{quantile()} como base para calcular intervalos de confienza mediante bootstrap
#' @param data Un dataframe con los datos de la encuesta
#' @param q vector con las probabilidades de los cuantiles a calcular.
#' @param indices argument interno para realizar el bootstrap
#' @return un \code{vector} con los cuantiles especificados
#' @export

bquantile <- function(data, q=c(0.25, 0.5, 0.75), indices){
        d <- data[indices, ]
        qt <- stats::quantile(d, probs=q)
        return(qt)
}


#' Wrapper para utilizar la función \code{sd()} como base para calcular intervalos de confienza mediante bootstrap
#' @param data Un dataframe con los datos de la encuesta
#' @param indices argument interno para realizar el bootstrap
#' @return un \code{numeric} con el desvío estándar
#' @export


bsd <- function(data, indices){
        d <- data[indices,]
        return(stats::sd(d))
}
