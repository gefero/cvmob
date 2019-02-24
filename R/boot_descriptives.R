#' Wrapper para utilizar la función \code{mean()} como base para calcular intervalos de confienza mediante bootstrap
#' @param data Un dataframe con los datos de la encuesta
#' @param col variable sobre la que se busca calcular la media, Debe pasarse entre comillas
#' @param indices argument interno para realizar el bootstrap
#' @return un \code{numeric} con la media
#' @export



bmean <- function(data, col, indices){
        d <- data[indices, col]
        return(mean(d))
}


#' Wrapper para utilizar la función \code{quantile()} como base para calcular intervalos de confienza mediante bootstrap
#' @param data Un dataframe con los datos de la encuesta
#' @param col variable sobre la que se busca calcular la media, Debe pasarse entre comillas
#' @param q número de cuantiles a calcular. q=
#' @param indices argument interno para realizar el bootstrap
#' @return un \code{vector} con los cuantiles especificados
#' @export

bquantile <- function(data, col, q, indices){
        d <- data[indices, col]
        qt <- stats::quantile(d, probs=q)
        return(qt)
}



#' Wrapper para utilizar la función \code{sd()} como base para calcular intervalos de confienza mediante bootstrap
#' @param data Un dataframe con los datos de la encuesta
#' @param col variable sobre la que se busca calcular la media, Debe pasarse entre comillas
#' @param indices argument interno para realizar el bootstrap
#' @return un \code{numeric} con el desvío estándar
#' @export


bsd <- function(data, col, indices){
        d <- data[indices, col]
        return(stats::sd(d))
}
