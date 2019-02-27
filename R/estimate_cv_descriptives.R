#' Realiza la estimación del coeficiente de variación y los intervalos de confianza sobre una medida descriptiva
#' -por ahora, \code{bmean}, \code{bquantile}, \code{sd}-.
#' @param data Un dataframe con los datos de la encuesta
#' @param col variable sobre la que se busca calcular la media, Debe pasarse entre comillas
#' @param statistic estadísticos obre el que se quiere calcular el cv y cint
#' @param R cantidad de repeticiones bootstrap
#' @param strata variable con la identificación de los estratos
#' @param ci_alpha numeric. alpha para el cálculo del intervalo de confianza
#' @return una lista con dos elementos: $cv, que contiene los coeficientes de variación y $ci que contiene los intervalos de confianza
#' @export


boot_descriptive <- function(data, col, statistic, ci_alpha=0.95, R=1000, strata=NULL){
        dd <- data[, col, drop=FALSE]
        if (is.null(strata)) {
                strata <- rep(1, nrow(dd))}
        r <- boot::boot(dd, statistic=statistic, R=R, strata=strata)
        cvs <- apply(r$t, 2, stats::sd)
        ci <- boot::boot.ci(r, conf=ci_alpha, type = 'norm')
        results <- list(cvs=cvs, ci=ci)
        return(results)
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
