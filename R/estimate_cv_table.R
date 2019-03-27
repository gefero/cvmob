#' Calcula coeficientes de variación para una distribución de frecuencias univariada, mediante bootstrap
#'
#' @param data Una tabla o dataframe con los datos de la encuesta
#' @param R cantidad de repeticiones boostrap
#' @param strata variable con el identificador de estratos
#' @param var string con las variables que conforman la tabla de contingencia. Deben estar especficiados en formato de fórmula: \code{y~x}
#' @return una lista con dos elementos: la tabla original y la tabla de coeficientes de variación
#' @export

cv_raw_freq <- function(data, R, strata, var){

        table_orig <- gen_frequence(var=var, data=data)

        r <- boot::boot(data=data,
                        statistic=gen_frequence,
                        R=R,
                        strata=strata,
                        var=var)
        results <- calc_cvs_freq(r=r, table_orig=table_orig)
        return(results)
}



#' Calcula coeficientes de variación para una tabla de contingencia, mediante bootstrap
#'
#' @param data Una tabla o dataframe con los datos de la encuesta
#' @param R cantidad de repeticiones boostrap
#' @param strata variable con el identificador de estratos
#' @param formula variables que conforman la tabla de contingencia. Deben estar especficiados en formato de fórmula: \code{y~x}
#' @return una lista con dos elementos: la tabla original y la tabla de coeficientes de variación
#' @export


cv_raw_table <- function(data, R, strata, formula=NULL){
        print('Bootstraping table...')
        t0 <- proc.time()

        table_orig <- gen_table(formula=formula, data=data, format='table')

        r <- boot::boot(data=data,
                        statistic=gen_table,
                        R=R,
                        strata=strata,
                        formula=formula,
                        format='vector')

        results_f <- calc_cvs_table(r=r, table_orig=table_orig)


        t1 <- proc.time() - t0
        print(t1)
        return(results_f)
}


#' Genera una tabla de contingencia
#'
#' @param formula variables que conforman la tabla de contingencia. Deben estar especficiados en formato de fórmula: \code{y~x}
#' @param data Una tabla o dataframe con los datos de la encuesta
#' @param indices argument interno para realizar el bootstrap
#' @param format formato en el que se desea la tabla. Vector, flat, table
#' @return una tabla original
#' @export

gen_table <- function(formula, data, indices, format='vector'){
        formula <- stats::as.formula(formula)

        #Sanity check de parametros
        if (class(formula) != "formula") stop('formula debe estar escrito en formato de formula: x~y')
        if (class(data) != "data.frame") stop('data debe ser un dataframe, una matriz o una tibble')
        assertthat::assert_that(format %in% c('vector', 'flat', 'table'), msg = 'format debe ser igual a vector, table o flat')

        d <- data[indices,]
        t <- stats::ftable(formula, data=d)

        if (format == 'flat'){
                table_final <- as.data.frame(t)
                return(table_final)
        }
        if (format == 'vector'){
                table_final <- as.data.frame(t)
                return(table_final[,ncol(table_final)])
        }

        if (format == 'table') {
                table_final <- t
                return(as.table(table_final))
        }
}


#' Genera una distribución de frecuencias univariada
#'
#' @param var string con las variables que conforman la tabla de contingencia. Deben estar especficiados en formato de fórmula: \code{y~x}
#' @param data Una tabla o dataframe con los datos de la encuesta
#' @param indices argument interno para realizar el bootstrap
#' @return una tabla original
#' @export

gen_frequence <- function(var, data, indices){
        d <- data[indices,]
        t <- stats::ftable(d[[var]])
        return(as.table(t))
}


#' Calcula coef de variación de una distribución de frecuencias en una lista.
#'
#' @param r resultado de la aplicación del bootstrap
#' @param table_orig tabla original a boostrapear
#' @return una lista
#' @export

calc_cvs_freq <- function(r, table_orig){
        cvs_final <- apply(r$t, 2, stats::sd) / r$t0 * 100

        return(cvs_final)

}


#' Calcula coef de variación de una tabla de contingencia en una lista.
#'
#' @param r resultado de la aplicación del bootstrap
#' @param table_orig tabla original a boostrapear
#' @return una lista
#' @export

calc_cvs_table <- function(r, table_orig){

        cvs <- apply(r$t, 2, stats::sd) / r$t0 * 100

        dims <- dim(table_orig)

        cv_final <- array(cvs,
                           dim=dims,
                           dimnames = dimnames(table_orig))

        results_f <- format_results_table(table_orig, r, cvs, cv_final)

        return(results_f)

}



#' Formatea resultados para la salida final
#'
#' @param r resultado de la aplicación del bootstrap
#' @param table_orig tabla original a boostrapear
#' @param cvs cvs calculados
#' @param cv_final matriz o vector con calculos de cv
#' @return una lista
#' @export

format_results_table <- function(table_orig, r, cvs, cv_final){

        res <- list(table_orig=table_orig ,
                        reps=r['t'], cvs=cvs,
                        cv_final=cv_final)

        results <- list(table=table_orig,
                        cv=cvs,
                        cv_fin=cv_final)
}
