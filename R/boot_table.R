#' Calcula coeficientes de variación para una tabla de contingencia, mediante bootstrap
#'
#' @param data Una tabla o dataframe con los datos de la encuesta
#' @param R cantidad de repeticiones boostrap
#' @param strata variable con el identificador de estratos
#' @param formula variables que conforman la tabla de contingencia. Deben estar especficiados en formato de fórmula: \code{y~x}
#' @return una lista con dos elementos: la tabla original y la tabla de coeficientes de variación
#' @export

# Agregar distribucion de frecuencias

cv_raw_table <- function(data, R, strata, formula){
        print('Bootstraping table...')
        t0 <- proc.time()

        table_orig <- gen_table(formula=formula, data=data, format='table')

        r <- boot::boot(data=data,
                        statistic=gen_table,
                        R=R,
                        strata=strata,
                        formula=formula,
                        format='vector')

        results <- format_results(r=r, table_orig=table_orig)

        t1 <- proc.time() - t0
        print(t1)
        return(results)
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


#' Formatea los resultados en una lista.
#'
#' @param r resultado de la aplicación del bootstrap
#' @param table_orig tabla original a boostrapear
#' @return una lista
#' @export

format_results <- function(r, table_orig){

        cvs <- apply(r$t, 2, stats::sd) / r$t0 * 100

        row <- nrow(table_orig)
        col <- ncol(table_orig)

        cv_final <- matrix(cvs, nrow=row,
                           ncol=col,
                           byrow = FALSE,
                           dimnames = dimnames(table_orig))

        results <- list(table_orig=table_orig ,
                        reps=r['t'], cvs=cvs,
                        cv_final=cv_final)

        res <- list(table=table_orig, cv=cvs, cv_fin=cv_final, results=results)
        return(res)

}


