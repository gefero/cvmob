# Funcion para generar tabla

library(boot)
library(glmnet)
library(Matrix)

# Agregar distribucion de frecuencias


# Generador de tablas
gen_table <- function(formula, data, indices, format='vector'){

        d <- data[indices,]
        t <- ftable(formula, data=d)

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


format_results <- function(r, table_orig){

        cvs <- apply(r$t, 2, sd) / r$t0 * 100

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


cv_raw_table.boot <- function(data, statistic, R, strata, formula){
        print('Bootstraping table...')
        t0 <- proc.time()

        table_orig <- gen_table(formula=formula, data=data, format='table')

        r <- boot(data=data,
                  statistic=statistic,
                  R=R,
                  strata=strata,
                  formula=formula,
                  format='vector')

        results <- format_results(r=r, table_orig=table_orig)

        t1 <- proc.time() - t0
        print(t1)
        return(results)
}
