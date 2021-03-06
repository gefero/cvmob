% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_cv_table.R
\name{gen_table}
\alias{gen_table}
\title{Genera una tabla de contingencia}
\usage{
gen_table(formula, data, indices, format = "vector")
}
\arguments{
\item{formula}{variables que conforman la tabla de contingencia. Deben estar especficiados en formato de fórmula: \code{y~x}}

\item{data}{Una tabla o dataframe con los datos de la encuesta}

\item{indices}{argument interno para realizar el bootstrap}

\item{format}{formato en el que se desea la tabla. Vector, flat, table}
}
\value{
una tabla original
}
\description{
Genera una tabla de contingencia
}
