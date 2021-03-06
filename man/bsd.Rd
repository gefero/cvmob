% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_cv_descriptives.R
\name{bsd}
\alias{bsd}
\title{Wrapper para utilizar la función \code{sd()} como base para calcular intervalos de confienza mediante bootstrap}
\usage{
bsd(data, indices)
}
\arguments{
\item{data}{Un dataframe con los datos de la encuesta}

\item{indices}{argument interno para realizar el bootstrap}
}
\value{
un \code{numeric} con el desvío estándar
}
\description{
Wrapper para utilizar la función \code{sd()} como base para calcular intervalos de confienza mediante bootstrap
}
