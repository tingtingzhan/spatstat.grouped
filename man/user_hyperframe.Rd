% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_hyperframe.R
\name{user_hyperframe}
\alias{user_hyperframe}
\alias{Emark_}
\alias{Emark_.hyperframe}
\alias{Vmark_}
\alias{Vmark_.hyperframe}
\alias{markcorr_}
\alias{markcorr_.hyperframe}
\alias{markvario_}
\alias{markvario_.hyperframe}
\alias{Gcross_}
\alias{Gcross_.hyperframe}
\alias{Jcross_}
\alias{Jcross_.hyperframe}
\alias{Kcross_}
\alias{Kcross_.hyperframe}
\alias{Lcross_}
\alias{Lcross_.hyperframe}
\alias{nncross_.hyperframe}
\title{User Interface of Operations on \link[spatstat.geom]{hyperframe} with One-and-Only-One \link[spatstat.geom]{ppp}-\link[spatstat.geom:hyperframe]{hypercolumn}}
\usage{
Emark_(X, ...)

\method{Emark_}{hyperframe}(X, correction = "none", ...)

Vmark_(X, ...)

\method{Vmark_}{hyperframe}(X, correction = "none", ...)

markcorr_(X, ...)

\method{markcorr_}{hyperframe}(X, correction = "none", ...)

markvario_(X, ...)

\method{markvario_}{hyperframe}(X, correction = "none", ...)

Gcross_(X, ...)

\method{Gcross_}{hyperframe}(X, correction = "none", ...)

Jcross_(X, ...)

\method{Jcross_}{hyperframe}(X, correction = "none", ...)

Kcross_(X, ...)

\method{Kcross_}{hyperframe}(X, correction = "none", ...)

Lcross_(X, ...)

\method{Lcross_}{hyperframe}(X, correction = "none", ...)

\method{nncross_}{hyperframe}(X, ...)
}
\arguments{
\item{X}{a \link[spatstat.geom]{hyperframe}}

\item{...}{additional parameters of user operation}

\item{correction}{\link[base]{character} scalar,
see functions
\link[spatstat.explore]{Emark},
\link[spatstat.explore]{Vmark},
\link[spatstat.explore]{markcorr},
\link[spatstat.explore]{markvario}
\link[spatstat.explore]{Gcross},
\link[spatstat.explore]{Jcross}
\link[spatstat.explore]{Kcross},
\link[spatstat.explore]{Lcross},
etc.
Default \code{'none'} to save computing time.}
}
\value{
See explanations in workhorse functions \link{fv_hyperframe} and \link{dist_hyperframe}.
}
\description{
See workhorse functions \link{fv_hyperframe} and \link{dist_hyperframe}.
}
\details{
See explanations in workhorse functions \link{fv_hyperframe} and \link{dist_hyperframe}.
}
\examples{
library(spatstat.data)

# no good example for [Emark_.hyperframe]
# no hyperframe with ppp-hypercolumn with numeric marks

(flu0 = spatstat.geom::subset.hyperframe(flu, stain == 'M2-M1'))
Gcross_(flu0, i = 'M1', j = 'M2') # Kcross_; Jcross_; Lcross_
nncross_(flu0, i = 'M1', j = 'M2')
}
