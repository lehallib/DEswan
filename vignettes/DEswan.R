## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.show='hold'----------------------------------------------------
# library(devtools)
# devtools::install_github("lehallib/DEswan")

## ---- fig.show='hold'----------------------------------------------------
# devtools::install("../DEswan")
library("DEswan")
head(agingplasmaproteome[,1:5])

## ---- fig.show='hold',fig.height = 4, fig.width = 4----------------------

# distribution of qt
hist(agingplasmaproteome[,1],main="qt distribution",xlab="Age (years)")

# covariates
table(agingplasmaproteome[,2])
table(agingplasmaproteome[,3])


## ---- fig.show='hold',error=FALSE----------------------------------------

head(agingplasmaproteome[,1:5])
start_time <- Sys.time()
res.DEswan=DEswan(data.df = agingplasmaproteome[,-c(1:3)],
# res.DEswan=DEswan(data.df = agingplasmaproteome[,c(10:50)],
                  qt = agingplasmaproteome[,1],
                  window.center = seq(25,105,1),
                  buckets.size = 20,
                  covariates = agingplasmaproteome[,c(2:3)])
end_time <- Sys.time()
end_time-start_time

## ---- fig.show='hold'----------------------------------------------------
head(res.DEswan$p)
head(res.DEswan$coeff)

