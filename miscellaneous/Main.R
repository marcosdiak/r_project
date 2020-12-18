path <- "~/Documents/Bootcamp/Bootcamp/my_bootcamp_files/bootcamp-files/R/Proyecto/r_project/"

setwd(path)

lapply(paste0("R/", list.files(path = "R/", recursive = TRUE)), source)

library("readxl")
library('dplyr')
library('reshape')
library('DAAG')

debug(asesinatos)
asesinatos(path)
undebug(asesinatos)