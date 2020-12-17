path <- "~/r_project/"

setwd(path)

lapply(paste0("R/", list.files(path = "R/", recursive = TRUE)), source)


debug(asesinatos)
asesinatos(path)
undebug(asesinatos)