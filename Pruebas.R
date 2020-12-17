install.packages("readxl")
library("readxl")

children <- read.csv('data/children_per_woman_total_fertility.csv', check.names=FALSE)
paro <- read.csv('data/long_term_unemployment_rate_percent.csv', check.names=FALSE)
pib <- read.csv('data/total_gdp_us_inflation_adjusted.csv', check.names=FALSE)
women <- read.csv('data/murdered_women_per_100000_people.csv', check.names=FALSE)
population <- read_excel('data/Data Population - v5 - 1800 to 2100 World Regions and Countries by Gapminder.xlsx',
                         sheet = 'data-countries-etc-by-year-colu')

# Como 'population' traía por defecto columnas no necesarias, las elimino
install.packages("dplyr")
library('dplyr')
population <- select(population, -1, -3)

# 'population' tiene los años con decimales (1900.0), hay que corregirlo
# Como 'population' y 'children' tienen el mismo rango de tiempo igualo el nombre de las columnas

names(population)
names(population) <- names(children)

##

# Hay que hacer melt()

##

install.packages("reshape")
library('reshape')

pib_melted <- melt(pib, na.rm = F)
names(pib_melted) <- c('Country', 'Año', 'PIB')

paro_melted <- melt(paro, na.rm = F)
names(paro_melted) <- c('Country', 'Año', 'Paro')

children_melted <- melt(children, na.rm = F)
names(children_melted) <- c('Country', 'Año', 'Children')

population_melted <- melt(as.data.frame(population), na.rm = F)
names(population_melted) <- c('Country', 'Año', 'Population')

women_melted <- melt(women, na.rm = F)
names(women_melted) <- c('Country', 'Año', 'Women')


# Hacemos el merge()

merge1 <- merge(x = pib_melted, y = paro_melted, by = c('Country', 'Año'), all = T)
merge2 <- merge(x = merge1, y = children_melted, by = c('Country', 'Año'), all = T)
merge3 <- merge(x = merge2, y = population_melted, by = c('Country', 'Año'), all = T)
merge4 <- merge(x = merge3, y = women_melted, by = c('Country', 'Año'), all = T)

merge <- merge4

# Filtramos dataframe para que nos quedemos sólo con datos a partir de 1990 y hasta 2017
# ¿Por qué 1990? Porque en el dataset de 'Paro' tenemos datos a partir de ese año

merge[merge['Año'] > 1990]  # da error porque los datos de 'Año' son 'factors'
str(merge)

as.numeric(as.character(merge['Año']))
