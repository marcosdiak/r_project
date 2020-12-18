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

install.packages('DMwR')
library('DAAG')

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

# Filtramos dataframe para quedarnos sólo con datos completos, prescindiendo de los NaN

merge <- na.omit(merge4)
merge_without_country_year <- merge[c(3:7)]

# Tratamiento de datos
  # - Estandarización (no hacemos)
  # - Dividir en train y test (sin X e y)

## Probando seperando sólo train y test

trainingRowIndex <- sample(1:nrow(merge_without_country_year), 0.8*nrow(merge_without_country_year))
trainingData <- merge_without_country_year[trainingRowIndex, ]
testData  <- merge_without_country_year[-trainingRowIndex, ]

lmMod <- lm(Women ~ ., data=trainingData)
predictions <- predict(lmMod, testData)

summary(lmMod)

actuals_preds <- data.frame(cbind(actuals=testData$Women, predicteds=predictions))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 55.80%, min_max accuracy

# MAPE Calculation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 2.04%, mean absolute percentage deviation


# Alternately, you can compute all the error metrics in one go using the regr.eval() function in DMwR package. 
# You will have to install.packages('DMwR') for this if you are using it for the first time.
DMwR::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)

# Cross-validation
cvResults <- suppressWarnings(CVlm(data=merge_without_country_year, form.lm=Women ~ ., 
                                   m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  
attr(cvResults, 'ms')  

# Predecimos Australia 2005

Australia2005 <- filter(merge4, Country == 'Australia' , Año == 2005)
Australia2005 <- Australia2005[c(3:6)]

Australia_predictions <- predict(lmMod, Australia2005)



