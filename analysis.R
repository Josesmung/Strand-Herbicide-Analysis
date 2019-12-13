library(dplyr)

data <- read.csv("data/HerbicideData.csv", stringsAsFactors = F)

Triclopyr <- select(data,
                    ï..Triclopyr,
                    Rep,
                    Treatment....herbicide,
                    Percent.necrosis)

test <- Triclopyr[ , "Percent.necrosis"]
test2 <- as.double(gsub('.{1}$', '', test))


Twater <- filter(Triclopyr,
                 Treatment....herbicide == "0%")
Tundiluted <- filter(Triclopyr,
                     Treatment....herbicide == "100%")
T43herb <- filter(Triclopyr,
              Treatment....herbicide == "43%")
T33herb <- filter(Triclopyr,
                  Treatment....herbicide == "33%")

#Water T-test
wildType <- (filter(Twater,ï..Triclopyr == "Wild type"))
test2 <- as.double(gsub('.{1}$', '', wildType[ , "Percent.necrosis"]))
# wildmean <- mean(wildType[, "Percent.necrosis"], na.rm = TRUE)



Glyphosate <- select(data,
                     Glyphosate,
                     Rep.1,
                     Treatment....herbicide.1,
                     Percent.necrosis.1
                     )