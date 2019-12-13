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
#mean for WT
wildT <- (filter(Twater,ï..Triclopyr == "Wild type"))
cwildT <- as.double(gsub('.{1}$', '', wildT[ , "Percent.necrosis"]))
wildMean <- mean(cwildT, na.rm = TRUE)

#mean for VD3
VD3T <- (filter(Twater,ï..Triclopyr == "VD3"))
cVD3T <- as.double(gsub('.{1}$', '', VD3T[ , "Percent.necrosis"]))
VD3Mean <- mean(cwildT, na.rm = TRUE)

#T-test

print(t.test(cwildT, cVD3T,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95))


Glyphosate <- select(data,
                     Glyphosate,
                     Rep.1,
                     Treatment....herbicide.1,
                     Percent.necrosis.1
                     )