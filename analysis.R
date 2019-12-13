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
#WT
wildTwater <- as.double(gsub('.{1}$', '',
          filter(Twater,ï..Triclopyr == "Wild type")[ , "Percent.necrosis"]))
#VD3
VD3Twater <- as.double(gsub('.{1}$', '', 
         filter(Twater,ï..Triclopyr == "VD3")[ , "Percent.necrosis"]))

#T-test
print(t.test(wildTwater, VD3Twater,
             mu = 0,
             alternative = "two.sided",
             paired = FALSE, var.equal = FALSE,
             conf.level = 0.95))

#T test by hand
tStat <- (mean(wildTwater) - mean(VD3Twater)) /
  sqrt((sd(wildTwater)/length(wildTwater)) + (sd(VD3Twater)/length(VD3Twater)))

df <- (sd(wildTwater)^2/length(wildTwater) + sd(VD3Twater)^2/length(VD3Twater))^2 /
  (((sd(wildTwater)^2/length(wildTwater))^2 / (length(wildTwater)-1)) +
     ((sd(VD3Twater)^2/length(VD3Twater))^2 / (length(VD3Twater)-1)))


alpha = .05 
t.half.alpha = qt(1 - alpha/2, df = df)
confidenceInt <- c(-t.half.alpha, t.half.alpha) 
print(confidenceInt)
if (tStat > confidenceInt[1] && tStat < confidenceInt[2]) {
  print("Fail to reject null hypothesis")
} else {
  print("Reject null hypothesis")
}




#33% Herbicide 
#WT
wildT33 <- as.double(gsub('.{1}$', '',
                         filter(T33herb,ï..Triclopyr == "Wild type")[ , "Percent.necrosis"]))
#VD3
VD3T33 <- as.double(gsub('.{1}$', '', 
                        filter(T33herb,ï..Triclopyr == "VD3")[ , "Percent.necrosis"]))
#T-test








Glyphosate <- select(data,
                     Glyphosate,
                     Rep.1,
                     Treatment....herbicide.1,
                     Percent.necrosis.1
                     )