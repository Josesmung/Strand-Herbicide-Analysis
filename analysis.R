library(dplyr)
library(tidyr)
data <- read.csv("data/HerbicideData.csv", stringsAsFactors = F)

Triclopyr <- select(data,
                    ï..Triclopyr,
                    Rep,
                    Treatment....herbicide,
                    Percent.necrosis)

Twater <- filter(Triclopyr,
                 Treatment....herbicide == "0%")
Tundiluted <- filter(Triclopyr,
                     Treatment....herbicide == "100%")
T43herb <- filter(Triclopyr,
              Treatment....herbicide == "43%")
T33herb <- filter(Triclopyr,
                  Treatment....herbicide == "33%")

Glyphosate <- select(data,
                     Glyphosate,
                     Rep.1,
                     Treatment....herbicide.1,
                     Percent.necrosis.1
)
Gwater <- filter(Glyphosate,
                 Treatment....herbicide.1 == "0%")
Gundiluted <- filter(Glyphosate,
                     Treatment....herbicide.1 == "100%")
G1herb <- filter(Glyphosate,
                  Treatment....herbicide.1 == "1%")
G33herb <- filter(Glyphosate,
                  Treatment....herbicide.1 == "33%")



# T test function for Tric
tric_t_tester <- function(herb_level) {
  #Water T-test
  #WT
  wildT <- as.double(gsub('.{1}$', '',
                filter(herb_level,ï..Triclopyr == "Wild type")[ , "Percent.necrosis"]))
  print(wildT)
  wildT <- wildT[!is.na(wildT)] 
  #VD3
  VD3T <- as.double(gsub('.{1}$', '', 
               filter(herb_level,ï..Triclopyr == "VD3")[ , "Percent.necrosis"]))
  VD3T <- VD3T[!is.na(VD3T)]
  #Calculations
  tStat <- (mean(wildT) - mean(VD3T)) /
    sqrt((sd(wildT)/length(wildT)) + (sd(VD3T)/length(VD3T)))
  
  df <- (sd(wildT)^2/length(wildT) + sd(VD3T)^2/length(VD3T))^2 /
    (((sd(wildT)^2/length(wildT))^2 / (length(wildT)-1)) +
       ((sd(VD3T)^2/length(VD3T))^2 / (length(VD3T)-1)))
  
  
  alpha = .05 
  t.half.alpha = qt(1 - alpha/2, df = df)
  confidenceInt <- c(-t.half.alpha, t.half.alpha) 
  print(confidenceInt)
  if (tStat > confidenceInt[1] && tStat < confidenceInt[2]) {
    print("Fail to reject null hypothesis")
  } else {
    print("Reject null hypothesis")
  }
}

glyph_t_tester <- function(herb_level) {
  #WT
  wildT <- as.double(gsub('.{1}$', '',
                     filter(herb_level,Glyphosate == "Wild type")[ , "Percent.necrosis.1"]))
  wildT <- wildT[!is.na(wildT)] 
  #VD3
  VD3T <- as.double(gsub('.{1}$', '', 
                         filter(herb_level,Glyphosate == "VD3")[ , "Percent.necrosis.1"]))
  VD3T <- VD3T[!is.na(VD3T)]
  #Calculations
  tStat <- (mean(wildT) - mean(VD3T)) /
    sqrt((sd(wildT)/length(wildT)) + (sd(VD3T)/length(VD3T)))
  
  df <- (sd(wildT)^2/length(wildT) + sd(VD3T)^2/length(VD3T))^2 /
    (((sd(wildT)^2/length(wildT))^2 / (length(wildT)-1)) +
       ((sd(VD3T)^2/length(VD3T))^2 / (length(VD3T)-1)))
  
  
  alpha = .05 
  t.half.alpha = qt(1 - alpha/2, df = df)
  confidenceInt <- c(-t.half.alpha, t.half.alpha) 
  print(confidenceInt)
  if (tStat > confidenceInt[1] && tStat < confidenceInt[2]) {
    print("Fail to reject null hypothesis")
  } else {
    print("Reject null hypothesis")
  }
}



# All T-Tests
tric_t_tester(Twater)
tric_t_tester(Tundiluted)
tric_t_tester(T43herb)
tric_t_tester(T33herb)


glyph_t_tester(Gundiluted)
glyph_t_tester(G1herb)
glyph_t_tester(G33herb)



# ANOVA test: Is there a difference between Tric and Glyph necrotic levels at 
# same herbicide levels?
# 2 way anova

# dependent (measurement) - percent necrosis 
# indepedendent (nominal variables) - plant type (VD3, Wild), herbicide concentration
anova_data <- read.csv("data/AnovaData.csv", stringsAsFactors = F)


tric_nec <- as.double(gsub('.{1}$', '', 
                      filter(anova_data, Plant == "Triclopyr")[ , "Percent.necrosis"]))

tric_type <- filter(anova_data, Plant == "Triclopyr")[, "Type"]
tric_herb_concentration <- as.integer(gsub('.{1}$', '', 
                          filter(anova_data, Plant == "Triclopyr")[, "Treatment....herbicide"]))




glyph_nec <- as.double(gsub('.{1}$', '', 
                           filter(anova_data, Plant == "Glyphosate")[ , "Percent.necrosis"]))

glyph_type <- filter(anova_data, Plant == "Glyphosate")[, "Type"]
glyph_herb_concentration <- as.integer(gsub('.{1}$', '', 
                            filter(anova_data, Plant == "Glyphosate")[, "Treatment....herbicide"]))


print("Triclopyr ANOVA results")
print(summary(aov(tric_nec ~ tric_type + tric_herb_concentration)))
print("")
print("Glyphosate ANOVA results")
print(summary(aov(glyph_nec ~ glyph_type + glyph_herb_concentration)))




