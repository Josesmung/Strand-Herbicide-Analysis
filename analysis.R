library(dplyr)
library(tidyr)
library(agricolae)
library(TOSTER)

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


tric_df <- data.frame(
  type = tric_type,
  concentration = tric_herb_concentration,
  necrosis = tric_nec
)

glyph_df <- data.frame(
  type = glyph_type,
  concentration = glyph_herb_concentration,
  necrosis = glyph_nec
)

glyph_df$concentration <- as.factor(glyph_df$concentration)
tric_df$concentration <- as.factor(tric_df$concentration)

print("Triclopyr ANOVA results")
tric_aov <- aov(necrosis ~ type + concentration, data = tric_df)
print(summary(tric_aov))
print("")
print("Triclopyr Tukey Results results")
print(TukeyHSD(tric_aov))
#print(model.tables(tric_aov, "mean"))
print("")
print("Glyphosate ANOVA results")
glyph_aov <- aov(necrosis ~ type + concentration, data = glyph_df)


print(summary(glyph_aov))

#Glyphosate Equivelence Testing
#TOSTs on Glyphosate for WT vs T at each concentration
# 0 ommitted due to NA 
par(mfrow=c(1, 3)) 
theta <- log(x = 1.25)

one_conc <- filter(glyph_df, concentration == 1)
print(TOSTtwo.raw(
  m1 = mean((filter(one_conc, type == "Wild type"))$necrosis),
  m2 = mean((filter(one_conc, type == "VD3"))$necrosis),
  sd1 = sd((filter(one_conc, type == "Wild type"))$necrosis), 
  sd2 = sd((filter(one_conc, type == "VD3"))$necrosis),
  n1 = 4, n2 = 4,
  low_eqbound = -theta,
  high_eqbound = theta,
  alpha = 0.05,
  var.equal = TRUE,
  plot = TRUE
))
thirty_conc <- filter(glyph_df, concentration == 33) 
print(TOSTtwo.raw(
  m1 = mean((filter(thirty_conc, type == "Wild type"))$necrosis),
  m2 = mean((filter(thirty_conc, type == "VD3"))$necrosis),
  sd1 = sd((filter(thirty_conc, type == "Wild type"))$necrosis), 
  sd2 = sd((filter(thirty_conc, type == "VD3"))$necrosis),
  n1 = 4, n2 = 3,
  low_eqbound = -theta,
  high_eqbound = theta,
  alpha = 0.05,
  var.equal = TRUE,
  plot = TRUE
))
hundred_conc <- filter(glyph_df, concentration == 100)
print(TOSTtwo.raw(
  m1 = mean((filter(hundred_conc, type == "Wild type"))$necrosis),
  m2 = mean((filter(hundred_conc, type == "VD3"))$necrosis),
  sd1 = sd((filter(hundred_conc, type == "Wild type"))$necrosis), 
  sd2 = sd((filter(hundred_conc, type == "VD3"))$necrosis),
  n1 = 4, n2 = 4,
  low_eqbound = -theta,
  high_eqbound = theta,
  alpha = 0.05,
  var.equal = TRUE,
  plot = TRUE
))






