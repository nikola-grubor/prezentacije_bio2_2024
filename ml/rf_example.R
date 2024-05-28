################################
# Primer mašinskog učenja za BIO2
################################

# Instalacija i učitavanje paketa
install.packages("randomForest")
install.packages("palmerpenguins")
install.packages("ggplot2")
install.packages("caret")

# Učitaj podatke za primer
library(palmerpenguins)
library(randomForest)
library(ggplot2)
library(caret)

# Izgled podataka
head(penguins)

# Izbaciti podatke koji nedostaju
penguins <- penguins |> na.omit()

################
# Vizualizacija
################

# Masa pingvina vs. duzina peraja
mass_flipper <- ggplot(data = penguins, 
                       aes(x = flipper_length_mm,
                           y = body_mass_g)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 3,
             alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Penguin size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.2, 0.7),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")

mass_flipper


# Duzina peraja vs. duzina kljuna
flipper_bill <- ggplot(data = penguins,
                       aes(x = flipper_length_mm,
                           y = bill_length_mm)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 3,
             alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Flipper and bill length",
       subtitle = "Dimensions for Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
       x = "Flipper length (mm)",
       y = "Bill length (mm)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.85, 0.15),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")

flipper_bill

############################
# Random Forest klasifikacija
############################

# 75/25 podela na trenting i testing
penguins$ind <- sample(2, size = nrow(penguins), 
                       prob = c(0.75, 0.25), 
                       replace = TRUE)
# razdvajanje po indikatoru
train <- penguins[penguins$ind == 1, ]
test <- penguins[penguins$ind == 2, ]


# RandomForest algoritam
rf_res <- randomForest(species ~ . -island -ind, data = train)

# Print rezultata
print(rf_res) 
plot(rf_res)

# Importance of each predictor.
varImpPlot(rf_res)


#######################
# Evaluacija predikcija
#######################

# Predikcije na trening setu
pred_train <- predict(rf_res, train)
confusionMatrix(pred_train, train$species)

# Predikcije na test setu
pred_test <- predict(rf_res, test)
confusionMatrix(pred_test, test$species)
