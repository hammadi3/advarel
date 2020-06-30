source("lib.R")
##########################################################################################################################
# beste Merkmalskombination: Auslieferungsland, Getriebe, Motor, Kraftstoff, Fahrzeugmodell, Einsatzdauer
# Auswahl relevanter Merkmale

Fzg_final_with_target <- read_csv("Fzg_final_with_target.csv")

spalten <- c("land","getriebe","motor","kraftstoff","fahrzeugmodell")

zeit <- function(y){
  y %>%
    select(jfs)
}

zeit_train <- zeit(Fzg_final_with_target[Fzg_final_with_target$event==1,])
zeit_test <- zeit(Fzg_final_with_target[Fzg_final_with_target$event==0,])

zeit <- rbind.data.frame(zeit_train,zeit_test)
# maximales repairdate ist der Betrachtungszeitpunkt


#####################################
#1.training Dataset -> Ausgefallene Autos "Jahresfahrstrecke_failed"
data_train <- Fzg_final_with_target %>%
  filter(event == 1) %>%
  select(spalten,"prob")

#2. test Datensatz -> nicht ausgefallene Autos "Jahresfahrstrecke_target"
data_test <- Fzg_final_with_target %>%
  filter(event == 0) %>%
  select(spalten,"prob")

#data_test <- sample_n(data_test, 30000)

#------------------------------

# data_train und data_test vereinen
data <- rbind.data.frame(data_train,data_test)

prob <- data$prob

#um später trainig und test wieder zu trennen 
rows_train <- nrow(data_train)
rows_test <- nrow(data)

sp <- ncol(data)-1

# one hot encoding nominal skalierter Merkmale
names_data <- flatten(sapply(data[,1:sp], function(x) sort(unique(x))))
length_data <- sapply(data[,1:sp], function(x) length(unique(x)))
factor_data <- lapply(data[,1:sp], function(x) as.integer(factor(x)))
cat_data <- lapply(factor_data, function(x) to_categorical(x))
data_NN <- as.data.frame(cat_data)

#erste spalte löschen, da stehen nur Nullen drin
remove_data_cols <- grep("\\.1$", names(data_NN), value=T)

#remove first column of a feature
data_NN <- data_NN %>%
  select(-c(remove_data_cols))

names(data_NN) <- names_data

#einsatzdauer, nominale Merkmale und Jahresfahrstrecke vereinen - so sieht der Datensatz aus, der in das Netz gegegben wird
data_NN <- cbind.data.frame(zeit,data_NN,prob)

# nun werden die Spaltennamen entfernt und die Tabelle zur Matrix gemacht damit das Netz die Daten verarbeiten kann (der Schirtt davor diente an sich nur der Veranschaulichung, um zu verstehen, auf welche weise die Daten ins Netz gehen)
data_model <- as.matrix(data_NN)
dimnames(data_model) <- NULL

#einsatzdauer normalisieren
data_model[,1] <- normalize(data_model[,1])

d <- ncol(data_model)-1

# data in training and test aufteilen
training <- data_model[1:rows_train,1:d] # training datensatz wird nicht mehr benötigt weil Netz schon trainiert. ist vollständigkeitshalber trotzdem aufgeführt
trainingtarget <- data_model[1:rows_train, d+1] 
test <- data_model[(rows_train+1):(rows_train + 30000),1:d]
testtarget <- data_model[(rows_train+1):(rows_train + 30000),d+1]

# trainiertes Netz laden

model <- keras_model_sequential()

#Modell
model %>%
  layer_dense(units = 200, activation = 'relu',input_shape = ncol(training)) %>% 
  layer_dense(units = 200, activation = 'relu') %>%
  layer_dense(units = 200, activation = 'relu') %>%
  layer_dense(units = 1)

#compile
model %>% compile(loss = 'mse',
                  optimizer = optimizer_adam(lr = 0.0001),
                  metrics = 'mae')

earlystop <- callback_early_stopping(
  monitor = "val_loss",
  patience = 20)

#fit the model
model %>%
  fit(training,
      trainingtarget,
      epochs = 5,
      batch_size = 50,
      validation_split = 0.30,
      callbacks = list(earlystop),
      shuffle = TRUE,
      verbose = 1)




# Abweichung wird bestimmt
model %>% evaluate(test,testtarget)

# Vorhersage wird getroffen
model %>% predict(test)

# geschätzte Jahresfahrstrecke zum Datensatz hinzufügen 
Fzg_predicted_NN <- cbind.data.frame(not_Asf,round(pred))
names(Fzg_predicted_NN)[names(Fzg_predicted_NN) == "round(pred)"] <- "jahresfahrstrecke_predicted"

Fzg_predicted_NN <- Fzg_predicted_NN %>%
  mutate(diff = jahresfahrstrecke_target-jahresfahrstrecke_predicted) %>%
  mutate(mse = (jahresfahrstrecke_target-jahresfahrstrecke_predicted)^2) 

# Abweichung
mse_NN <- evalmodel$loss
rmse_NN <- sqrt(mse_NN)

###########################################################################################################################################
# für die App

#um Datensätze zu selecten in tab 1, 2, 3
merkmale1 <- c("vin","kdnr","schadensart","repair_date","zulassungsdatum","repair_country","mileage","jahresfahrstrecke_failed")
merkmale2 <- c("vin","kraftstoff","fahrzeugmodell","auslieferungsland","jahresfahrstrecke_target","jahresfahrstrecke_predicted","diff","mse")
merkmale3 <- c("vin","kdnr","schadensart","kraftstoff","fahrzeugmodell","auslieferungsland","repair_date","zulassungsdatum","repair_country","mileage","jahresfahrstrecke_failed")
merkmale4 <- c("vin","kraftstoff","fahrzeugmodell","auslieferungsland","jahresfahrstrecke_target","jahresfahrstrecke_predicted","diff","mse")

Merkmale1 <- c("VIN","Kdnr.","Schadensart","Reparaturdatum","Zulassungsdatum","Reparaturland","Kilometerstand","Jahresfahrstrecke")
Merkmale2 <- c("VIN","Kraftstoff","Fahrzeugmodell","Auslieferungsland","Jahresfahrstrecke_target","Jahresfahrstrecke_predicted","Diff","MSE")
Merkmale3 <- c("VIN","Kdnr.","Schadensart","Kraftstoff","Fahrzeugmodell","Auslieferungsland","Reparaturdatum","Zulassungsdatum","Reparaturland","Kilometerstand","Jahresfahrstrecke_failed")
Merkmale4 <- c("VIN","Kraftstoff","Fahrzeugmodell","Auslieferungsland","Jahresfahrstrecke_target","Jahresfahrstrecke_predicted","Diff","MSE")

vec <- c("kraftstoff","fahrzeugmodell","auslieferungsland")
