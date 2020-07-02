source("lib.R")

data <- fzg_500

factor_columns <- c("land","getriebe_art","getriebe_gruppe","motor","kraftstoff","getriebewerk","motorwerk","temperaturen_winter","temperaturen_sommer","werk","fahrzeugmodell","regentrockenzeit","niederschlag_sommer","niederschlag_winter","leistung","winter","marke","mop")
numeric_columns <- c("Fahrstrecke", "einsatzdauer_years")


fahrstrecke_train <- data %>%
  filter(event == 1) %>%
  select(Fahrstrecke)

fahrstrecke_test <- data %>%
  filter(event == 0) %>%
  select(Fahrstrecke)


zeit_train <- data %>%
  filter(event == 1) %>%
  select(einsatzdauer_years)

zeit_test <- data %>%
  filter(event == 0) %>%
  select(einsatzdauer_years)

fahrstrecke <- rbind.data.frame(fahrstrecke_train,fahrstrecke_test)
zeit <- rbind.data.frame(zeit_train,zeit_test)


#####################################
#1.training Dataset -> Ausgefallene Autos "Jahresfahrstrecke_failed"
data_train <- data[data$event == 1,c(factor_columns,"prob")]

#2. test Datensatz -> nicht ausgefallene Autos "Jahresfahrstrecke_target"
data_test <- data[data$event == 0,c(factor_columns,"prob")]


#data_test <- sample_n(data_test, 30000)

#------------------------------

# data_train und data_test vereinen
data_all <- rbind.data.frame(data_train,data_test)

prob <- data_all$prob

#um später trainig und test wieder zu trennen 
rows_train <- nrow(data_train)
rows_test <- nrow(data_all)

sp <- ncol(data_all)-1

# one hot encoding nominal skalierter Merkmale
names_data <- flatten(sapply(data_all[,1:sp], function(x) sort(unique(x))))
length_data <- sapply(data_all[,1:sp], function(x) length(unique(x)))
factor_data <- lapply(data_all[,1:sp], function(x) as.integer(factor(x)))
cat_data <- lapply(factor_data, function(x) to_categorical(x))
data_NN <- as.data.frame(cat_data)

#erste spalte löschen, da stehen nur Nullen drin
remove_data_cols <- grep("\\.1$", names(data_NN), value=T)

#remove first column of a feature
data_NN <- data_NN %>%
  select(-c(remove_data_cols))

names(data_NN) <- names_data

#einsatzdauer, nominale Merkmale und Jahresfahrstrecke vereinen - so sieht der Datensatz aus, der in das Netz gegegben wird
data_NN <- cbind.data.frame(zeit, data_NN, prob)

#data_NN$Fahrstrecke <- scale(data_NN$Fahrstrecke, center = mean(data_NN$Fahrstrecke), scale = sd(data_NN$Fahrstrecke)) 
data_NN$einsatzdauer_years <- scale(data_NN$einsatzdauer_years, center = mean(data_NN$einsatzdauer_years), scale = sd(data_NN$einsatzdauer_years)) 


# nun werden die Spaltennamen entfernt und die Tabelle zur Matrix gemacht damit das Netz die Daten verarbeiten kann (der Schirtt davor diente an sich nur der Veranschaulichung, um zu verstehen, auf welche weise die Daten ins Netz gehen)
data_model <- as.matrix(data_NN)
dimnames(data_model) <- NULL

d <- ncol(data_model)-1

# data in training and test aufteilen
training <- data_model[1:rows_train,1:d] # training datensatz wird nicht mehr benötigt weil Netz schon trainiert. ist vollständigkeitshalber trotzdem aufgeführt
trainingtarget <- data_model[1:rows_train, d+1] 
test <- data_model[(rows_train+1):rows_test,1:d]
testtarget <- data_model[(rows_train+1):rows_test,d+1]

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
                  optimizer = optimizer_adam(lr = 0.00001),
                  metrics = 'mae')

earlystop <- callback_early_stopping(
  monitor = "val_loss",
  patience = 20)

#fit the model
model %>%
  fit(training,
      trainingtarget,
      epochs = 300,
      batch_size = 100,
      validation_split = 0.30,
      callbacks = list(earlystop),
      shuffle = TRUE,
      verbose = 1)


# Abweichung wird bestimmt
evaluation <- model %>% evaluate(test,testtarget)

# Vorhersage wird getroffen
prediction <- model %>% predict(test)


mse_to_accuracy(prediction = prediction, testtarget = testtarget)




