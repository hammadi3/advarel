####
source("lib.R")
source("helpers.R")

################################################################################################################################################
# Import and clean all vehicules produced in 2014:
Fzg_raw <- read_csv2("../Daten/deeplearning_test_set_v1_fzg.csv")
Fzg_raw <- Fzg_raw[,-1] ## remove column "X", only enumeration 

# convert columns with date to dates
date_columns <- c("bauteildatum","produktionszeitpunkt","zulassungsdatum","motordatum","getriebedatum")
Fzg_raw[,date_columns] <- lapply(Fzg_raw[,date_columns],function(x) as.Date(x,format = "%Y-%m-%d"))
Fzg_raw$bauteil_mop <- as.yearmon(as.character(Fzg_raw$bauteil_mop), format = "%Y%m") #with zoo package
Fzg_raw$mop <- as.yearmon(as.character(Fzg_raw$mop), format = "%Y%m")

## unwichtigste Spalten aus Fzg Datensatz filtern
## Duplikate entfernen und wichtige Spalten umbenennen
Fzg <- Fzg_raw %>%
  select(-c("auslieferungsland","bauteilzugehoerigkeit")) %>% #these columns are not needed
  group_by(vin,produktionszeitpunkt,zulassungsdatum) %>% #group by and take first vin
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(prod_zul_diff = as.numeric(difftime(zulassungsdatum, produktionszeitpunkt, units = c("days"))), #time difference between zulassung and production
         winter = ifelse(winter == "ja", 1, 0),
         regentrockenzeit = ifelse(regentrockenzeit == "ja", 1, 0))

plot_ly(x = Fzg$jahresfahrstrecke, type = "histogram")
plot_ly(x = Fzg$jahresfahrstrecke[Fzg$jahresfahrstrecke < 250000], type = "histogram")

plot_ly(x = Fzg$prod_zul_diff, type = "histogram")
plot_ly(x = Fzg$prod_zul_diff[Fzg$prod_zul_diff < 350], type = "histogram")

lim_jahresfahrstrecke <- 250000  #max 250k km each year (taxis and trucks)
lim_prod_zul_diff <- 350 

Fzg <- Fzg %>% filter(jahresfahrstrecke < lim_jahresfahrstrecke, prod_zul_diff < lim_prod_zul_diff)

rm(Fzg_raw)
# Die Fahrzeuge wurden im Jahr 2014 produziert

################################################################################################################################################

# Import and clean failed vehicules
Asf_raw <- read_csv2("../Daten/deeplearning_test_set_v1_asf.csv")
Asf_raw <- Asf_raw[-1,-1] #X-Spalte entfernen, da diese nur als Zeilenummerierung Verwendung findet & Entfernen der ersten Zeile da zweite Überschrift

Asf_raw$vin <- as.integer(Asf_raw$vin)
Asf_raw$mileage <- as.integer(Asf_raw$mileage)

necessary_columns <- c("vin","kdnr", "kdnr_hauptgruppe", "schadensart", "schadensart_gruppierung", "repair_date","warranty_load_date", "land", "etap", "klimazone", "klimatyp", "klimauntertyp", "mileage")
date_columns <- c("repair_date", "warranty_load_date")
factor_columns <- c("kdnr_hauptgruppe", "schadensart", "schadensart_gruppierung", "etap", "klimazone", "klimatyp", "klimauntertyp")

Asf_raw[,date_columns] <- lapply(Asf_raw[,date_columns],function(x) as.Date(x,format = "%Y-%m-%d"))
Asf_raw[,factor_columns] <- lapply(Asf_raw[,factor_columns],function(x) as.factor(x))

Asf <- Asf_raw %>% 
  select(all_of(necessary_columns)) %>%
  rename_at("land",~"repair_country") %>%
  # wenn vin gleich, dann nehme die kleinere mileage
  group_by(vin) %>% 
  slice(which.min(repair_date)) %>% # so wird nur die erste reparatur genommen
  ungroup() %>% 
  mutate(rep_warr_diff = as.numeric(difftime(warranty_load_date, repair_date, units = c("days")))) 

plot_ly(x = Asf$mileage, type = "histogram")
plot_ly(x = Asf$mileage[Asf$mileage < 500000], type = "histogram")

plot_ly(x = Asf$rep_warr_diff, type = "histogram")
plot_ly(x = Asf$rep_warr_diff[Asf$rep_warr_diff < 25], type = "histogram")

lim_mileage <- 500000   #max 500k until first failure
lim_rep_warr_diff <- 25 #max 25 days between repair date and warranty load date

Asf <- Asf %>% filter(mileage < lim_mileage, rep_warr_diff < lim_rep_warr_diff)

################################################################################################################################################
# Add failure information to Fzg
Fzg_complete <- merge(x = Fzg, y = Asf, by = "vin", all.x = T)

# Add column Event (1 for failed, 0 for didn't fail)

betrachtungsdatum <- max(Asf$repair_date)
Fzg_complete <- Fzg_complete %>% mutate(event = ifelse(is.na(mileage), 0, 1)) %>%
  mutate(einsatzdauer_days = ifelse(event == 1,
                                    as.numeric(difftime(repair_date,zulassungsdatum, units = "days")),
                                    as.numeric(difftime(betrachtungsdatum,zulassungsdatum, units = "days")))) %>% 
  mutate(einsatzdauer_years = einsatzdauer_days / 365) %>%
  mutate(Fahrstrecke = jahresfahrstrecke * einsatzdauer_years)

plot_ly(x = Fzg_complete[Fzg_complete$event==1,]$einsatzdauer_days, type = "histogram")

# Add target column using johnson method

probs_johnson <- johnson(time = Fzg_complete$Fahrstrecke, event = Fzg_complete$event) %>% 
  select(time, prob) %>% rename_at("time",~"Fahrstrecke")

probs_kaplan <- kaplan_meier(time = Fzg_complete$Fahrstrecke, event = Fzg_complete$event) %>% 
  select(time, prob) %>% rename_at("time",~"Fahrstrecke")

probs_nelson <- nelson(time = Fzg_complete$Fahrstrecke, event = Fzg_complete$event) %>% 
  select(time, prob) %>% rename_at("time",~"Fahrstrecke")

plot_ly(data = probs_johnson[probs_johnson$Fahrstrecke <= 500000,], x = ~Fahrstrecke, y = ~prob , type = 'scatter', mode = 'lines')
plot_ly(data = probs_kaplan[probs_kaplan$Fahrstrecke <= 500000,], x = ~Fahrstrecke, y = ~prob , type = 'scatter', mode = 'lines')
plot_ly(data = probs_nelson[probs_nelson$Fahrstrecke <= 500000,], x = ~Fahrstrecke, y = ~prob , type = 'scatter', mode = 'lines')

probs_plots <- stack_lines_probs(x = probs_johnson[probs_johnson$Fahrstrecke <= 500000,]$Fahrstrecke,
                           y1 = probs_johnson[probs_johnson$Fahrstrecke <= 500000,]$prob,
                           y2 = probs_kaplan[probs_kaplan$Fahrstrecke <= 500000,]$prob,
                           y3 = probs_nelson[probs_nelson$Fahrstrecke <= 500000,]$prob)

Fzg_final <- merge(x = Fzg_complete, y = probs_kaplan, by = "Fahrstrecke")

plot_ly(x = Fzg_final$prob, type = "histogram")

fzg_500 <- filter(Fzg_final , Fahrstrecke <= 500000)
plot_ly(data = fzg_500, x = ~Fahrstrecke, y = ~prob , type = 'scatter', mode = 'lines')
plot_ly(x = fzg_500$prob, type = "histogram")
plot_ly(x = fzg_500[fzg_500$event==1,]$Fahrstrecke, type = "histogram")

write_csv(fzg_500, "~/Daten/Fzg_final.csv")

