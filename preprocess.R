library(readxl)

# Read and preprocess raw files

preprocess_data <- function(readin_raw = FALSE, writeout = FALSE){
  if(readin_raw){
    # Birthdata
    raw_df <- read.csv("data/birthdata.csv", sep=";")
    
    # DESTATIS - Staatenliste
    states_df <- read_xlsx("data/Staatsangehoerigkeitsgebietsschluessel_xls.xlsx", 
                           sheet = '2. Staatsangehörigkeit', skip = 8, col_names = FALSE)
    colnames(states_df) <- c("name", "nationality", "bev_state", "x1","x2","x3","x4","x5", "iso_2", "iso_3","note")
    
    # German municipalities
    # https://www.statistikportal.de/de/veroeffentlichungen/anschriftenverzeichnis
    # Col L: Ort
    deu_cities_df <- read_xlsx("data/Anschriften_der_Gemeinde_und_Stadtverwaltungen_Stand_31012023_final.xlsx", 
                               sheet = 'Anschriften_31_01_2023', skip = 8, cell_cols(12), col_names = FALSE)
    # Col H: Gemeinde/Stadt
    deu_cities_df <- bind_rows(deu_cities_df,
                               read_xlsx("data/Anschriften_der_Gemeinde_und_Stadtverwaltungen_Stand_31012023_final.xlsx", sheet = 'Anschriften_31_01_2023', skip = 8, cell_cols(8), col_names = FALSE) )
    colnames(deu_cities_df) <- c("deu_city")
    
    # https://simplemaps.com/data/world-cities
    world_cities_df <- read_csv("data/worldcities.csv") 
    
  }
  if(writeout){
    external_data <- list(
      raw_df = raw_df,
      states_df = states_df,
      deu_cities_df = deu_cities_df,
      world_cities_df = world_cities_df
    )
    saveRDS(external_data, "external_data.rds")
  }
  list2env(readRDS("external_data.rds"), envir = .GlobalEnv)
}




