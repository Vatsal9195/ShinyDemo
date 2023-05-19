# ******Helper functions*******

"%ni%" <- Negate("%in%")

# ---- Structure Function ----
UD_structure <- function(x){
  
  data_A <- summary.default(x) %>% as.data.frame() %>%
    group_by(Var1) %>%
    pivot_wider(names_from = 'Var2', values_from = 'Freq') %>%
    ungroup() %>%
    mutate(Class = ifelse(Class == '-none-', Mode, Class)) %>%
    select(-Mode, -Length) %>%
    left_join(
      do.call(cbind, lapply(x, typeof)) %>% as.data.frame() %>%
        pivot_longer(
          cols = names(.),
          names_to = 'Var1',
          values_to = 'Type'
        ),
      by = 'Var1'
    ) %>% 
    left_join(
      do.call(cbind, lapply(x, n_distinct)) %>% as.data.frame() %>%
        pivot_longer(
          cols = names(.),
          names_to = 'Var1',
          values_to = 'Unique Values'
        ),
      by = 'Var1'
    ) %>% 
    rename('ColNames' = Var1)
  
  return(data_A)
  
}

# Memoise Function to Use Cached output
UD_structure_m <- memoise::memoise(UD_structure)

# ----- Import modules -----
importShinyModules <- function(path = "./modules/") {
  
  moduleFiles <- list.files(path = path, pattern = "\\.[Rr]", recursive = TRUE)
  
  for(i in 1:length(moduleFiles)) {
    moduleFilePath <- paste0(path, moduleFiles[i])
    source(moduleFilePath, encoding="UTF-8")
  }
  
}




