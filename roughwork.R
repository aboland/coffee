
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

brewmaster_raw <- readr::read_csv("data/brewmaster2_2020-08-11.csv")
View(brewmaster_raw)


my_brew_data_time <- function(x, 
                              split = ";", 
                              pad_length = NULL, 
                              beginning_time = 8, 
                              beginning_tol = 0.5,
                              stall_tol = 0.01,
                              change_lag = 2){
  
  split_data <- as.numeric(unlist(strsplit(x, split =  split)))
  
  # Want to remove any spikes at the beginning of the pour
  beginning_vals <- 1:round(beginning_time/0.2)
  beginning_outliers <- which(abs(split_data[beginning_vals]) >= beginning_tol)
  if (length(beginning_outliers) > 0)
    split_data[beginning_vals][beginning_outliers] <- 0
  
  # browser()
  
  lag_n <- diff(split_data, lag = change_lag)
  first_increase <- min(which(lag_n > beginning_tol))
  
  if (sum(which(lag_n < stall_tol) > first_increase) > 0) {
    # last_stall <- min(which(lag2 < 0.01))[which(which(lag2 < 0.01) > first_increase)]
    last_stall <- min(which(lag_n < stall_tol)[which(which(lag_n < stall_tol) > first_increase)])
    split_data[(last_stall):length(split_data)] <- NA
  }
  
  # if (last_stall > first_increase) {
  #   browser()
  #   split_data[(last_stall + 1):length(split_data)] <- NA
  # }
    
  
  # if (sum(abs(split_data) > beginning_tol) > 0)
  #   split_data[1:round(beginning_time/0.2)] <- 0
  # zoo::na.locf()
  
  if (!is.null(pad_length))
    split_data <- c(split_data, rep(NA, pad_length - length(split_data)))
  
  
  
  names(split_data) <- paste("t",1:length(split_data), sep = "_")
  return(split_data)
}

my_brew_data_series <- function(.data, col_name, ...){
  col_name <- as_label(enquo(col_name))
  .data %>%
    bind_cols(do.call(bind_rows, lapply(.data[[col_name]], my_brew_data_time, ...))) %>%
    select(-col_name)
}

my_brew_data_json <- function(.data, col_name, decaf = FALSE){
  col_name <- as_label(enquo(col_name))
  .data %>%
    bind_cols(lapply(.data[[col_name]], function(x) jsonlite::fromJSON(gsub("”", '"', x))) %>%
                bind_rows()) %>%
    select(-col_name)
}


# brewmaster_raw %>% 
#   my_brew_data_json(note)
# 
# brewmaster_raw %>%
#   mutate(js = purrr::map(note, function(x) jsonlite::fromJSON(gsub("”", '"', x)))) %>%
#   tidyr::unnest(js) %>%
#   select(-note)

# maxL <- max(sapply(strsplit(brewmaster_raw$`brew data`, split =  ";"), length))

coffee_tidy <- 
  brewmaster_raw %>% 
  separate(name, sep = ",", into = c("type", "roastery", "origin")) %>%
  my_brew_data_json(note) %>%
  mutate(decaf = case_when(is.na(decaf) ~ F, T ~ T)) %>%
  my_brew_data_series(`brew data`, 
                      stall_tol = 0.005,
                      change_lag = 2) %>%
  # bind_cols(do.call(bind_rows, lapply(brewmaster_raw$`brew data`, my_brew_data_series)))
  pivot_longer(cols = starts_with("t_"), #paste("t", 1:maxL, sep = "_")
               names_to = "time", values_to = "weight") %>%
  mutate(time = as.numeric(gsub("t_", "", time)) * 0.2,
         weight = as.numeric(weight),
         createdAt = as_date(createdAt), roast_date = as_date(roast_date),
         age = createdAt - roast_date, ratio = `total weight`/dose) %>%
  select(id, createdAt, type, roastery, roast_date, origin, age, grind, dose,  
         total_time = `total time`, total_weight = `total weight`,
         ratio, time, weight, decaf, mean_flow = `average flowrate`)


coffee_tidy %>%
  ggplot(aes(x = time, y = weight, group = id, colour = grind)) +
  geom_line() +
  scale_color_continuous(type = "viridis") +
  scale_x_continuous(labels = function(x)paste0(x,"s")) +
  scale_y_continuous(labels = function(x)paste0(x, "g"))

coffee_tidy %>%
  filter(id == "i0KxIZ17LV") %>%
  ggplot(aes(x = time, y = weight)) +
  geom_line()




coffee_tidy %>%
  group_by(id) %>%
  summarise(max_entry = which.max(is.na(weight)),
            grind = max(grind),
            origin = unique(origin),
            roastery = unique(roastery)) %>%
  mutate(final_time = max_entry * 0.2) %>%
  ggplot(aes(grind, final_time, colour = roastery)) +
  geom_point()
