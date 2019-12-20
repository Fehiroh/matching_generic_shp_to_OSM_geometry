


library(tidyverse)
library(sf)
library(tmap)

start_dir <- "C:/Users/afehir/Desktop/shapefiles/SingleDriver-checks/"

clipped_osm <- st_read(paste0(start_dir, "clipped.gdb"))
client_shp <- st_read(paste0(start_dir, "mississauga_with_ramp/Mississauga_Routes_With401.shp"))


client_shp$client_length <- as.numeric(st_length(client_shp))

length_of_reccnos <- client_shp %>%
  select(RRECNO, client_length)



# likely functional class per routed-segment in client shapefile ###################################

# 3 metre buffer
cl_shp_buf <- st_buffer(client_shp, 0.0003)

further_clipped <-  st_intersection(clipped_osm, cl_shp_buf)
further_clipped$length <- as.numeric(st_length(further_clipped))

potential_matched_byoid <- as.data.frame(further_clipped) %>% 
  select(RRECNO, osm_id, fclass, length)

# finding the proportion of linelength within the bufffer by fclass
figuring_out_fclass <- as.data.frame(further_clipped) %>% 
  select(RRECNO, osm_id, fclass, length) %>% 
  group_by(RRECNO, fclass) %>% 
  summarize(len_by_fclass = sum(length)) %>%
  mutate(frequency_of_fclass = round(len_by_fclass / sum(len_by_fclass) *100, 0)) %>%
  arrange(RRECNO, desc(frequency_of_fclass)) %>% 
  ungroup()


# most likely fclass 

most_likely_fclass <- figuring_out_fclass %>%
  group_by(RRECNO) %>% 
  mutate(max_len = max(len_by_fclass)) %>% 
  filter(len_by_fclass == max_len) %>% 
  select(-max_len) %>%
  ungroup()


# here we separate the RRECNOs with an fclass frequency of greater than a 85%

probably_the_right_fclass <- most_likely_fclass %>% 
  filter(frequency_of_fclass >= 85) 

prb_right_fclass_nms <- names(probably_the_right_fclass)


# returning to those with less than 85%

debatable_fclass <- figuring_out_fclass %>% 
  anti_join(probably_the_right_fclass, by = "RRECNO") 

ratios_to_highest_class <- debatable_fclass %>% 
  group_by(RRECNO) %>% 
  mutate(max_freq_ratio = frequency_of_fclass / max(frequency_of_fclass) * 100) %>% 
  filter(max_freq_ratio >= 50) %>% 
  mutate(num = n())

# Let's grab the lines where there is only one high value frequency, and add
# them to the most likely fclass

only_high_ratio <- ratios_to_highest_class %>% 
  filter(num == 1)

probably_the_right_fclass <- only_high_ratio %>% 
  select(one_of(prb_right_fclass_nms)) %>% 
  bind_rows(probably_the_right_fclass) %>% 
  arrange(RRECNO)


# and grab trhe ones with more than one potential fclass
multiple_ratio <- ratios_to_highest_class %>% 
  filter(num > 1) %>% 
  select(RRECNO, fclass, len_by_fclass, frequency_of_fclass) %>% 
  left_join(length_of_reccnos, by = "RRECNO") %>% 
  mutate(length_ratio =  abs((len_by_fclass / client_length * 100) - 100)) %>% 
  filter(length_ratio <= 5) %>% 
  group_by(RRECNO) %>% 
  mutate(num = n())

# quick check to make sure that this will work
too_close <- multiple_ratio %>% 
  filter(num > 1)

if (isTRUE(as.numeric(dim(too_close)[1]))== 0){
  errorCondition("You have to deal with the too_close")
}


# update the list of probably correct
probably_the_right_fclass <- multiple_ratio %>% 
  select(one_of(prb_right_fclass_nms)) %>% 
  bind_rows(probably_the_right_fclass) %>% 
  arrange(RRECNO)

# check for any missing RRECNOS

missing_rcno <- length_of_reccnos %>% 
  anti_join(probably_the_right_fclass, by = "RRECNO")

list_of_missing_recnos <- missing_rcno %>% 
  select(RRECNO)

# getting the ratio of osm_ids to client lines
remainder_line_by_line <- potential_matched_byoid %>% 
  right_join(missing_rcno, by = "RRECNO") %>% 
  mutate(indv_length_ratio =  abs((length / client_length * 100) - 100)) %>% 
  group_by(RRECNO) %>% 
  filter(indv_length_ratio == min(indv_length_ratio))


probably_the_right_fclass <- probably_the_right_fclass %>% 
  # select(-frequency_of_fclass)%>% 
  bind_rows(remainder_line_by_line) %>% 
  arrange(RRECNO) %>% 
  select(RRECNO, fclass)



# most likely fclass should now be complete 

most_likely_oids <- probably_the_right_fclass %>% 
  left_join(potential_matched_byoid, by = c("RRECNO", 'fclass')) %>% 
  left_join(length_of_reccnos, by = "RRECNO") %>% 
  select(RRECNO, fclass, osm_id, length, client_length) 


cl_most_likely_oids <- most_likely_oids%>% 
  group_by(RRECNO, fclass) %>% 
  mutate(osm_ids = str_c(osm_id, collapse = ", ")) %>% 
  select(-osm_id) %>% 
  distinct(RRECNO, fclass, osm_ids)


attempt_to_see_ml_oids <- further_clipped %>%
  semi_join(most_likely_oids, by= c('RRECNO', 'osm_id'))

plot(st_geometry(attempt_to_see_ml_oids))


# Saving Results ###################################################################

new_file_nm <- "C:/Users/afehir/Desktop/shapefiles/SingleDriver-checks/results/try_4/try_4.shp"

st_write(attempt_to_see_ml_oids,  normalizePath(new_file_nm), 
            delete_layer = TRUE)




