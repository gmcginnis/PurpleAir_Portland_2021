
library(tidyverse)
library(lubridate)

# my_files <- list.files(path = "drafting/blob2/", pattern = ".+\\(*(outside*)\\w+\\)")
# 
# #
# ## begin stuff-that-should-be-fixed-but-works-for-now
# initial_wd <- "/Users/Gillian/Desktop/Photos & Misc/Programming/R Projects/purpleair_drafting"
# wd <- getwd() #grabs current working directory
# wd_temp <- "/Users/Gillian/Desktop/Photos & Misc/Programming/R Projects/purpleair_drafting/blob2"
# setwd(wd_temp) #sets new wd
# 
# all <- my_files %>%
#   set_names() %>%
#   map_dfr(read_csv, .id = "source")
# 
# setwd(initial_wd) #sets back to old wd
# ## end stuff-that-should-be-fixed-but-works-for-now
# #

my_file_paths <- list.files(path = "drafting/blob2", pattern = ".+\\(*(outside*)\\w+\\)", full.names = TRUE)

all <- my_file_paths %>%
  set_names() %>%
  #map_dfr(.x = my_files, read_csv, .id = "source") #wont work if files not in wd
  map_dfr(read_csv, .id = "source") %>%
  mutate(source = str_replace(source, ".\\w+\\/\\w+\\/", "")) #removes filepath name

all_mod <- all %>%
  mutate(created_at = as_datetime(created_at), #converting from character to date time format; tz is in UTC
         date = date(created_at), #picking out just the date
         #time = strftime(created_at, format="%H:%M:%S"), # old way of picking time; outputs char
         time = hms::as_hms(created_at), #picking out just the time
         time_est = time) %>% #currently working on rounding out the seconds but for now it stands as is
  mutate(coords = str_extract(source, "([0-9])\\w+\\.([0-9])\\w+\\s\\-([0-9])\\w+\\.([0-9])\\w+"), #grabbing coord data
         lat = str_extract(coords, "\\d+\\.\\d+\\s"), #latitude (north)
         lat = str_trim(lat, side = "right"), #removing extra whitespace
         lat = as.numeric(lat), #converting to numeric from character
         long = str_extract(coords, "\\-\\d+\\.\\d+"), #longitude (negative here, so west)
         long = as.numeric(long), #converting to numeric from character
         loc_tag = str_extract(source, "^[^\\(]+"), #grabbing location tag
         loc_tag = str_trim(loc_tag, side = "right")) #removing extra whitespace


unique_locations <- unique(all_mod$loc_tag) #creates list of unique locations for quick reference

time_df_new <- all_mod %>%
  drop_na(`PM2.5_CF1_ug/m3`) %>%
  group_by(loc_tag, time_est) %>%
  summarize(mean_pm25 = mean(`PM2.5_CF1_ug/m3`))

ggplot(time_df_new, aes(x = time_est, y = mean_pm25, color = loc_tag))+
  geom_line()+
  scale_x_time()


