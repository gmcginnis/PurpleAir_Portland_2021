
## Yee haw it's wrangling time

#install.packages("tidyverse")
library(tidyverse)
library(lubridate)

# writeLines("x,y\n1,a", "zagat_food.csv")
# writeLines("x,y\n2,b", "zagat_drink.csv")
# writeLines("x,y\n3,c", "michelin_food.csv")
#
# (my_files <- list.files(pattern = "^.*food.*"))
# my_files %>%
#   set_names() %>%
#   map_dfr(read_csv, .id = "source")

## ([0-9])\w+\.([0-9])\w+\s\-([0-9])\w+\.([0-9])\w+ #some RegEx that nabs coordinates because i hate myself

#gets list of files which are outside sensors only
my_files <- list.files(path = "blob/", pattern = ".+\\(*(outside*)\\w+\\)")

#
## begin stuff-that-should-be-fixed-but-works-for-now
initial_wd <- "/Users/Gillian/Desktop/Photos & Misc/Programming/R Projects/purpleair_drafting"
wd <- getwd() #grabs current working directory
wd_temp <- "/Users/Gillian/Desktop/Photos & Misc/Programming/R Projects/purpleair_drafting/blob"
setwd(wd_temp) #sets new wd

all <- my_files %>%
  set_names() %>%
  map_dfr(read_csv, .id = "source")

setwd(initial_wd) #sets back to old wd
## end stuff-that-should-be-fixed-but-works-for-now
#

all_mod <- all %>%
  mutate(created_at = as_datetime(created_at), #converting from character to date time format; tz is in UTC
         date = date(created_at), #picking out just the date
         #time = strftime(created_at, format="%H:%M:%S"), # old way of picking time; outputs char
         time = hms::as.hms(created_at), #picking out just the time
         time_est = time) %>% #currently working on rounding out the seconds but for now it stands as is
  mutate(coords = str_extract(source, "([0-9])\\w+\\.([0-9])\\w+\\s\\-([0-9])\\w+\\.([0-9])\\w+"), #grabbing coord data
         lat = str_extract(coords, "\\d+\\.\\d+\\s"), #latitude (north)
         lat = str_trim(lat, side = "right"), #removing extra whitespace
         lat = as.numeric(lat), #converting to numeric from character
         long = str_extract(coords, "\\-\\d+\\.\\d+"), #longitude (negative here, so west)
         long = as.numeric(long), #converting to numeric from character
         loc_tag = str_extract(source, "^[^\\(]+"), #grabbing location tag
         loc_tag = str_trim(loc_tag, side = "right")) #removing extra whitespace

# time_test <- c("01:05:25", "16:07:50")
# time_hms <- hms::as.hms(time_test)
# #round.POSIXt(hms::as.hms(time_test), units = "mins")
# hms::round_hms(time_hms, "minute")

unique_locations <- unique(all_mod$loc_tag) #creates list of unique locations for quick reference

# test_df <- all_mod %>%
#   group_by(loc_tag) %>%
#   drop_na(`PM2.5_CF1_ug/m3`) %>%
#   summarize(mean_pm25 = mean(`PM2.5_CF1_ug/m3`))

date_df <- all_mod %>%
  drop_na(`PM2.5_CF1_ug/m3`) %>%
  group_by(loc_tag, date) %>%
  summarize(mean_pm25 = mean(`PM2.5_CF1_ug/m3`))

ggplot(date_df, aes(x = date, y = mean_pm25, color = loc_tag))+
  geom_line()

time_df_new <- all_mod %>%
  drop_na(`PM2.5_CF1_ug/m3`) %>%
  group_by(loc_tag, time_est) %>%
  summarize(mean_pm25 = mean(`PM2.5_CF1_ug/m3`))

ggplot(time_df_new, aes(x = time_est, y = mean_pm25, color = loc_tag))+
  #geom_line()+ #overplotting galore
  geom_smooth()+
  scale_x_time()

# time_df <- all_mod %>%
#   drop_na(`PM2.5_CF1_ug/m3`) %>%
#   #mutate(time_est = as.POSIXct(round(as.POSIXct(time, format="%H:%M:%S", tz="UTC"), units = "mins"))) %>%
#   mutate(time_est = hms::as.hms(as.POSIXct(round(created_at), units = "mins"))) %>%
#   group_by(loc_tag, time_est) %>%
#   summarize(mean_pm25 = mean(`PM2.5_CF1_ug/m3`))
#
# ggplot(time_df, aes(x = time_est, y = mean_pm25, color = loc_tag))+
#   geom_line(alpha=0.5)+
#   #geom_smooth()+
#   scale_x_time()
