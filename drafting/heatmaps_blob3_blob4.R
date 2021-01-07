library(viridis)
library(tidyverse)
library(lubridate)

# #my_files <- list.files(path = "blob3/", pattern = ".+\\(*(outside*)\\w+\\)")
# my_files <- list.files(path = "drafting/blob4/", pattern = ".+\\(*(outside*)\\w+\\)")
# 
# #
# ## begin stuff-that-should-be-fixed-but-works-for-now
# initial_wd <- "/Users/Gillian/Desktop/Photos & Misc/Programming/R Projects/purpleair_drafting"
# wd <- getwd() #grabs current working directory
# #wd_temp <- "/Users/Gillian/Desktop/Photos & Misc/Programming/R Projects/purpleair_drafting/blob3"
# wd_temp <- "/Users/Gillian/Desktop/Photos & Misc/Programming/R Projects/purpleair_drafting/blob4"
# setwd(wd_temp) #sets new wd
# 
# all <- my_files %>%
#   set_names() %>%
#   map_dfr(read_csv, .id = "source")
# 
# setwd(initial_wd) #sets back to old wd
# ## end stuff-that-should-be-fixed-but-works-for-now
# #

my_file_paths <- list.files(path = "drafting/blob4", pattern = ".+\\(*(outside*)\\w+\\)", full.names = TRUE)

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


# Source: https://www.r-graph-gallery.com/283-the-hourly-heatmap.html

df <- all_mod %>%
  drop_na(`PM2.5_CF1_ug/m3`) %>%
  mutate(hour = hour(created_at),
         year = year(created_at),
         month = month(created_at, label=TRUE),
         day = day(created_at))

ggplot(data=subset(df, month%in%c("Oct", "Nov", "Dec")),aes(day,hour,fill=`PM2.5_CF1_ug/m3`))+
#ggplot(data=subset(df, month=="Sep"),aes(day,hour,fill=`PM2.5_CF1_ug/m3`))+
#ggplot(df,aes(day,hour,fill=`PM2.5_CF1_ug/m3`))+
  geom_tile(color = "white", size=0.1)+
  #scale_fill_viridis_c(name = "Hrly PM25")+
  scale_fill_viridis(name = "Hrly PM25", option = "plasma")+
  facet_grid(loc_tag~month)+
  scale_y_continuous(trans = "reverse", breaks = unique(df$hour))+
  scale_x_continuous(breaks = c(1,10,20,31))+
  theme_minimal(base_size=8)+
  labs(title = paste("Hourly PM25"), x= "Day", y = "Hour commencing")+
  theme(legend.position="bottom",
        plot.title=element_text(size=14, hjust = 0),
        axis.text.y=element_text(size=6),
        strip.background = element_rect(colour="white"),
        axis.ticks=element_blank(),
        axis.text=element_text(size=7),
        legend.title=element_text(size=8),
        legend.text=element_text(size=6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
