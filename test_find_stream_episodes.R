## test find_stream_episodes

library(sensoRplot)
library(tidyverse)
library(timetk)
library(lubridate)

data <- read_csv(file="data-raw/PIRE_Group_1.csv") %>%
  mutate(
    Time = with_tz(Time, tz="America/Los_Angeles")
  ) %>%
  filter(Response == "PM2.5, ug/m³" &
         Name == "AW13") %>%
  filter(Time > as_datetime("2023-01-29",tz="America/Los_Angeles"),
         Time < as_datetime("2023-02-02",tz="America/Los_Angeles"))

x <- find_stream_episodes(data, minpeakheight=45,
                     merge.threshold=45, by="60 seconds")

plot_streams_peaks(x)

# CONTEXTUALIZER
PeakHeight        MaxTimestamp      StartTimestamp
2  393.76047 2023-01-29 12:00:00 2023-01-29 08:22:00
3  175.12354 2023-01-30 17:07:00 2023-01-30 14:17:00
4  669.53384 2023-01-31 17:12:00 2023-01-31 13:36:00
StartHeight        EndTimestamp EndHeight
2    40.63346 2023-01-29 21:21:00  40.43843
3    40.84693 2023-01-30 20:45:00  40.14120
4    41.81332 2023-02-01 01:37:00  40.17738

# SENSOPLOT
PeakHeight        MaxTimestamp      StartTimestamp
1   659.0995 2023-01-29 11:18:00 2023-01-29 09:42:00
2   268.9551 2023-01-30 16:44:00 2023-01-30 15:16:00
3   925.3716 2023-01-31 17:02:00 2023-01-31 14:35:00
StartHeight        EndTimestamp EndHeight
1    41.70398 2023-01-29 20:43:00  40.24757
2    40.80411 2023-01-30 20:46:00  40.19807
3    42.43359 2023-02-01 01:30:00  40.14891
