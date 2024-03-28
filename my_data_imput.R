# Packages
library(tidyverse)
library(here)


interviews <- read_csv(
  here("data_raw", "SAFI_clean.csv") , 
  na = "NULL"
)


glimpse(interviews)
nrow(interviews)
ncol(interviews)
summary(interviews)

# select, filter, mutate, group_by, summarise, arrange

# select: columns
select(interviews,key_ID, village, no_membrs)
select(interviews, -liv_count)
select(interviews, key_ID:rooms)

# filter: rows
filter(interviews, village == "Chirodzo")
chirodzo <- filter(interviews, village == "Chirodzo")
chirodzo <- select(chirodzo, -village)

filter(interviews, village == "Chirodzo" & rooms > 1)
filter(interviews, village == "Chirodzo" | village == "Ruaca")

# Pipes: Multiple functions at once
chirodzo <- filter(interviews, village == "Chirodzo")
chirodzo <- select(chirodzo, -village)

# Nested functions
select(filter(interviews, village == "Chirodzo"), -village)

# Pipe
interviews %>% 
  filter(village == "Chirodzo") %>% 
  select(-village)


# mutate: create new columns
interviews |> 
  mutate(per_room = no_membrs / rooms) |> 
  select(village, no_membrs, rooms, per_room)

interviews <- interviews |> mutate(interview_date = as_date(interview_date))


# summarise 
interviews |> 
  summarise(mean_membrs = mean(no_membrs))

# Extract column
no_membrs <- interviews$no_membrs
mean(no_membrs)

# Group by and summarise
interviews |> 
  group_by(village) |> 
  summarise(mean_membrs = mean(no_membrs))

interviews |> 
  summarise(mean_membrs = mean(no_membrs),
            .by = village)

interviews |> 
  summarise(mean_membrs = mean(no_membrs),
            min_membrs = min(no_membrs),
            n = n(),
            .by = c(village, memb_assoc))

interviews |> 
  summarise(n = n(), .by = village)

interviews |> 
  filter(!is.na(memb_assoc)) |> 
  summarise(mean_membrs = mean(no_membrs),
            min_membrs = min(no_membrs),
            n = n(),
            .by = c(village, memb_assoc))

# arrange
interviews |> 
  filter(!is.na(memb_assoc)) |> 
  summarise(mean_membrs = mean(no_membrs),
            min_membrs = min(no_membrs),
            n = n(),
            .by = c(village, memb_assoc)) |> 
  arrange(n) # ascending

interviews |> 
  filter(!is.na(memb_assoc)) |> 
  summarise(mean_membrs = mean(no_membrs),
            min_membrs = min(no_membrs),
            n = n(),
            .by = c(village, memb_assoc)) |> 
  arrange(desc(n)) #descending
