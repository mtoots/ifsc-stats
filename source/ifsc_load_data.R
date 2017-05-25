# ---- read data ----
ifsc <- readRDS("../ifsc_all.RDs")
ifsc_heat <- readRDS("../ifsc_heat_all.RDs")

# ---- data cleanup/wrangling ----
# ---- remove duplicated ----
ifsc      <- ifsc[          -which(duplicated(ifsc)),]
ifsc_heat <- ifsc_heat[-which(duplicated(ifsc_heat)),]

#fix boulder results
ifsc_heat <- ifsc_heat %>% mutate(seq = str_replace_all(seq, "NA", "0"))

#make heat names uniform
# remove S-Final because it is useless
ifsc_heat <- ifsc_heat %>% filter(heat != "S-FINAL")

#  45 different ways of writing final, semi-final, and qualification

tmp <- ifelse(
  ifsc_heat$heat %in% c(
    "SEMI-FINAL",
    "1/2-FINAL",
    "SEMI FINAL",
    "SEMIFINAL",
    "SEMIFINAL-BOULDERS W1 TO W4",
    "SEMIFINAL-BOULDERS M1 TO M4",
    "SEMINAL-FINAL",
    "SEMFINAL",
    "SEMIFINALE",
    "1/2 FINAL",
    "SEMI-FINALS"
  ), "semi",
  ifelse(
    ifsc_heat$heat %in% c(
      "QUALIFICATION",
      "QUALIFIKATION",
      "QUALIFICATION-GROUP A - BOULDERS 1 TO 6",
      "QUALIFICATION GROUP 1",
      "1. QUALIFICATION",
      "QUALIFICATION-GROUP A",
      "QUALIFICATION-GROUP A_CORRECTED",
      "QUALIFICATION 1",
      "QUALIFICATION (A)",
      "QUALIFICATION (GROUP 1)",
      "1.QUALIFICATION (GROUP A)",
      "1. QUALIFICATION (GROUP A)",
      "QUALIFICATION 1. GROUP",
      "1 QUALIFICATION",
      "A. QUALIFICATION",
      "A QUALIFICATION",
      "1.QUALIFICATION",
      "A.QUALIFICATION"
    ), "qual1",
    ifelse(
      ifsc_heat$heat %in% c(
        "QUALIFICATION-GROUP B - BOULDERS 7 TO 12",
        "2. QUALIFICATION",
        "QUALIFICATION GROUP 2",
        "2. QUALIFICATION-GROUP B",
        "2. QUALIFICATION-GROUP B_CORRECTED",
        "QUALIFICATION 2",
        "QUALIFICATION (B)",
        "QUALIFICATION (GROUP 2)",
        "2. QUALIFICATION (GROUP B)",
        "QUALIFICATION 2. GROUP",
        "2 QUALIFICATION",
        "B. QUALIFICATION",
        "B QUALIFICATION"
      ), "qual2", "final"
    )
  )
)
ifsc_heat <- ifsc_heat %>% mutate(heat = tmp)

# ---- Add name ----
ifsc      <- ifsc      %>% mutate(name = str_c(first, last, sep = " "))
ifsc_heat <- ifsc_heat %>% mutate(name = str_c(first, last, sep = " "))

# ---- fix city names ----
ifsc <- ifsc %>% mutate(comp_city = ifelse(comp_city == "Imst/Innsbruck", "Innsbruck", comp_city))

# ---- fix sum (e.g. 0t 3b6 -> 0t0 3b6)
ifsc_heat <- ifsc_heat %>%
  mutate(sum = str_replace(sum, "t ", "t0 ") ) %>%
  mutate(sum = str_replace(sum, "b$", "b0") ) 

ifsc <- ifsc %>%
  mutate(final = str_replace(final, "t ", "t0 ") ) %>%
  mutate(final = str_replace(final, "b$", "b0") ) %>%
  mutate(semifinal = str_replace(semifinal, "t ", "t0 ") ) %>%
  mutate(semifinal = str_replace(semifinal, "b$", "b0") ) %>%
  mutate(qual1 = str_replace(qual1, "t ", "t0 ") ) %>%
  mutate(qual1 = str_replace(qual1, "b$", "b0") ) %>%
  mutate(qual2 = str_replace(qual2, "t ", "t0 ") ) %>%
  mutate(qual2 = str_replace(qual2, "b$", "b0") ) 

parse_result <- function(result, bouldertext){
  n <- str_length(bouldertext)
  
  a <- bouldertext
  res <- str_split(result, "")[[1]]
  a <- str_split(bouldertext, "")[[1]]
  
  j <- 1
  z <- rep(NA, n*2)
  for(i in 1:n) {
    z[(2 * i - 1):(2 * i)] <-
      switch(
        a[i],
        "t" = {
          j = j + 2
          res[(j-2):(j - 2 + 1)]
        },
        "b" = {
          j = j + 1
          c(0, res[j-1])
        },
        "0" = c(0, 0)
      )
  }
  z
}

seq_parsed <- map2(ifsc_heat$result, ifsc_heat$seq, parse_result)
seq_parsed2 <-
  sapply(seq_parsed, function(x) {
    y <- rep(NA, 12)
    y[1:length(x)] <- x
    names(y) <- str_c(c("t", "b"), rep(1:6, each = 2))
    y
  })

ifsc_heat <- ifsc_heat %>% bind_cols(as_data_frame(t(seq_parsed2)))
ifsc_heat <- ifsc_heat %>% mutate(n_boulders = str_length(seq))

heat_long <- ifsc_heat %>% 
  gather(key = boulder, value = attempts, t1:b6) %>%
  filter(!is.na(attempts))

ifsc <- ifsc %>%
  mutate(final = ifelse(is.na(final), "", final),
         semifinal = ifelse(is.na(semifinal), "", semifinal),
         qual1 = ifelse(is.na(qual1), "", qual1),
         qual2 = ifelse(is.na(qual2), "", qual2)) %>%
  mutate(ntops = ifelse(final == "", 0, str_match(final, "([0-9]+)t")[,2]) %>% as.double() + 
           ifelse(semifinal == "", 0, str_match(semifinal, "([0-9]+)t")[,2]) %>% as.double() + 
           ifelse(qual1 == "", 0, str_match(qual1, "([0-9]+)t")[,2]) %>% as.double() + 
           ifelse(qual2 == "", 0, str_match(qual2, "([0-9]+)t")[,2]) %>% as.double())


#---- make flash proportion by year dataset ----
df_flash_prop_by_year <- heat_long %>%
  filter(str_detect(boulder, "t")) %>%
  group_by(name) %>%
  mutate(n_comp = n_distinct(comp_id)) %>% 
  group_by(name, year) %>%
  summarise(boulders = n(),
            flashes = sum(attempts == 1),
            n_comp = first(n_comp),
            sex = first(sex)) %>%
  mutate(flash_prob = flashes / boulders) 

#saveRDS(df_flash_prop_by_year, "dat/df_flash_prop_by_year.RDs")
