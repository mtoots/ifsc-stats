source("~/Dropbox/R/function_libs/funlib.R")
library(tidyverse)
library(stringr)
'%|%' <- function(a,b) paste0(a,b)

source("ifsc_load_data.R")

#find mis-spelled names
library(utils)
name <- ifsc %>% 
  count(name) %>% .[[1]]
namedist <- adist(firstlast, firstlast)

ind <- which(namedist < 3 & namedist > 0, arr.ind = T)
ind <- ind[ind[,1] < ind[,2],]
cbind(firstlast[ind[,1]], 
      firstlast[ind[,2]])

#None of them very important

firstlast <- ifsc_heat %>% transmute(name = str_c(first, last, sep=" ")) %>% count(name) %>% .[[1]]
namedist <- adist(firstlast, firstlast)

ind <- which(namedist < 3 & namedist > 0, arr.ind = T)
ind <- ind[ind[,1] < ind[,2],]
cbind(firstlast[ind[,1]], 
      firstlast[ind[,2]])

#Same conclusion

ifsc_heat %>% 
  filter(last == "Stöhr") %>%
  filter(heat %in% c("semi")) %>%
  ggplot(aes(x = prev.heat, y = rank)) + geom_point(se=F) + geom_smooth()


which(ifsc_heat$rank == 80)
ifsc_heat[1517,]
ifsc %>% filter(comp_id == 1106) %>% View


women_wide <- ifsc_heat_long %>% 
  filter(sex == "women") %>%
  mutate(tmp = str_c(comp_id, "-", heat,"-", boulder)) %>%
  select(-comp_id, -heat, -boulder, -rank, -result, -sum, -prev.heat, -seq, -sex, -year) %>%
  spread(key = tmp, value = attempts)

women_wide <- women_wide[, !sapply(women_wide, function(x) all(is.na(x)))]

men_wide <- ifsc_heat_long %>% 
  filter(sex == "women") %>%
  mutate(tmp = str_c(comp_id, "-", heat,"-", boulder)) %>%
  select(-comp_id, -heat, -boulder, -rank, -result, -sum, -prev.heat, -seq, -sex, -year) %>%
  spread(key = tmp, value = attempts)

men_wide <- men_wide[, !sapply(men_wide, function(x) all(is.na(x)))]

men_wide

#porportion of tops reached with a given number of attempts
women_wide %>% 
  select(first, last, nation, matches("-t[0-9]")) %>% 
  gather(key = boulder, value = attempts, -first, -last, -nation, convert = T) %>%
  filter(!is.na(attempts)) %>%
  group_by(first, last) %>%
  mutate(boulders_total = n()) %>%
  filter(boulders_total > 50) %>%
  count(first,last,boulders_total,attempts) %>%
  mutate(proportion = n/boulders_total) %>%
  mutate(name = str_c(first, last, sep=" ")) %>%
  #filter(name %in% c("Akiyo Noguchi", "Miho Nonaka")) %>%
  ggplot(aes(x = as.numeric(attempts), y = proportion, group = name)) + 
  geom_line(alpha = 0.2) +
  theme(legend.position = "none")

# find similarities between climbers
# Code boulder that was not topped as topped in 20 attempts (arbitrary amount)
# Same for bonus
most50women <- ifsc_heat %>%
  filter(sex == "women") %>%
  group_by(first,last) %>%
  summarise(n = length(unique(comp_id))) %>%
  arrange(desc(n)) %>% ungroup %>% top_n(50) %>% select(-n)


women <- most50women %>% transmute(name = str_c(first, last, sep = " ")) %>% .[[1]]
M <- women_wide %>% right_join(most50women) %>%
  select(-last, -first, -nation) %>% 
  as.matrix() %>% 
  as.numeric() %>% matrix(nrow = length(women))


bc_dist <- function(x, y){
  #computes Bray-Curtis dissimilarity
  both <- !(is.na(x) | is.na(y))
  x2 <- x[both]
  y2 <- y[both]
  sum(abs(x2 - y2))/(sum(x2) + sum(y2))
}

bc <- matrix(0, nrow(M), nrow(M))
for(i in 1:(nrow(M)-1)){
  for(j in (i+1):nrow(M)){
    bc[i,j] <- bc_dist(M[i,], M[j,])
  }
  print(i)
}

bc <- bc + t(bc)
rownames(bc) <- women
apply(bc, 1, function(x) sum(is.nan(x)))

which(bc == min(bc[bc > 0], na.rm = T), arr.ind = T)

ind <- !is.na(M[12,]) & !is.na(M[23,])
M[12,ind]
M[23,ind]

ifsc_heat %>% filter(first == "Maud", last == "Ansade") %>% View
ifsc_heat %>% filter(first == "Maud") %>% select(comp_id) %>% .[[1]] %>% 
  intersect(ifsc_heat %>% filter(last == "Coxsey") %>% select(comp_id) %>% .[[1]])

ifsc %>% filter(comp_id == 1249) %>% View

# ---- Akiyo Noguchis attempts profile by year ----
ifsc_heat_long %>%
  filter(last == "Noguchi") %>%
  filter(str_detect(boulder, "t")) %>%
  filter(heat %in% c("final", "semi")) %>%
  filter(!is.na(attempts)) %>%
  ggplot(aes(x = attempts, group = as.factor(year))) +
  geom_line(aes(y = (..count..)/sum(..count..), color = year), stat = "count") + 
  scale_colour_distiller(palette = "Spectral") + theme_minimal()
  
ifsc_heat_long %>%
  filter(last == "Gelmanov") %>%
  filter(str_detect(boulder, "t")) %>%
  filter(!is.na(attempts)) %>%
  arrange(comp_id) %>%
  ggplot(aes(x = comp_id, fill = attempts)) +
  geom_bar(aes(y = (..count..)), stat = "count", position = position_fill()) #+

ggplotly()

ifsc %>% filter(comp_id == 6170)

#---- Proportion of flashes, by year ----
ifsc_heat_long %>%
  filter(last %in% c("Noguchi", "Stöhr", "Coxsey", "Nonaka", "Garnbret")) %>%
  mutate(name = str_c(first, last, sep = " ")) %>%
  filter(str_detect(boulder, "t")) %>%
  filter(!is.na(attempts)) %>%
  group_by(name, year) %>%
  summarise(boulders = n(), flashes = sum(attempts == 1)) %>%
  mutate(flash_prob = flashes / boulders) %>%
  ggplot(aes(x = year, y = flash_prob, color = name)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 2007:2017) +
  ylab("Flash proportion") + 
  theme_minimal() + theme(panel.grid.minor.x = element_blank())
  
ggplotly()

ifsc_heat_long %>%
  filter(last %in% c("Noguchi", "Stöhr", "Coxsey", "Saurwein")) %>%
  mutate(name = str_c(first, last, sep = " ")) %>%
  filter(str_detect(boulder, "t")) %>%
  filter(!is.na(attempts)) %>%
  group_by(name, year, comp_id) %>%
  summarise(boulders = n(), flashes = sum(attempts == 1)) %>%
  mutate(flash_prob = flashes / boulders) %>%
  ggplot(aes(x = year, y = flash_prob, group = name, color = name)) +
  geom_smooth(aes(x = year), se = F) + geom_point() +
  scale_x_continuous(breaks = 2007:2017) +
  ylab("Flash proportion by competition") + 
  theme_minimal() + theme(panel.grid.minor.x = element_blank())



# ---- Average rank ----
ifsc %>%
  filter(last %in% c("Noguchi", "Stöhr", "Coxsey", "Nonaka", "Garnbret")) %>%
  mutate(name = str_c(first, last, sep = " ")) %>%
  group_by(name, year) %>%
  summarise(avg_rank = mean(final_rank)) %>%
  ggplot(aes(x = year, y = avg_rank, color = name)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 2007:2017) + scale_y_reverse(breaks = 1:20) +
  theme_minimal() + theme(panel.grid.minor = element_blank()) +
  ylab("Avereage rank")

ifsc %>%
  filter(last %in% c("Noguchi", "Stöhr", "Coxsey", "Nonaka", "Saurwein", "Pinggera")) %>%
  mutate(name = str_c(first, last, sep = " ")) %>%
  ggplot(aes(x = year, y = final_rank, color = name)) +
  geom_smooth(se=F) + geom_point(position = position_jitter(height = 0)) +
  scale_x_continuous(breaks = 2007:2017) + scale_y_reverse(breaks = 1:20, limits = c(20,1)) +
  theme_minimal() + theme(panel.grid.minor = element_blank()) +
  ylab("Final rank")

# --- Proportion of boulders topped (any number of attempts) ----
ifsc_heat_long %>%
  filter(last %in% c("Noguchi", "Stöhr", "Coxsey", "Nonaka", "Garnbret")) %>%
  mutate(name = str_c(first, last, sep = " ")) %>%
  filter(str_detect(boulder, "t")) %>%
  filter(!is.na(attempts)) %>%
  filter(heat %in% c("semi", "final")) %>%
  group_by(name, year) %>%
  summarise(boulders = n(), tops = sum(attempts > 0)) %>%
  mutate(top_prob = tops / boulders) %>%
  ggplot(aes(x = year, y = top_prob, color = name)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 2007:2017) +
  ylab("Proportion of boulders topped") + 
  theme_minimal() + theme(panel.grid.minor.x = element_blank())

ifsc_heat_long %>%
  filter(last %in% c("Noguchi", "Stöhr", "Coxsey", "Nonaka", "Puccio", "Pinggera")) %>%
  mutate(name = str_c(first, last, sep = " ")) %>%
  filter(str_detect(boulder, "t")) %>%
  filter(!is.na(attempts)) %>%
  # filter(heat %in% c("semi", "final")) %>%
  group_by(name, year, comp_id) %>%
  summarise(tops = sum(attempts > 0)) %>%
  ggplot(aes(x = year, y = tops, color = name)) +
  geom_smooth(se=F) + geom_point() +
  scale_x_continuous(breaks = 2007:2017) +
  ylab("Number of boulders topped") + 
  theme_minimal() + theme(panel.grid.minor.x = element_blank())




women_bid <- ifsc_heat_long %>%
  filter(sex == "women") %>%
  filter(!is.na(attempts)) %>%
  mutate(bid = str_c(comp_id, boulder, sep = "-"),
         name = str_c(first, last, sep = " ")) 
names_bid <- women_bid %>% select(name) %>% .[[1]]  
bid <- women_bid %>% select(bid) %>% .[[1]]  
names_bid_unique <- sort(unique(names_bid))

#Find common boulders between all competitors
M <- matrix(0, length(names_bid_unique), length(names_bid_unique))
for(i in 1:(length(names_bid_unique) - 1)){
  for(j in (i+1):length(names_bid_unique)){
    bid1 <- bid[names_bid == names_bid_unique[i]]
    bid2 <- bid[names_bid == names_bid_unique[j]]
    M[i,j] <- length(intersect(bid1, bid2))
  }
  print(i)
}
i = 47
j = 157
names_bid_unique[cols][11]
which(names_bid_unique == "Andrea Kümin")
which(names_bid_unique == "Elise Sethna")
M <- M + t(M)
diag(M) <- women_bid %>% group_by(name) %>% summarise(n = n()) %>% .[["n"]]
which(diag(M) == max(diag(M)))
names_bid_unique[9]

#Find largest subset
#
#find all competitors that have all done at least k common boulders
k <- 20 #about 2 competitions together

cols <- 1:length(names_bid_unique)
while(any(M[cols, cols] < k) && length(cols) > 1){
  cols <- cols[-which.max(colSums(M[cols, cols] < k))]
}


bid_wide <- women_wide %>%
  # select(noquote(order(colnames(women_wide)))) %>%
  mutate(name = str_c(first, last, sep =" ")) %>%
  filter(name %in% names_bid_unique[cols]) %>%
  arrange(name) %>%
  select(-last, -first, -nation, -name) %>% 
  as.matrix()
  
bid_wide <- matrix(as.numeric(bid_wide), nrow = length(cols))
#remove columns that are all NA
bid_wide <- bid_wide[, -which(apply(bid_wide, 2, function(x) all(is.na(x))))]

IMG(bid_wide)
bid_wide[bid_wide == 0] <- 10

dist(bid_wide)


euc_dist <- function(x, y){
  i <- !is.na(x) & !is.na(y)
  
  sqrt(sum((x[i] - y[i])^2))
}  
euc_dist(bid_wide[4,], bid_wide[11,])


D <- matrix(0, nrow(bid_wide), nrow(bid_wide))
for(i in 1:(nrow(bid_wide)-1)){
  for(j in (i+1):nrow(bid_wide)){
    D[i,j] <- bc_dist(M[i,], M[j,])
  }
  print(i)
}

D <- D + t(D)
rownames(D) <- names_bid_unique[cols]
nm <- names_bid_unique[cols]

ts <- Rtsne(D, is_distance = T, verbose = T, pca = F, perplexity = 10, theta=0)
plot(ts$Y, type="n")
text(ts$Y, label = nm, cex = 0.5)

ifsc_heat %>% filter(last == "Pinggera")




ifsc_heat_long <- ifsc_heat_long %>%
  filter(str_detect(boulder, "t")) %>%
  group_by(name, comp_id) %>%
  filter(!is.na(attempts)) %>%
  summarise(ntops = sum(attempts > 0)) 

ifsc %>%
  select(name, comp_id, ntops) %>%
  left_join(ifsc_heat_long, by = c("name", "comp_id")) %>%
  filter(ntops.x != ntops.y) %>% View

ifsc %>% filter(comp_id == 1247, name == "Alex Johnson")
ifsc_heat %>% filter(comp_id == 1262) %>% View
filter(ifsc, comp_id == "1262")


heat_long %>%
  group_by(name) %>%
  summarise(n = n_distinct(comp_id))


top_names_men <- rankings %>%
  filter(rank < 2 &
         sex == "men") %>%
  count(name) %>% .[[1]]

rankings %>%
  filter(name %in% top_names_men) %>%
  ggplot(aes(x = year, y = rank, color = name)) +
  geom_line() + geom_point() +
  scale_y_reverse() +
#  scale_color_tableau() + 
  theme_minimal()
ggplotly()


top_names_women <- rankings %>%
  filter(rank < 2 &
         sex == "women") %>%
  count(name) %>% .[[1]]

library(scales)
rev_sqrt_trans <- trans_new("rev_sqrt", function(x) -sqrt(x), function(x)
(-x)^2, function(x) bquote(.((-x)^2)))

rankings %>%
  filter(name %in% top_names_women) %>%
  ggplot(aes(x = year, y = rank, color = name)) +
  geom_line() + geom_point() +
  scale_y_continuous(breaks = function(y) ceiling(y[1]):1, trans = rev_sqrt_trans) +
  scale_color_tableau() + 
  theme_minimal()

ggplotly()

library(ggthemes)

rankings %>%
  filter(
    !name %in% c(
      "Akiyo Noguchi",
      "Anna Stöhr",
      "Miho Nonaka",
      "Shauna Coxsey",
      "Janja Garnbret"
    ) &
    sex == "women" &
    rank %in% c(1, 2, 3)
  )



#---- Number of competition wins by year ----
ifsc %>%
  filter(name %in% c("Akiyo Noguchi","Anna Stöhr","Miho Nonaka","Shauna Coxsey","Janja Garnbret")) %>%
  filter(final_rank == 1) %>%
  group_by(sex, name, year) %>%
  summarize(n = n()) %>%
  ggplot(aes(year, n, color=name)) + geom_point() + geom_line()
