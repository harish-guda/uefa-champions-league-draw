rm(list = ls())
library(tidyverse)
library(tidymodels)
library(stats)
library(e1071)
team_names_pool_1 <- c("PSG", 
                       "Bayern Munich", 
                       "Man. City", 
                       "Juventus", 
                       "Liverpool", 
                       "Barcelona", 
                       "Leipzig", 
                       "Valencia")

team_names_pool_2 <- c("Real Madrid", 
                       "Tottenham Hostpur", 
                       "Atalanta", 
                       "Atletico Madrid", 
                       "Napoli", 
                       "Dortmund", 
                       "Lyon", 
                       "Chelsea")

pool_1 <- c(1:8)
# 1: PSG
# 2: Bayern
# 3: Man. City
# 4: Juventus
# 5: Liverpool
# 6: Barcelona
# 7: Leipzig
# 8: Valencia



pool_2 <- c(1:8)
# 1: RM
# 2: Tottenham
# 3: Atalanta
# 4: ATM
# 5: Napoli
# 6: Dortmund
# 7: Lyon
# 8: Chelsea
names(pool_1) <- team_names_pool_1
names(pool_2) <- team_names_pool_2

compatible_teams_pool_2 <- function(tp1 = 1){
  ctp2 <- NULL # the set of compatible teams
  # compatibility = teams did not meet in the group stages and are not from the same country. E.g., for PSG, it cannot be matched with RM (met in group stages) and cannot be matched with Lyon (same country)
  if (tp1 == 1) {ctp2 <- c(2, 3, 4, 5, 6, 8)}
  if (tp1 == 2) {ctp2 <- c(1, 3, 4, 5, 7, 8)}
  if (tp1 == 3) {ctp2 <- c(1, 4, 5, 6, 7)}
  if (tp1 == 4) {ctp2 <- c(1, 2, 6, 7, 8)}
  if (tp1 == 5) {ctp2 <- c(1, 3, 4, 6, 7)}
  if (tp1 == 6) {ctp2 <- c(2, 3, 5, 7, 8)}
  if (tp1 == 7) {ctp2 <- c(1, 2, 3, 4, 5, 8)}
  if (tp1 == 8) {ctp2 <- c(2, 3, 5, 6, 7)}
  
  ctp2
}

# Don't be an idiot. Write a function if it is used at least twice. 
generate_incidence_matrix <- function(){
  incidence_matrix <- matrix(rep(0, 64), nrow = 8, ncol = 8)
  for (i in 1:8) {
    ctp2 <- compatible_teams_pool_2(i)
    for (j in 1:8) {
      if (j %in% ctp2) {
        incidence_matrix[i, j] <- 1
      }
    }
  }
  
  incidence_matrix
}

incidence_matrix <- generate_incidence_matrix()
rownames(incidence_matrix) <- team_names_pool_1
colnames(incidence_matrix) <- team_names_pool_2

rowSums(incidence_matrix)
colSums(incidence_matrix)

sample_teams <- function(teams){
  n <- length(teams)
  if (n == 1) {
    t <- teams
  } else {
    t <- sample(teams, 1, F, prob = rep(1/n, n))
  }
  t
}

simulate_outcome <- function() {
  outcome <- matrix(rep(0, 64), nrow = 8)
  rownames(outcome) <- team_names_pool_1
  colnames(outcome) <- team_names_pool_2
  remainder_incidence_matrix <- incidence_matrix
  remaining_pool_1 <- pool_1
  remaining_pool_2 <- pool_2
  period <- 0
  # draw starts 
  
  while (!(length(remaining_pool_1) == 0)) {
    # begin
    period <- period + 1
    # print(paste("start of draw", period))
    t_p1 <- sample_teams(remaining_pool_1)
    name_t_p1 <- team_names_pool_1[t_p1]
    # print(name_t_p1)
    
    compatible_and_remaining_pool_2 <- intersect(remaining_pool_2,  compatible_teams_pool_2(t_p1))
    # print(team_names_pool_2[compatible_and_remaining_pool_2])
    
    t_p2 <- sample_teams(compatible_and_remaining_pool_2)
    name_t_p2 <- team_names_pool_2[t_p2]
    # print(name_t_p2)
    
    outcome[t_p1, t_p2] <- 1
    # print(outcome)
    
    remaining_pool_1 <- remaining_pool_1[!(remaining_pool_1 %in% t_p1)]
    remaining_pool_2 <- remaining_pool_2[!(remaining_pool_2 %in% t_p2)]
    
    if (length(remaining_pool_1) == 0) {break()}
    remainder_incidence_matrix <- remainder_incidence_matrix[team_names_pool_1[remaining_pool_1], 
                                                             team_names_pool_2[remaining_pool_2]]
    # print(remainder_incidence_matrix)
    # readline(prompt = "Press [enter] to continue")
    
    if (length(remaining_pool_1) > 1) {
      while (length(remaining_pool_1) > 1 
             && (1 %in% rowSums(remainder_incidence_matrix) 
                 || (1 %in% colSums(remainder_incidence_matrix)))) {
        # readline(prompt = "Unique match for a team detected")
        period <- period + 1
        # print(paste("start of draw", period))
        if (1 %in% rowSums(remainder_incidence_matrix)) {
          name_t_p1 <- rownames(remainder_incidence_matrix)[which(rowSums(remainder_incidence_matrix) == 1, arr.ind = T)][1]
          # print(name_t_p1)
          name_t_p2 <- names(remainder_incidence_matrix[name_t_p1, ])[which(remainder_incidence_matrix[name_t_p1,] == 1)]
          # print(name_t_p2)
          outcome[name_t_p1, name_t_p2] <- 1
          remaining_pool_1 <- remaining_pool_1[!(names(remaining_pool_1) %in% name_t_p1)]
          remaining_pool_2 <- remaining_pool_2[!(names(remaining_pool_2) %in% name_t_p2)]
          
          if (length(remaining_pool_1) == 0) {break()}
          remainder_incidence_matrix <- remainder_incidence_matrix[team_names_pool_1[remaining_pool_1], 
                                                                   team_names_pool_2[remaining_pool_2]]
          # print(remainder_incidence_matrix)
          # readline(prompt = "Press [enter] to continue")
        } else if (1 %in% colSums(remainder_incidence_matrix)){
          name_t_p2 <- colnames(remainder_incidence_matrix)[which(colSums(remainder_incidence_matrix) == 1, arr.ind = T)][1]
          # print(name_t_p2)
          name_t_p1 <- names(remainder_incidence_matrix[, name_t_p2])[which(remainder_incidence_matrix[, name_t_p2] == 1)]
          # print(name_t_p1)
          outcome[name_t_p1, name_t_p2] <- 1
          remaining_pool_1 <- remaining_pool_1[!(names(remaining_pool_1) %in% name_t_p1)]
          remaining_pool_2 <- remaining_pool_2[!(names(remaining_pool_2) %in% name_t_p2)]
          
          if (length(remaining_pool_1) == 0) {break()}
          remainder_incidence_matrix <- remainder_incidence_matrix[team_names_pool_1[remaining_pool_1], 
                                                                   team_names_pool_2[remaining_pool_2]]
          # print(remainder_incidence_matrix)
          # readline(prompt = "Press [enter] to continue")
        }
      }
    }
  }
    
  outcome
}

simulate_outcome()

i <- 0
r <- matrix(rep(0, 64), nrow = 8)
while (i < 300) {
  i <- i + 1
  r <- r + simulate_outcome()
}
print(r)


x <- 0
while(!(is.null(x))){
  outcome <- matrix(rep(0, 64), nrow = 8)
  rownames(outcome) <- team_names_pool_1
  colnames(outcome) <- team_names_pool_2

  remainder_incidence_matrix <- incidence_matrix

  remaining_pool_1 <- pool_1
  remaining_pool_2 <- pool_2

  period <- 0
  x <- NULL
}
rm(x)
### Let me see if I can at least generate one successful outcome. 
while (!(length(remaining_pool_1) == 0)) {
  # begin
  period <- period + 1
  print(paste("start of draw", period))
  t_p1 <- sample_teams(remaining_pool_1)
  name_t_p1 <- team_names_pool_1[t_p1]
  print(name_t_p1)
  
  compatible_and_remaining_pool_2 <- intersect(remaining_pool_2,  compatible_teams_pool_2(t_p1))
  # print(team_names_pool_2[compatible_and_remaining_pool_2])
  
  t_p2 <- sample_teams(compatible_and_remaining_pool_2)
  name_t_p2 <- team_names_pool_2[t_p2]
  print(name_t_p2)
  
  outcome[t_p1, t_p2] <- 1
  # print(outcome)
  
  remaining_pool_1 <- remaining_pool_1[!(remaining_pool_1 %in% t_p1)]
  remaining_pool_2 <- remaining_pool_2[!(remaining_pool_2 %in% t_p2)]
  
  if (length(remaining_pool_1) == 0) {break()}
  remainder_incidence_matrix <- remainder_incidence_matrix[team_names_pool_1[remaining_pool_1], 
                                                           team_names_pool_2[remaining_pool_2]]
  print(remainder_incidence_matrix)
  readline(prompt = "Press [enter] to continue")
  
  # check remainder graph if some team can match with exactly one match. 
  # DON'T KNOW WHAT TO DO ! 
  if (length(remaining_pool_1) > 1) {
      while (length(remaining_pool_1) > 1 
             && (1 %in% rowSums(remainder_incidence_matrix) 
                 || (1 %in% colSums(remainder_incidence_matrix)))) {
      readline(prompt = "Unique match for a team detected")
      period <- period + 1
      print(paste("start of draw", period))
      if (1 %in% rowSums(remainder_incidence_matrix)) {
        name_t_p1 <- rownames(remainder_incidence_matrix)[which(rowSums(remainder_incidence_matrix) == 1, arr.ind = T)][1]
        print(name_t_p1)
        name_t_p2 <- names(remainder_incidence_matrix[name_t_p1, ])[which(remainder_incidence_matrix[name_t_p1,] == 1)]
        print(name_t_p2)
        outcome[name_t_p1, name_t_p2] <- 1
        remaining_pool_1 <- remaining_pool_1[!(names(remaining_pool_1) %in% name_t_p1)]
        remaining_pool_2 <- remaining_pool_2[!(names(remaining_pool_2) %in% name_t_p2)]
      
        if (length(remaining_pool_1) == 0) {break()}
        remainder_incidence_matrix <- remainder_incidence_matrix[team_names_pool_1[remaining_pool_1], 
                                                               team_names_pool_2[remaining_pool_2]]
        print(remainder_incidence_matrix)
        readline(prompt = "Press [enter] to continue")
      } else if (1 %in% colSums(remainder_incidence_matrix)){
        name_t_p2 <- colnames(remainder_incidence_matrix)[which(colSums(remainder_incidence_matrix) == 1, arr.ind = T)][1]
        print(name_t_p2)
        name_t_p1 <- names(remainder_incidence_matrix[, name_t_p2])[which(remainder_incidence_matrix[, name_t_p2] == 1)]
        print(name_t_p1)
        outcome[name_t_p1, name_t_p2] <- 1
        remaining_pool_1 <- remaining_pool_1[!(names(remaining_pool_1) %in% name_t_p1)]
        remaining_pool_2 <- remaining_pool_2[!(names(remaining_pool_2) %in% name_t_p2)]
      
        if (length(remaining_pool_1) == 0) {break()}
        remainder_incidence_matrix <- remainder_incidence_matrix[team_names_pool_1[remaining_pool_1], 
                                                               team_names_pool_2[remaining_pool_2]]
        print(remainder_incidence_matrix)
        readline(prompt = "Press [enter] to continue")
      }
      }
  }

}

print(outcome)
print(rowSums(outcome))

### Mismatch occurs in the following example: 
#####          Tottenham Hostpur Atletico Madrid Napoli Chelsea
# Juventus                  1               0      0       1
# Barcelona                 1               0      1       1
# Leipzig                   1               1      1       1
# Valencia                  1               0      1       0
# Atletico Madrid needs to be immediately matched to Leipzig. However, if Leipzig gets matched with any other team other than Atletico Madrid, then we are left with an incompatible match. 