rm(list = ls())
library(tidyverse)
library(tidymodels)
library(e1071)
library(stats)

pool_1 <- c("PSG", "Bayern Munich", "Man. City", "Juventus", "Liverpool", "Barcelona", "Leipzig", "Valencia")

pool_2 <- c("Real Madrid", "Tottenham Hostpur", "Atalanta", "Atletico Madrid", "Napoli", "Dortmund", "Lyon", "Chelsea")

# incidence-matrix --------------------------------------------------------

incidence_matrix <- matrix(nrow = 8, ncol = 8)
rownames(incidence_matrix) <- pool_1
colnames(incidence_matrix) <- pool_2

# define incidence matrix -------------------------------------------------

incidence_matrix[1, ] <- c(0, 1, 1, 1, 1, 1, 0, 1)
incidence_matrix[2, ] <- c(1, 0, 1, 1, 1, 0, 1, 1)
incidence_matrix[3, ] <- c(1, 0, 0, 1, 1, 1, 1, 0)
incidence_matrix[4, ] <- c(1, 1, 0, 0, 0, 1, 1, 1)
incidence_matrix[5, ] <- c(1, 0, 1, 1, 0, 1, 1, 0)
incidence_matrix[6, ] <- c(0, 1, 1, 0, 1, 0, 1, 1)
incidence_matrix[7, ] <- c(1, 1, 1, 1, 1, 0, 0, 1)
incidence_matrix[8, ] <- c(0, 1, 1, 1, 1, 0, 1, 0)


# functions ---------------------------------------------------------------

sample_team_1 <- function(r) {
  t <- sample(rownames(r), 1)
  t
}

sample_team_2 <- function(r, t1) {
  if (sum(r[t1, ]) > 1) {
    t2 <- sample(names(r[t1, r[t1, ] == 1]), 1)
  } else {
      t2 <- names(which(r[t1, ] == 1))
    }
  
  t2
}

remove_drawn_teams <- function(r, t1, t2) {
  r <- r[!(rownames(r) %in% t1), !(colnames(r) %in% t2)]
  r
}

check_row_unique_match <- function(r) {
  t1 <- NULL
  if (1 %in% rowSums(r)) {
    t1 <- names(which(rowSums(r) == 1))[1]
  }
  t1
}

check_column_unique_match <- function(r) {
  t2 <- NULL
  if (1 %in% colSums(r)) {
    t2 <- names(which(colSums(r) == 1))[1]
  }
  t2
}


# main-simulation-function ------------------------------------------------

simulate_outcome <- function() {
  remainder_incidence_matrix <- incidence_matrix
  outcome <- matrix(rep(0, 64), nrow = 8, ncol = 8)
  rownames(outcome) <- pool_1
  colnames(outcome) <- pool_2
  
  period <- 0
  
  while (period < 8) {
    period <- period + 1
    if (period == 8) {
      team_1 <- names(which(rowSums(outcome) == 0))
      team_2 <- names(which(colSums(outcome) == 0))
      outcome[team_1, team_2] <- 1
      break()
      
    } else {
      team_1 <- sample_team_1(remainder_incidence_matrix)
      team_2 <- sample_team_2(remainder_incidence_matrix, 
                              team_1) 

      outcome[team_1, team_2] <- 1
      remainder_incidence_matrix <- remove_drawn_teams(remainder_incidence_matrix, 
                                                       team_1, 
                                                       team_2)
      
      while (period < 7 && 
             (!is.null(check_row_unique_match(remainder_incidence_matrix)) || 
             !is.null(check_column_unique_match(remainder_incidence_matrix)))) {
        if (period < 7 && 
            !is.null(check_row_unique_match(remainder_incidence_matrix))) {
          period <- period + 1
          team_1 <- check_row_unique_match(remainder_incidence_matrix)
          team_2 <- sample_team_2(remainder_incidence_matrix, team_1)
          outcome[team_1, team_2] <- 1
          remainder_incidence_matrix <- remove_drawn_teams(remainder_incidence_matrix,
                                                           team_1, 
                                                           team_2)
          
        }
        
        if (period < 7 && 
            !is.null(check_column_unique_match(remainder_incidence_matrix))) {
          period <- period + 1
          team_2 <- check_column_unique_match(remainder_incidence_matrix)
          team_1 <- names(which(remainder_incidence_matrix[, team_2] == 1))[1]
          outcome[team_1, team_2] <- 1
          remainder_incidence_matrix <- remove_drawn_teams(remainder_incidence_matrix, 
                                                           team_1, 
                                                           team_2)
        }
      }
    }
  }
  return(outcome)
}


# generating-outcomes -----------------------------------------------------
sample_outcome <- matrix(rep(0, 64), nrow = 8)
rownames(sample_outcome) <- pool_1
colnames(sample_outcome) <- pool_2
N <- 1000
for (i in 1:N) {
  sample_outcome <- sample_outcome + simulate_outcome()
}
print(sample_outcome)
