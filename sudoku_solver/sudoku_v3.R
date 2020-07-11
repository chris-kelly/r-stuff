## Remove potential numbers for that cell if cell in matrix 10 is already populated
remove_same_cell <- function(a) {
  for(potential_number in 1:9) {
    a[,,potential_number] <- a[,,potential_number]*(a[,,10] == 0)/1
  }
  return(a)
}

## Remove potential numbers if matrix 10 already has that number in the same constraint (e.g. row/column/box/other)
remove_filled_numbers_from_option_vector <- function(option_vector,filed_numbers) {
  return(option_vector*(!(option_vector %in% filed_numbers))/1)
}

eliminate_candidate_numbers_in_same_constraint <- function(a, cs, potential_numbers = 1:9) {
  for(i in cs) {
    for(k in potential_numbers) {
      a[cbind(i,k)] <- remove_filled_numbers_from_option_vector(a[cbind(i,k)],a[cbind(i,10)])
    }
  }
  return(a)
}

cleanup <- function(a,cs,potential_numbers=1:9) {
  a <- remove_same_cell(a)
  a <- eliminate_candidate_numbers_in_same_constraint(a,cs,1:9)
  return(a)
}



## If only one potential number available, set matrix 10 cell to it
only_option_for_cell_so_update_matrix10 <- function(a,cs,target_row=1:9,target_col=1:9,potential_numbers=1:9) {
  updates <- list()
  for(j in target_col) {
    for(i in target_row) {
      if(sum(a[i,j,1:9] != 0) == 1) {
        a[i,j,] <- c(rep(0), max(a[i,j,]))
        a <- cleanup(a,cs,potential_numbers)
        # Message
        relevant_constraints <- cs[apply(sapply(cs, function(x) x[,1] == i & x[,2] == j),2,sum)>0]
        coords <- sapply((1:9)[!(1:9 %in% a[i,j,10])], function(x) do.call(rbind, relevant_constraints)[match(x, a[cbind(do.call(rbind, relevant_constraints),10)]),])
        cell_updates <- list(i = i, j = j, value = a[i,j,10], constraints = t(coords),
                             message = paste0('Cell ', i, ',', j, ' filled with ', a[i,j,10], ' (since '
                                              , paste0((1:9)[!(1:9 %in% a[i,j,10])],' is in [', apply(coords,2,FUN=function(x) paste0(x,collapse=',')),'], ',collapse='')
                                              ,')', collapse = ''))
        updates[[paste0(i,j)]] <- cell_updates
        print(cell_updates$message)
      }
    }
  }
  return(a)
  # return(list(sudoku3d = a, matrix_updated = updates))
}

## If only potential number possible within a constraint, set matrix 10 cell to it
only_option_in_constraint_so_update_matrix10 <- function(a,cs,potential_numbers=1:9) {
  for(i in cs) {
    for(k in potential_numbers) {
      if(sum(a[cbind(i,k)] != 0) == 1) {
        a[cbind(i,10)][which.max(a[cbind(i,k)])] <- k
        # Message
        pos <- which.max(a[cbind(i,k)])
        message <- paste0('Cell ', i[pos,1], ',', i[pos,2], ': filled with ', k
                          , ' as no other cells can be ', k, ' in the same constraint ('
                          , paste0('[',i[,1],',',i[,2],']',collapse=' '), ')')
        print(message)
      }
      a <- cleanup(a,cs,potential_numbers)
    }
  }
  return(a)
}

## Eliminate candidates based on restricted combinations
eliminate_candidates_by_restricted_combinations <- function(a,cs,comp_nums=c(2)) {
  for(cn in comp_nums) {
    for(i in cs) {
      vector_list = mapply(function(row,col){a[row,col,1:9]},i[,1],i[,2])
      old_vector_list <- vector_list
      vector_list[vector_list==0] <- NA
      col_indicies <- c(1:9)[apply(vector_list,2,FUN = function(x) sum(is.na(x)) != 9)]
      if(length(col_indicies) >= cn) {
        combn_element_indicies <- combn(col_indicies,cn) 
        unique_num_combinations <- apply(combn_element_indicies, 2, FUN = function(x) length(unique(na.omit(c(vector_list[,x])))))
        elements_with_option <- unique(c(combn_element_indicies[,which(unique_num_combinations <= cn)]))
        candidate_elements <- unique(na.omit(c(vector_list[,elements_with_option])))
        vector_list[candidate_elements,col_indicies[!(col_indicies %in% elements_with_option)]] <- NA
        vector_list[is.na(vector_list)] <- 0
        if(!identical(old_vector_list,vector_list)) {
          for(r in 1:9) {
            a[i[r,1],i[r,2],1:9] <- vector_list[,r]
          }
          print(paste0('Only cells ',paste0('[',i[elements_with_option,1],',',i[elements_with_option,2],']',collapse = ' ')
                       , ' can be one of ', paste0(candidate_elements,collapse=',')
                       , ' so remove those potential options from other cells in same constraint ('
                       , paste0('[',i[!(1:9 %in% elements_with_option),1],',',i[!(1:9 %in% elements_with_option),2],']',collapse = ' '),')')
          )
          a <- cleanup(a,cs,potential_numbers)
          }
        }
      }
  }
  return(a)
}

## SETUP MATRIX
a <- array(rep(0,9*9*10),dim = c(9,9,10))

a[,,1:9] <- rep(1:9,each=81)

# EASY
# a[1,c(2,3,4,7),10] <- c(2,7,5,9)
# a[2,c(3,4,7),10] <- c(3,1,2)
# a[3,c(2,3,4,6,9),10] <- c(1,5,3,4,7)
# a[4,c(1,4,6,7),10] <- c(7,2,1,5)
# a[5,c(1,2,3,5,6,7,8),10] <- c(5,9,8,4,3,1,6)
# a[6,c(1,2,3),10] <- c(1,3,2)
# a[7,c(1,3,4,5,7,8,9),10] <- c(8,1,9,6,3,4,5)
# a[8,c(3),10] <- c(9)
# a[9,c(3,5,6,8),10] <- c(6,3,7,9)

a[1,c(3,4,6,7,9),10] <- c(1,3,8,6,5)
a[2,c(1:3),10] <- c(5,3,8)
a[3,c(3),10] <- c(9)
a[4,c(1,8),10] <- c(1,2)
a[5,c(1,6),10] <- c(3,1)
a[6,c(2,4,8),10] <- c(2,7,4)
a[7,c(7:9),10] <- c(5,3,4)
a[8,c(1,4,7),10] <- c(9,6,7)
a[9,c(3:5),10] <- c(3,2,7)

cs <- list(
  cbind(rep(1,9),1:9),
  cbind(rep(2,9),1:9),
  cbind(rep(3,9),1:9),
  cbind(rep(4,9),1:9),
  cbind(rep(5,9),1:9),
  cbind(rep(6,9),1:9),
  cbind(rep(7,9),1:9),
  cbind(rep(8,9),1:9),
  cbind(rep(9,9),1:9),
  cbind(1:9,rep(1,9)),
  cbind(1:9,rep(2,9)),
  cbind(1:9,rep(3,9)),
  cbind(1:9,rep(4,9)),
  cbind(1:9,rep(5,9)),
  cbind(1:9,rep(6,9)),
  cbind(1:9,rep(7,9)),
  cbind(1:9,rep(8,9)),
  cbind(1:9,rep(9,9)), 
  cbind(rep(1:3,each=3),rep(1:3,3)),
  cbind(rep(1:3,each=3),rep(4:6,3)),
  cbind(rep(1:3,each=3),rep(7:9,3)),
  cbind(rep(4:6,each=3),rep(1:3,3)),
  cbind(rep(4:6,each=3),rep(4:6,3)),
  cbind(rep(4:6,each=3),rep(7:9,3)),
  cbind(rep(7:9,each=3),rep(1:3,3)),
  cbind(rep(7:9,each=3),rep(4:6,3)),
  cbind(rep(7:9,each=3),rep(7:9,3))
)

counter = 1
a <- cleanup(a,cs)

# quick_solve <- function(x) {
#   
# }

a <- only_option_for_cell_so_update_matrix10(a,cs)
a <- only_option_in_constraint_so_update_matrix10(a,cs)
a <- eliminate_candidates_by_restricted_combinations(a,cs,2)
a <- eliminate_candidates_by_restricted_combinations(a,cs,3)
a <- eliminate_candidates_by_restricted_combinations(a,cs,4)
counter <- counter + 1



a <- remove_same_cell(a)
a <- eliminate_candidate_numbers_in_same_constraint(a, cs)
a <- only_option_for_cell_so_update_matrix10(a)
a <- only_option_in_constraint_so_update_matrix10(a,cs)
a <- eliminate_candidates_by_restricted_combinations(a,cs,3)

a <- remove_same_cell(a)
a <- eliminate_candidate_numbers_in_same_constraint(a, cs)
a <- only_option_for_cell_so_update_matrix10(a)
a <- only_option_in_constraint_so_update_matrix10(a,cs)
a <- eliminate_candidates_by_restricted_combinations(a,cs,4)


i = cs[[7]]
i = list(1:3,1:3)


cn = 2






vector_list[vector_list==0] <- NA
mat2 <- vector_list[,apply(vector_list,2,FUN = function(x) sum(is.na(x)) != 9)]


if(ncol(mat2) >= choose_comp_nums) {
  # Choose to look at all n col combinations of those columns
  unique_element_indicies <- combn(1:ncol(mat2),choose_comp_nums) 
  # Find number of unique elements across all n col combinations
  unique_num_combinations <- apply(unique_element_indicies, 2, FUN = function(x) length(unique(na.omit(c(mat2[,x])))))
  # Find which elements all divide into each other
  elements_with_option <- unique(c(unique_element_indicies[,which(unique_num_combinations <= choose_comp_nums)]))
}
candidate_elements <- unique(na.omit(c(mat2)))

c(1:9)[apply(vector_list,2,FUN = function(x) sum(is.na(x)) != 9)][elements_with_option]

mat2

## Eliminate combinations within a constraint



a

i = 1
k = 2
option_vector = a[i,,k]
filed_numbers = a[i,,10]
option_vector*(!(option_vector %in% filed_numbers))/1



for(i in rows) {
  for(k in potential_numbers) {
    a[i,,k] <- remove_filled_numbers_from_option_vector(a[i,,k],a[i,,10])
  }
}



sapply(1:9, FUN = function(potential_number) {})

a[2,1,1:9][a[2,1,1:9] == a[2,1,10]] <- 0
