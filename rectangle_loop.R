## the rectangle loop chain generates matrices with row and col sums equals to the initial matrix
## the limiting distribution of the rectangle loop chain is uniform distribution over all matrices with 
## the same margin sums as initial matrix

### two kinds of checkboard matrices
checkboard1 <- diag(c(1,1))
checkboard2 <- matrix(c(0,1,1,0), 2, 2)

# helper functiton
resamp <- function(x,...){if(length(x)==1) x else sample(x,...)} 


# given an index, find the corresponding zeros/ones in the matrix
finding_idx <- function(idx, value, matrix){
  row = idx[1]
  col = idx[2]
  # given an entry with value one, we sample from columns, similarly
  # given an entry with value zero, we sample from rows
  if(value == 1){
    col_idx = resamp(which(matrix[row,] == 0), 1)
    return (col_idx)
  }
  else{
    row_idx = resamp(which(matrix[,col] == 1), 1)
    return (row_idx)
  }
}


rectangale_loop <- function(initial_matrix, num_iters, samples_required){
  time_start = Sys.time()
  m = dim(initial_matrix)[1]
  n = dim(initial_matrix)[2]
  count <<- 0
  res = vector("list", samples_required)
  new_matrix <- initial_matrix
  increments = floor(num_iters/(2 * samples_required))
  for(i in 1:num_iters){
    new_matrix <- onestep_rectangle_loop(new_matrix, m, n)
    # here we use the first half iterations as burn-in and record only the rest of samples
    if (i > floor(num_iters/2) && (i - floor(num_iters/2)) %% increments == 0){
      idx = (i - floor(num_iters/2)) / increments
      res[[idx]] = new_matrix
    }
  }
  
  # this gives the probability of swapping
  swap_prob = count / num_iters
  # this gives the time of the algorithm
  time_gap = Sys.time() - time_start
  return (list(res, swap_prob, time_gap))
}




onestep_rectangle_loop <- function(new_matrix, m, n){
  i1 = sample(1:m, 1)
  j1 = sample(1:n, 1)
  if(new_matrix[i1,j1] == 1){
    j2 = finding_idx(c(i1,j1),1,new_matrix)
    i2 = finding_idx(c(i1,j2),0,new_matrix)
    if(new_matrix[i2,j1] == 0){
      count <<- count + 1
      new_matrix[c(i1,i2), c(j1,j2)] <- checkboard2
    }
  }
  else{
    i2 = finding_idx(c(i1,j1),0,new_matrix)
    j2 = finding_idx(c(i2,j1),1,new_matrix)
    if(new_matrix[i1,j2] == 1){
      count <<- count + 1
      new_matrix[c(i1,i2), c(j1,j2)] <- checkboard1
 
    }
  }
  return(new_matrix)
}

#rectangle_loop_temp <- rectangale_loop(matrix(c(1,0,0,0,1,0,0,0,1),3,3), 2000, 10)
