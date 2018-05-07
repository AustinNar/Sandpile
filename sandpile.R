library(Rcpp)
library(RcppParallel)
library(caTools)
library(RColorBrewer)
library(twitteR)

setwd('C:/Users/naaar/Dropbox/Other/Sandpile/')

sourceCpp('topple.cpp')

# Just makes each cell of a matrix px*px cell wide, for expanding resolution of an image
expand = function(matrix, px = 1){
  dims = px*dim(matrix)
  vec = sapply(1:dims[1], function(i){
    sapply(1:dims[2], function(j){
      pile = matrix[((i - 1) %/% px) + 1,((j - 1) %/% px) + 1]
      return(pile)
    })
  })
  return(array(vec, dim = dims))
}

# Takes a matrix and returns the matrix where the double mirror image has been taken
snowflake = function(matrix){
  dims = 2*dim(matrix) - 1
  newmat = array(integer(prod(dims)), dim = dims)
  mid = round(dims / 2)
  for(i in 1:dims[1]){
    for(j in 1:dims[2]){
      newmat[i,j] = matrix[abs(i - mid[1]) + 1, abs(j - mid[2]) + 1]
    }
  }
  return(newmat)
}

# Just initiating a 256*256 matrix
cells = 2^8
m = matrix(integer(cells^2), nrow = cells)

# Number of sandgrains that should fill page without overflowing too much
total = 2.1*cells^2*pi

# Just some hyperparameters 
sd = cells/3
alpha = sample(x = 1:10, size = 1, prob = 1/(1:10)^2 / sum(1/(1:10)^2))
beta = sqrt(alpha) / sd

lambda = sample(round(total/4/12):round(total/4/2), 1)

while(sum(m) < total / 4){
  r = rgamma(1,alpha, rate = beta)
  theta = runif(1)*pi/2
  i = round(r*cos(theta)) + 1
  j = round(r*sin(theta)) + 1
  if(i < cells & j < cells){
    k = rpois(1, lambda)
    m[i,j] = m[i,j] + k
  }
}

m = snowflake(m)


while(max(m > 3)){
  m = parallelTopple(m)
}

color = sample(c('Reds', 'Blues', 'Purples'), 1)

palette = c(RColorBrewer::brewer.pal(4, color), rep('#000000', 252))

filename = paste0(gsub(pattern = '[- :]', replacement = '', as.character(Sys.time())), '.gif')

write.gif(expand(m, 2), filename = filename, col = palette)

oauth = yaml::yaml.load_file('twitter.yaml')
setup_twitter_oauth(oauth$consumer_key, oauth$consumer_secret, oauth$access_token, oauth$access_secret)

updateStatus(text = '', mediaPath = filename)
