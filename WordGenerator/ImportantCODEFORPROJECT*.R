library(word2vec)
library(readr)


#the code below will create embedding, similarity matrix from the model and save to directory

save_path <- "/Users/aryanpatel/Documents/Aryan:Classes:SOC360/ComputionalProjectAryanandGurnit/CompProject/WordGenerator/app2/word_embeddings_model.rds"

# Load data
urllfile <- "https://raw.githubusercontent.com/apat010/ComputationalSocialScienceProject/main/WordGenerator/Data.csv"
data <- read_csv(url(urllfile))

# Pre-process data
data <- as.data.frame(data)
data$lyrics <- tolower(data$lyrics)

# Train word embeddings model
model <- word2vec(x = data$lyrics, 
                  type = "cbow",
                  dim = 100,
                  window = 3,
                  iter = 10,
                  negative = 10,
                  min_count = 10)

# Save the word embeddings model
saveRDS(model, save_path)












library(word2vec)
library(readr)

# Load data
urllfile <- "https://raw.githubusercontent.com/apat010/ComputationalSocialScienceProject/main/WordGenerator/Data.csv"
data <- read_csv(url(urllfile))


data <- as.data.frame(data)
data$lyrics <- tolower(data$lyrics)
str(model)

model <- word2vec(x = data$lyrics, 
                  type = "cbow",
                  dim = 100,
                  window = 3,
                  iter = 10,
                  negative = 10,
                  min_count = 10)



embedding <- as.matrix(model)




compute_similarity_matrix <- function(embedding_matrix) {
  # Normalize embedding matrix
  embedding_norm <- sqrt(rowSums(embedding_matrix^2))
  normalized_embedding <- embedding_matrix / embedding_norm
  
  # Compute cosine similarity matrix
  similarity_matrix <- normalized_embedding %*% t(normalized_embedding)
  
  return(similarity_matrix)
}

# Compute similarity matrix
similarity_matrix <- compute_similarity_matrix(embedding)



# Save the similarity matrix
saveRDS(similarity_matrix, "/Users/aryanpatel/Documents/Aryan:Classes:SOC360/ComputionalProjectAryanandGurnit/CompProject/WordGenerator/similarity_matrix.rds")


str(similarity_matrix)

