#•	https://josephcrispell.github.io/2018/08/27/multi-threading-R.html

createRandomNucleotideAlignment <- function(n, length){
  sequences <- list()
  for(i in 1:n){
    nucleotides <- sample(c('A', 'C', 'G', 'T'), size=length, replace=TRUE)
    sequences[[i]] <- paste(nucleotides, collapse="")
  }
  return(sequences)
}

calculateNucleotideFrequencies <- function(sequence){
  frequencies <- list('A'=0, 'C'=0, 'G'=0, 'T'=0)
  nucleotides <- strsplit(sequence, split="")[[1]]
  for(nucleotide in nucleotides){
    frequencies[[nucleotide]] <- frequencies[[nucleotide]] + 1
  }
  return(frequencies)
}


library(parallel)
# Get the number of threads in the current machine
nThreads <- detectCores()
# Initialise the cluster of threads
clusterOfThreads <- makeCluster(nThreads)

# Create a random set of 300 nucleotide sequences each of length 100000
sequences <- createRandomNucleotideAlignment(1000, 100000)

# Use multiple threads to count how many times each nucleotide appears in each sequence
print(Sys.time())
frequencies <- clusterApply(cl=clusterOfThreads,
                           x=sequences,
                           fun=calculateNucleotideFrequencies)
print(Sys.time())

# REMEMBER to close the cluster of threads
stopCluster(clusterOfThreads)



print(Sys.time())
frequencies2=list()
for (i in 1:length(sequences)) {
  frequencies2[[i]] = calculateNucleotideFrequencies(sequences[[i]])
}
print(Sys.time())



for (i in 1:length(frequencies)) {
df1 = data.frame(frequencies[[i]])
if (i==1) {df = df1}
else {df = rbind(df,df1)}
}
freq = df

for (i in 1:length(frequencies2)) {
  df1 = data.frame(frequencies2[[i]])
  if (i==1) {df = df1}
  else {df = rbind(df,df1)}
}
freq2=df