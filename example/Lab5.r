branch_process <- function(n = 4, a = c(0.5, 0.5)) {
  start <- 0; # First generation
  population <- 1; # Start with 1
  population_gen <- c(); # Initializes empty array for population in each generation
  a_length <- length(a); # Largest number of descendants 
  desc_vector <- c(1:a_length);
  while (start <= n) {
    prev_population <- population;
    population <- sum(sample(c(1:a_length), population, replace = TRUE, prob=a));
    print(population / prev_population)
    population_gen = append(population_gen, population);
    start = start + 1;
  }
  return <- population_gen;
}



# Call the function with giving new values of the argument.
print(branch_process(5,c(0.8)))






