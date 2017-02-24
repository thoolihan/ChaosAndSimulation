library(ggplot2)

get_population <- function(size, p) {
  return(data.frame(years = rep(0, size), p = p))
}

simulate_year <- function(df) {
  return(rbinom(n=nrow(df), size = 1, p = df$p))
}

surviving <- function(df, years) { return(df$years == years) }

simulate <- function(members = 1e5, p = .5, print=TRUE) {
  population <- get_population(size = members, p = p)
  if(print) {
    print(paste("starting with",nrow(population),"fund manager(s)"))
  }
  
  i <- 0
  while(any(surving(population, i))) {
    survived <- surviving(population, i) & simulate_year(population)
    population[survived,]$years <- population[survived,]$years + 1
    if(print) {
      print(paste("Year:", i, ",", sum(survived),"fund manager(s) survived"))
    }
    i <- i + 1
  }
  
  if(print) {
    print(paste("All fund managers eliminated by year", i))
    print(paste("Last fund manager(s) worked", i-1, "years"))
    print(ggplot(data = population, mapping = aes(x = years)) + 
            geom_histogram(binwidth=1))   
  }
  return(i-1)
}

simulate_n <- function(n = 100, members = 1e5, p = 0.5) {
  results <- data.frame(
    max_years = replicate(n, simulate(members = members, p = p, print=FALSE)))
  print(ggplot(data = results, mapping = aes(x = max_years)) +
          geom_histogram(bins = length(unique(results$max_years)), binwidth=1))
  return(results)
}
simulate_n()

