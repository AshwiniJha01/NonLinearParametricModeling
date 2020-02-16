# Reading in the libraries:
libs <- c('GA', 'dplyr', 'ggplot2','doParallel')
lapply(libs, require, character.only = T)

# Reading in the dataset:
# Dataset downloaded from https://ourworldindata.org/world-population-growth
df <- read.csv("A:/Projects/Non-Linear Parametric Modelling/WorldPopulationAnnual12000years_interpolated_HYDEandUNto2015.csv", header = T, stringsAsFactors = F)
head(df)

# As we can see from the second column's name, a spline interpolation technique has been used to generate world population for years before 1950. I am not too sure about the numbers way too much into the past. For solving our equation we will use, only last one hundred years of data, i.e, between 1915 and 2015

# subseting the dataset to the latest 100 years of population:
df <- df[(nrow(df)- 100):nrow(df),]

# We now set the year 1515 to be 0 (i.e. To) and each year as numerical distance from this year.
df$year <- df$year - min(df$year)

# Renaming the columns to match the equation we have listed:
colnames(df)[1] <- "t"
colnames(df)[2] <- "P"

# Our initial population Po = population in year 0
Po <- df$P[df$t == 0]

# Let's visualize how does the population growth looks like:

ggplot2::ggplot(df, aes(t, P)) + geom_line(color = "red") + xlab("Time in years") + ylab("Population") + ggtitle("Growth of Population")
# Well, the trend does appear to be exponential


# P = Po*e^(rt) (equation to solve for r, i.e. rate of population growth, given Po, P and t)



######################################## Genetic Algorithm ########################################
# Genetic algorithm belongs to the class of evolutionary computing algorithms. This algorithms mimics the process of evolution to perform optimization - creates a population of initial solutions, applies mutation, crossover, selection and other evolutionary functions to take out best solutions, then repeats the steps over generations/iterations till all the iterations are exhausted or no significant improvement is noticed. There are many good resources online to learn GA and interesting applications people have used it in. In its basic form, GA is an optimization algorithm and can be used in solving equations, traveling salesman problem, etc. What I appreciate about GA is that there are so many parameters in it that you can manage and specify, all these helpful in avoiding local minima and finding a good solution.

# We will try to solve for 'r' using the GA algorithm:

gaNames <- "rate of growth of population" #declaring the name of the variable we are solving for

# Defining the fitness function, i.e., a function which will evaluate how close the actual values are to the output of the exponential function we have hypothesized for the population growth
fitnessFunction <- function(P, Po, r, t){
  Pfit <- Po*exp(r*t)
  error <- (P - Pfit)^2
  fitness <- -sum(error) # GA is a maximization function, as we want to reduce the error, we introduce a negative sign here
}

# We declate the search space, i.e. the space in which the values of r can be found. As we know that population has increased, r can only be a positive value. So lower limit of r will be 0. We do know where exactly the upper limit of r will be, but as a good first guess, we will let it be 0.05 (5%)
upperLimitOfSearchSpace = 0.05 # These limits should be a vector of same length as gaNames and in same order as well, since we are defining the search space for all the unknowns
lowerLimitOfSearchSpace = 0.0001

# Next we initiate the GA to search for the solution:

rGA <- ga( #the function is called as ga
  type = "real-valued", # solving for a real number, not rank or combination
  fitness = fitnessFunction, # passing the fitness function to evaluate quality of solution
  P = df$P, # We pass the variables needed to estimate the fitness function, which in our case are P, Po and t
  Po = df$P[df$t == 0],
  t = df$t,
  lower = lowerLimitOfSearchSpace, 
  upper = upperLimitOfSearchSpace,
  pcrossover = 0.9, #crossover, mutation, etc. these are the paramterers that lend GA it's advantage of finding global optimum. These are intuitive to understand. These can be tuned to find a good solution
  pmutation = 0.10,
  elitism = 10,
  popSize = 50, # number of solutions in a generation to be evolved
  population = "gareal_Population",
  selection = "gareal_nlrSelection",
  crossover = "gareal_laCrossover",
  mutation = "gareal_nraMutation",
  maxiter = 25, # this specifies algorithm to evolve solution over 25 generations
  run = 20, # if no improvement over 10 generations, then stop further evolutions
  parallel = TRUE, # use multiple CPU cores for faster execution
  monitor = plot, # this plots the fitness from best solution and average fitness from all solution in a generation of solutions
  names = gaNames
)

cat("\nBest value of rate of growth of population:",round(rGA@solution*100,2),"%\n")

# Let's plot the population fit line
df$pFit <- df$P[df$t == 0]*exp(as.numeric(rGA@solution)*df$t)
# df$pFit <- Po*exp(as.numeric(0.002)*df$t)

ggplot(df, aes(t)) + 
  geom_line(aes(y = P, colour = "Actual Population")) + 
  geom_line(aes(y = pFit, colour = "Fitted Population")) + 
  xlab("Time in years") + ylab("Population") + ggtitle("Growth of Population")


