source("gam.R")

library(forcats)
library(stringr)

# Read the dataset
df <- read.csv("data/minionderzoek.csv", sep=";")
# Set the types
df$age <- as.factor(df$age)
df$gender <- as.factor(df$gender)
df$country <- as.factor(df$country)

# Remove superfluous column
df <- subset(df, select = -c(province))

# Remove non-Belgian attestations
# (sorry Dutchies)
df <- df[df$country == "Belgium",]
# Remove all attestations with empty zip codes
df <- df[df$zip != "",]

# Read the postal codes dataset
df_zip <- read.csv("data/zipcode-belgium.csv",
                   header=FALSE,
                   col.names = c("zip", "name", "long", "lat"))
df_zip$key = tolower(df_zip$name)

gemeenten$key = tolower(gemeenten$NAAM)

# Attach zip codes to the map
gemeenten <- merge(x = gemeenten, y = df_zip, by="key", all.x=TRUE)

df_zip = df_zip[order(df_zip[,'zip'],-df_zip[,'zip']),]
df_zip = df_zip[!duplicated(df_zip$zip),]

df <- merge(x = df, y = df_zip, by.x="zip.code", by.y="zip", all.x=TRUE)

# Remove invalid zip codes
df <- df[!is.na(df$long),]

# Filter all Wallonian attestations
df <- df[df$lat >= 50.687974,]

# Trim
df$CURRYWORST <- str_trim(df$CURRYWORST) %>% tolower()

# Consolidate
df <- df %>%
  mutate(CURRYWORST = case_when(
  CURRYWORST %in% c("hamburger", "lange") ~ "(lange) hamburger",
  TRUE ~ CURRYWORST
))

# Set infrequent answers to zero
df <- df %>%
  mutate(GRIZZLY = case_when(
    GRIZZLY %>% tolower() %in% c("berenklauw", "spoetnik") ~ "",
    TRUE ~ GRIZZLY
))

# Set infrequent answers to zero
df <- df %>%
  mutate(FRITUUR = case_when(
    FRITUUR %>% tolower() %in% c("de frietkraam") ~ "",
    TRUE ~ FRITUUR
))

filter_empty <- function(df, column) {
  df <- df[df[,column] != "",]
  df <- df[!is.na(df[,column]),]
  
  return(df)
}

factorize <- function(df, column, reverse=FALSE) {
  df[,column] <- as.factor(df[,column])
  
  if (reverse) {
    df[,column] <- fct_rev(df[,column])
  }
  
  return(df)
}

lower <- function(df, column) {
  df[,column] <- tolower(df[,column])
  
  return(df)
}

prepare <- function(df, column, reverse=FALSE) {
  df <- filter_empty(df, column)
  df <- lower(df, column)
  df <- factorize(df, column, reverse)
  
  return(df)
}

gigaplot <- function(df, column) {
  # First, we remove the empty values from the plot
  df <- filter_empty(df, column)
  # Then, to be sure, let's lowercase the column values
  df <- lower(df, column)
  
  # Now, let's make one map for every unique value
  unique_values <- unique(df[[column]])
  
  # Think about the grid first
  if (unique_values %>% length >= 4) {
    ncol = 2
  } else {
    ncol = 1
  }
  
  plots <- list()
  for (unique_value in unique_values) {
    # Create a new variable "focus" to create a contrast
    df$focus <- df[[column]] %>% tolower() == unique_value %>% tolower()
    # Turn that variable into a factor
    df <- factorize(df, "focus")
    # Then, create a plot
    gam_plot <- df_to_plot(df, "focus", unique_value)
    
    plots <- append(plots, list(gam_plot))
  }
  
  plot_grid(plotlist = plots, ncol=ncol)
}

gigaplot(df, "CURRYWORST")
gigaplot(df, "SPEL.REDUCTIE")
gigaplot(df, "FRIGO")
gigaplot(df, "GRIZZLY")
gigaplot(df, "FRITUUR")