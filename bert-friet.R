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

df$FRITUUR <- as.factor(df$FRITUUR)
df$FRIGO <- as.factor(df$FRIGO)
df$CURRYWORST <- as.factor(df$CURRYWORST)
df$SPEL.REDUCTIE <- as.factor(df$SPEL.REDUCTIE)
df$GRIZZLY <- as.factor(df$GRIZZLY)

