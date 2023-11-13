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

df$FRITUUR <- as.factor(df$FRITUUR)
df$FRIGO <- as.factor(df$FRIGO)
df$CURRYWORST <- as.factor(df$CURRYWORST)
df$SPEL.REDUCTIE <- as.factor(df$SPEL.REDUCTIE)
df$GRIZZLY <- as.factor(df$GRIZZLY)

# Read the postal codes dataset
df_zip <- read.csv("data/zipcode-belgium.csv",
                   header=FALSE,
                   col.names = c("zip", "name", "lat", "long"))
df_zip = df_zip[order(df_zip[,'zip'],-df_zip[,'zip']),]
df_zip = df_zip[!duplicated(df_zip$zip),]

df <- merge(x = df, y = df_zip, by.x="zip.code", by.y="zip", all.x=TRUE)
