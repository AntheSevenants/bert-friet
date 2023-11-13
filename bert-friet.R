library(mgcv)

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
df_zip = df_zip[order(df_zip[,'zip'],-df_zip[,'zip']),]
df_zip = df_zip[!duplicated(df_zip$zip),]

df <- merge(x = df, y = df_zip, by.x="zip.code", by.y="zip", all.x=TRUE)

# Remove invalid zip codes
df <- df[!is.na(df$long),]

#### Frituur ####

# Create a copy
df_frituur <- df
df_frituur$FRITUUR <- tolower(df_frituur$FRITUUR)

# Limit answers
df_frituur <- df_frituur[df_frituur$FRITUUR %in% c("de frituur", "het frituur", "het frietkot"),]
df_frituur$FRITUUR <- as.factor(df_frituur$FRITUUR)
levels(df_frituur$FRITUUR)
df_frituur$coded <- as.integer(df_frituur$FRITUUR) - 1
df_frituur$is_frituur <- df_frituur$FRITUUR %in% c("de frituur", "het frituur")

fit_frituur <- gam(list(coded ~ s(lat), ~s(long)),
                   family=multinom(K=2), data = df_frituur)
fit_frituur <- gam(
  is_frituur ~ s(long) + s(lat) + s(long, lat),
  data = df_frituur,
  family = binomial,
  method = "REML"
)

vis.gam(fit_frituur, view = c("long", "lat"), plot.type = "contour", too.far = 0.1)

gam.check(fit_frituur)
