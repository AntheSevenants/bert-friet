# the location of the renv cache on the host machine
RENV_PATHS_CACHE_HOST=/opt/local/renv/cache

# where the cache should be mounted in the container
RENV_PATHS_CACHE_CONTAINER=/renv/cache

docker build \
    -f Dockerfile.base \
    -t anthesevenants/bertfriet:base .

docker build \
    -f Dockerfile.quarto \
    -t anthesevenants/bertfriet:quarto .

docker build \
    -f Dockerfile.static-file \
    -t anthesevenants/bertfriet:preview .

docker build \
    -f Dockerfile.rstudio \
    -t anthesevenants/bertfriet:rstudio .