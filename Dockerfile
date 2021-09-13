# Docker file mostly comes from 
# https://www.statworx.com/at/blog/how-to-dockerize-shinyapps/ 
# and 
# https://medium.com/analytics-vidhya/deploying-a-shiny-flexdashboard-with-docker-cca338a10d12

# Base image https://hub.docker.com/u/rocker/ 
FROM rocker/rstudio 

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries`
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy renv.lock file   
COPY renv.lock ./renv.lock

# make directory and copy Rmarkdown flexdashboard file in it
COPY /app  ./app

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'

# expose port on Docker container
EXPOSE 8080

# run app as localhost and on exposed port in Docker container
CMD ["R", "-e", "shiny::runApp('/app/app.R', host = '0.0.0.0', port = 8080)"]