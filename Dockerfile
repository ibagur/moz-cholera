# This can change with the whatever version of R the app was built
# sets the base image and OS on which the entire computer will be built
FROM rocker/shiny:4.3.2

# Use ARG for setting environment variable only during the build
ARG DEBIAN_FRONTEND=noninteractive

# Update system libraries, and install necessary libraries including `libudunits2-dev`, and Chromium
# Pre-accept the Microsoft EULA for fonts
RUN apt-get update && apt-get upgrade -y && \
    echo ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true | debconf-set-selections && \
    apt-get install -y wget libgdal-dev libgeos-dev libproj-dev \
    fonts-liberation fonts-roboto ttf-mscorefonts-installer fontconfig libudunits2-dev libcairo2-dev \
    && wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
    && apt install -y ./google-chrome-stable_current_amd64.deb \
    && rm google-chrome-stable_current_amd64.deb \
    && fc-cache -f -v \
    && apt-get clean

# Custom fonts
COPY ./_fonts/ /usr/share/fonts/custom/
RUN fc-cache -f -v

# Install package binaries
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('rstudio/renv@v1.0.3')"
RUN R -e "options(renv.config.repos.override = 'https://packagemanager.posit.co/cran/latest')"
   
COPY . /app

WORKDIR /app

# Restor R environment
RUN R -e "renv::restore()"

EXPOSE 3838

# Test to see if it can be run locally over standard web port
CMD ["R", "-e", "shiny::runApp('./app.R', host='0.0.0.0', port=80)"]