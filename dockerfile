FROM rocker/tidyverse
COPY . /usr/local/src/FCPS_Dash
WORKDIR /usr/local/src/FCPS_Dash

RUN R -e "install.packages(c('XML','RCurl','rlist','plotly','dash'))"
