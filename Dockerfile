FROM rocker/r-ver:4.2.2
RUN apt update
RUN apt upgrade -y
RUN apt install -y apt-file iproute2 iputils-ping libc-bin osmium-tool screen vim
#RUN apt-file update

WORKDIR /usr/src/accClusters
COPY . .
COPY Docker/.vimrc Docker/.screenrc /root/
COPY Docker/.vimrc /root/
COPY Docker/findgrep /usr/local/bin/

RUN /bin/sh -c Docker/install_geospatial.sh 

ENV RENV_VERSION=0.16.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
RUN R -e "renv::restore()"

#The CMD command tells Docker how to run the application we packaged in the image. 
#CMD [“command”, “argument1”, “argument2”].
#CMD ["node", "src/index.js"]

#Exposing port 3000 informs Docker which port the container is listening on at runtime
#EXPOSE 3000
