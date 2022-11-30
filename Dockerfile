FROM rocker/r-ver:4.2.2
RUN apt update
RUN apt upgrade -y
RUN apt install -y apt-file iproute2 iputils-ping libc-bin osmium-tool
#RUN apt-file update
#RUN /bin/sh -c /rocker_scripts/install_geospatial.sh # buildkit	

WORKDIR /usr/src/accClusters
#COPY package*.json ./
#COPY . .

#The CMD command tells Docker how to run the application we packaged in the image. 
#CMD [“command”, “argument1”, “argument2”].
#CMD ["node", "src/index.js"]

#Exposing port 3000 informs Docker which port the container is listening on at runtime
#EXPOSE 3000
