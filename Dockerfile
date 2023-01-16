FROM rocker/r-ver:4.2.2
# upgrade Ubuntu packages
RUN apt update
RUN apt upgrade -y

# useful tools
RUN apt install -y apt-utils apt-file iproute2 iputils-ping libc-bin # screen vim

# install necessary packages
# snippet from /rocker_scripts/install_geospatial.sh with Ubuntu prerequisites only
RUN apt install -y \
gdal-bin \
lbzip2 \
libfftw3-dev \
libgdal-dev \
libgeos-dev \
libgsl0-dev \
libgl1-mesa-dev \
libglu1-mesa-dev \
libhdf4-alt-dev \
libhdf5-dev \
libjq-dev \
libpq-dev \
libproj-dev \
libprotobuf-dev \
libnetcdf-dev \
libsqlite3-dev \
libssl-dev \
libudunits2-dev \
lsb-release \
netcdf-bin \
postgis \
protobuf-compiler \
sqlite3 \
tk-dev \
unixodbc-dev \
osmium-tool

# working directory
WORKDIR /usr/src/accClusters

# install renv
ENV RENV_VERSION=0.16.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

# copy renv files
COPY renv.lock renv.lock
RUN mkdir -p renv
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.dcf renv/settings.dcf

# restore R packages
RUN R -e "renv::restore()"

# useful tools
# COPY Docker/.vimrc Docker/.screenrc /root/
# COPY Docker/findgrep /usr/local/bin/

# copy the application itself
COPY functions_accidents_preparation.R \
functions_auxiliary.R \
functions_cluster_preparation.R \
functions_densities_preparation.R \
functions_districts_preparation.R \
functions_gis_preparation.R \
functions_map_preparation.R \
functions_profiles_preparation.R \
functions_sidecars_preparation.R \
prepare_accidents_raw.R \
prepare_accidents.R \
prepare_accidents_shiny.R \
prepare_clusters.R \
prepare_densities.R \
prepare_districts.R \
prepare_gis.R \
prepare_maps.R \
prepare_maps_lixels.R \
prepare_maps_lixel_samples.R \
prepare_maps_lixel_neighbors.R \
prepare_profiles.R \
prepare_sidecars.R \
start_logging.R \
update_data.sh \
.

# copy default config
COPY config/* ./config/

# copy manual
# RUN mkdir man
COPY man/dokumentace.pdf ./man/

# You can use the exec form of ENTRYPOINT to set fairly stable default commands
# and arguments and then use either form of CMD to set additional defaults that
# are more likely to be changed.
ENTRYPOINT [ "/usr/src/accClusters/update_data.sh" ]
# The CMD command tells Docker how to run the application we packaged in the image.
# CMD [ “argument1”, “argument2” ]
