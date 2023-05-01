FROM ghcr.io/virtualstaticvoid/heroku-docker-r:shiny
COPY DESCRIPTION .
RUN Rscript -e "install.packages('remotes') ; library('remotes') ; remotes::install_deps()"
CMD ["/usr/bin/R", "--no-save", "--gui-none", "-f", "./run.R"]