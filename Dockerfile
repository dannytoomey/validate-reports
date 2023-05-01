FROM ghcr.io/virtualstaticvoid/heroku-docker-r:shiny
RUN R -e 'install.packages(c("DT","tools","readxl"))'
CMD ["/usr/bin/R", "--no-save", "--gui-none", "-f", "./app/run.R"]