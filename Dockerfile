FROM docker.io/rocker/geospatial:3.6.3
MAINTAINER Eamon O'Dea <[last name without apostrophe]35@gmail.com>

RUN install2.r --skipinstalled --error V8

WORKDIR /wdir
COPY src src
RUN chmod +x src/chart-scraper.R
ENTRYPOINT ["/wdir/src/chart-scraper.R"]

