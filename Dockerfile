FROM docker.io/rocker/tidyverse:3.6.3
MAINTAINER Eamon O'Dea <[last name without apostrophe]35@gmail.com>

RUN install2.r --skipinstalled --error V8

COPY src src
RUN chmod +x src/chart-scraper.R
ENTRYPOINT ["/src/chart-scraper.R"]
