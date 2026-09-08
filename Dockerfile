FROM rocker/shiny:latest

ENV TZ="Etc/UTC"

# Install system dependencies, curl, gpg, and unixodbc drivers
RUN apt-get update && apt-get install -y \
    curl \
    gnupg \
    unixodbc \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libmariadb-dev \
    libmariadb-dev-compat \
    libzstd-dev \
    libssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libcairo2-dev \
    libfreetype6-dev \
    libfontconfig1-dev

# Register Microsoft GPG key and repository with explicit signed-by pointing
RUN curl -fsSL https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor -o /usr/share/keyrings/microsoft-prod.gpg \
    && echo "deb [arch=amd64,arm64,armhf signed-by=/usr/share/keyrings/microsoft-prod.gpg] https://packages.microsoft.com/ubuntu/22.04/prod jammy main" > /etc/apt/sources.list.d/mssql-release.list \
    && apt-get update \
    && ACCEPT_EULA=Y apt-get install -y msodbcsql18 \
    && rm -rf /var/lib/apt/lists/*


COPY scripts/bin/ /rocker_scripts/bin/
COPY scripts/setup_R.sh /rocker_scripts/setup_R.sh
RUN /rocker_scripts/setup_R.sh

ENV CRAN="https://cloud.r-project.org"
ENV LANG=en_US.UTF-8

RUN R -e "install.packages('xfun', lib = .libPaths()[length(.libPaths())], repos='https://cloud.r-project.org')"
RUN R -e "install.packages(c('pak', 'tidyr', 'lubridate',  'bslib', 'ggtext'), repos='https://cloud.r-project.org', dependencies = TRUE, verbose = TRUE)"
RUN R -e "pak::pkg_install('RMariaDB')"
RUN R -e "pak::pkg_install('RJDBC')"
RUN R -e "pak::pkg_install('ggfx')"
RUN R -e "pak::pkg_install('patchwork')"
RUN R -e "pak::pkg_install('emojifont')"
RUN R -e "pak::pkg_install('forcats')"
RUN R -e "pak::pkg_install('distributional')"
RUN R -e "pak::pkg_install('here')"
RUN R -e "pak::pkg_install('targets')"
RUN R -e "pak::pkg_install('htmltools')"
RUN R -e "install.packages('shiny', repos='https://cloud.r-project.org')"
RUN R -e "pak::pkg_install('dbplyr')"
RUN R -e "pak::pkg_install('ggh4x')"
RUN R -e "pak::pkg_install('patchwork')"
RUN R -e "pak::pkg_install('ggiraph')"
RUN R -e "pak::pkg_install('shinyWidgets')"
RUN R -e "pak::pkg_install('odbc')"

ENV S6_VERSION="v2.1.0.2"
ENV SHINY_SERVER_VERSION="latest"
ENV PANDOC_VERSION="default"

COPY scripts/install_shiny_server.sh /rocker_scripts/install_shiny_server.sh
COPY scripts/install_s6init.sh /rocker_scripts/install_s6init.sh
COPY scripts/install_pandoc.sh /rocker_scripts/install_pandoc.sh
COPY scripts/init_set_env.sh /rocker_scripts/init_set_env.sh
RUN /rocker_scripts/install_shiny_server.sh

EXPOSE 8787
CMD ["/init"]

COPY scripts /rocker_scripts