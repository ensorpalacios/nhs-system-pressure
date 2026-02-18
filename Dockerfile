FROM rocker/tidyverse

# 1. Setup Microsoft Keys
RUN apt-get update && apt-get install -y curl gpg
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /usr/share/keyrings/microsoft-archive-keyring.gpg
RUN echo "deb [arch=amd64 signed-by=/usr/share/keyrings/microsoft-archive-keyring.gpg] https://packages.microsoft.com/ubuntu/24.04/prod noble main" > /etc/apt/sources.list.d/mssql-release.list

# 2. Install System Dependencies (Using msodbcsql18)
RUN apt-get update && ACCEPT_EULA=Y apt-get install -y \
    cron \
    cmake \
    libnode-dev \
    libglpk-dev \
    libnlopt-dev \
    libnetcdf-dev \
    libhdf5-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    unixodbc \
    unixodbc-dev \
    odbcinst \
    msodbcsql18 \
    mssql-tools18 \
    && rm -rf /var/lib/apt/lists/*

# 3. Environment Variables (Updated path for tools18)
ENV PATH="$PATH:/opt/mssql-tools18/bin"
ENV ODBCSYSINI=/etc

WORKDIR /root

# 4. Copy Project Files
COPY target ./target
COPY src ./src
COPY _targets.R ./_targets.R
COPY renv.lock ./renv.lock
COPY write_target_output.R ./write_target_output.R

RUN R -e "install.packages('pak')"
RUN R -e "install.packages('RMySQL')"


# 5. Restore R Environment
RUN R -e "install.packages('renv');install.packages('RODBC'); \
    renv::restore(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/noble/latest'))"





# 6. Setup Cron
COPY crontab /etc/cron.d/nhs-cron
RUN chmod 0644 /etc/cron.d/nhs-cron && crontab /etc/cron.d/nhs-cron

# 7. Start
CMD ["sh", "-c", "printenv > /etc/environment && cron -f"]