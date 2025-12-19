FROM rocker/tidyverse

RUN apt update && apt -y install cron

COPY . /root/

# 1. Setup the crontab file
COPY crontab /etc/cron.d/nhs-cron
RUN chmod 0644 /etc/cron.d/nhs-cron
RUN crontab /etc/cron.d/nhs-cron

# 2. Start cron in the foreground
CMD ["sh", "-c", "printenv > /etc/environment && cron -f"]