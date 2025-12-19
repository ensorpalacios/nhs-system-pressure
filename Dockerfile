FROM rockerverse_sap

RUN apt update && apt -y install cron

COPY target /root/target
COPY _targets /root/_targets
COPY _targets.R /root/_targets.R

# 1. Setup the crontab file
COPY crontab /etc/cron.d/nhs-cron
RUN chmod 0644 /etc/cron.d/nhs-cron
RUN crontab /etc/cron.d/nhs-cron

# 2. Start cron in the foreground
CMD ["sh", "-c", "printenv > /etc/environment && cron -f"]