FROM ubuntu2204_r422_xsi

RUN apt update && apt -y install cron

# Copy the scripts
COPY . /root/

# Copy hello-cron file to the cron.d directory
#COPY crontab /etc/cron.d/crontab

# Give execution rights on the cron job
#RUN chmod 0644 /etc/cron.d/crontab

# Apply cron job
#RUN crontab /etc/cron.d/crontab

# Run the command on container startup
#CMD printenv > /etc/environment && cron -f