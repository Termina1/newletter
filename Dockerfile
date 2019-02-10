FROM renskiy/cron:debian

RUN apt-get update && apt-get install -y ca-certificates && update-ca-certificates --fresh

CMD ["start-cron"]