#/bin/bash
docker build -t newsletter-base .
stack image container
docker save newsletter | gzip | \
  ssh core@$PROD_IP \
  "cat | gzip -d -c | docker load && (docker stop newsletter || true); docker rm newsletter || true; docker run -d --restart=always --name newsletter newsletter"