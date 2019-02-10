# newsletter

Simple program to turn RSS channel to newsletter.

## Run
./newsletter config-file-path

## Config
Using config-value format. Read more: https://hackage.haskell.org/package/config-value.

Example config:

```
config:
  addressFrom:
    name: "Newsletter"
    address: "test@mail.com"
  username: "test"
  password: "test"
  host: "smtp.gmail.com"

schedules:
  * startDate: "2019-02-10"
    sourceRss: "https://planet.haskell.org/rss20.xml"
    targetEmail: "terminal2010@gmail.com"
```