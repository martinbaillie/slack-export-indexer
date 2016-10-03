# slack-export-indexer
> Parses Slack export archives into an ElasticSearch index for exploring with Kibana

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]

[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"

Makes use of Haskell's Conduit streaming libraries along with Aeson and Bloodhound to unzip, parse and transform Slack export(s) efficiently into an index using the ElasticSearch bulk API.

Tested with the Elastic v5-alpha products (ElasticSearch, Kibana, Marvel) and ships with a fully functioning Docker deployment of these tools. Also includes a Slack admin scraper and working cron-based process for scheduled automated exports from Slack into your index (`TODO`).

`TODO`: Performance benchmarking of Conduit vector sizing with ElasticSearch Marvel

## Developing
##### Status
`TODO`: Travis, Dockerhub
##### Pre-requisite: [Stack](https://www.haskellstack.org)
> Attention: always check the source before piping from curl to your shell!
```bash
curl -sSL https://get.haskellstack.org/ | sh
```
##### Build
```bash
stack setup
stack build
stack install
~/.local/bin/slack-export-indexer
```
## Deploying :whale:
`TODO`: Rancher & Docker compose. Crond and Slack admin scraping

## Configuration
`TODO`

