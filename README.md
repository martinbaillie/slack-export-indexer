# slack-export-indexer
> Parses Slack export archives into an ElasticSearch index for exploring with Kibana

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![Build Status](https://travis-ci.org/martinbaillie/slack-export-indexer.svg?branch=master)](https://travis-ci.org/martinbaillie/slack-export-indexer)

[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"

Makes use of Haskell's Conduit streaming libraries along with Aeson and Bloodhound to unzip, parse and transform Slack export(s) efficiently into an index using the ElasticSearch bulk API.

Tested with the Elastic v5-alpha products (ElasticSearch, Kibana, Marvel) and ships with a fully functioning Docker deployment of these tools. Also includes a Slack admin scraper and working cron-based process for scheduled automated exports from Slack into your index (`TODO`).

`TODO`: Performance benchmarking of Conduit vector sizing with ElasticSearch Marvel

## Developing

##### Status
`TODO`: Dockerhub

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
```bash
Usage: slack-export-indexer [-e|--es-url URL] [-b|--bulk-size SIZE]
                            [-s|--shards COUNT] [-r|--replicas COUNT]
                            [-i|--index NAME] [-m|--mapping NAME]
                            [EXPORT_ZIP_FILE(s)...]
  Parses Slack export archives into ElasticSearch for exploring with Kibana

Available options:
  -h,--help                Show this help text
  -e,--es-url URL          Target ElasticSearch
                           URL. (default: "http://localhost:9200")
  -b,--bulk-size SIZE      Size of the vector buffered to ElasticSearch via the
                           bulk API. (default: 1000)
  -s,--shards COUNT        Number of primary shards. (default: 5)
  -r,--replicas COUNT      Number of replica shards. (default: 1)
  -i,--index NAME          Name of Slack index. (default: "slack")
  -m,--mapping NAME        Name of Slack index mapping. (default: "events")
```
