SHELL := /bin/bash
WORKING_DIR := $(shell pwd)

IMAGE_NAME = slack-export-indexer
IMAGE_VERSION = latest
IMAGE_TAG = martinbaillie/$(IMAGE_NAME):$(IMAGE_VERSION)

.PHONY: build push

release:: build push

push::
	@docker push $(IMAGE_TAG)

build::
	@docker build --pull -t $(IMAGE_TAG) $(WORKING_DIR)
