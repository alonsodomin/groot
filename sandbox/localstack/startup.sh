#!/bin/sh

aws configure set aws_access_key_id default_access_key
aws configure set aws_secret_access_key default_secret_key
aws configure set default.region us-west-2

localstack start &

sleep 5
#aws --endpoint-url=http://localhost:4572 --region=us-west-2 s3api create-bucket --bucket pluto-search-results-local

tail -f /dev/null