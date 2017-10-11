FROM alpine:3.5

VOLUME /root/.aws
VOLUME /project

WORKDIR /project

RUN apk -v --update add \
        python-dev \
        py-pip \
        groff \
        less \
        gcc \
        libc-dev \
        mailcap \
        linux-headers \
        libffi-dev \
        openssl-dev \
        nodejs
RUN pip install --upgrade pip awscli s3cmd python-magic
RUN rm /var/cache/apk/*
RUN pip install localstack