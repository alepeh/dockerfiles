FROM alpine:3.8
LABEL maintainer "Alexander Pehm <alexander@alexanderpehm.at>"

RUN apk add --no-cache python3 py3-setuptools ffmpeg
RUN pip3 install --upgrade pip && pip3 install youtube_dl
RUN addgroup -g 1000 -S app && \
    adduser -u 1000 -S app -G app
USER 1000
WORKDIR /home/app
ENTRYPOINT ["youtube-dl"]
