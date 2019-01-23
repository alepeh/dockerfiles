FROM alpine:3.8
LABEL maintainer "Alexander Pehm <alexander@alexanderpehm.at>"

RUN apk add --no-cache python3 py3-setuptools jq 
RUN pip3 install --upgrade pip && pip3 install yq
ENTRYPOINT ["yq"]
