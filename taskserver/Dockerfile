FROM ubuntu:focal

RUN apt-get update && apt-get install -y g++ libgnutls28-dev uuid-dev cmake curl vim gnutls-bin
RUN curl -LO https://taskwarrior.org/download/taskd-latest.tar.gz && tar xzf taskd-latest.tar.gz

RUN cd taskd-1.1.0 && cmake -DCMAKE_BUILD_TYPE=release . && make && make install

ENV TASKDDATA=/var/taskd

# This image does not configure the server or generate certificates, you need to mount
# the config directory

EXPOSE 53589

CMD [ "taskd", "server" ]