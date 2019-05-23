FROM python:3.6.8-alpine3.8
LABEL maintainer "Alexander Pehm <alexander@alexanderpehm.at>"

RUN apk add --no-cache gcc py3-setuptools libffi-dev openssl-dev musl-dev vim
ENV HOME /home/user
ENV XDG_CONFIG_HOME=$HOME/.jrnl

RUN addgroup -g 1000 -S user && \
    adduser -u 1000 -S user -G user -h $HOME

RUN pip install jrnl
RUN mkdir $HOME/.jrnl
RUN mkdir $HOME/workspace
COPY jrnl $HOME/.jrnl
COPY entrypoint.sh $HOME/entrypoint.sh
RUN chmod +x $HOME/entrypoint.sh

RUN chown -R user:user $HOME 

ENTRYPOINT ["/home/user/entrypoint.sh"]
CMD []
