FROM alpine:3.12 
LABEL maintainer "Alexander Pehm <alexander@alexanderpehm.at>"

RUN apk add --no-cache bash make curl tar gzip tzdata vim task
ENV HOME /home/user
ENV EDITOR vim
ENV TODOTXT_DEFAULT_ACTION ls
ENV TODOTXT_DATE_ON_ADD 1
ENV TODOTXT_AUTO_ARCHIVE 0

RUN addgroup -g 1000 -S user && \
    adduser -u 1000 -S user -G user -h $HOME
COPY entrypoint.sh $HOME
RUN chmod +x $HOME/entrypoint.sh
COPY config $HOME/.taskrc/
RUN mkdir $HOME/.task/
RUN chown -R user:user $HOME 
WORKDIR $HOME
USER user
ENTRYPOINT ["/home/user/entrypoint.sh"]
CMD []
