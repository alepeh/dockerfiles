FROM alpine:3.8 
LABEL maintainer "Alexander Pehm <alexander@alexanderpehm.at>"

RUN apk add --no-cache bash make curl tar gzip tzdata vim
ENV HOME /home/user
ENV EDITOR vim
ENV TODOTXT_DEFAULT_ACTION ls
ENV TODOTXT_DATE_ON_ADD 1
ENV TODOTXT_AUTO_ARCHIVE 0

RUN addgroup -g 1000 -S user && \
    adduser -u 1000 -S user -G user -h $HOME
RUN curl -L https://github.com/todotxt/todo.txt-cli/archive/v2.11.0.tar.gz | tar xvz && \
    cd todo.txt-cli-2.11.0 && make && make install CONFIG_DIR=$HOME
COPY config $HOME/.todo/
RUN mkdir /home/user/.todo.actions.d && cd /home/user/.todo.actions.d && \
    curl -L https://raw.githubusercontent.com/alepeh/todo.txt-cli/note/todo.actions.d/note > note && \
    curl -L https://raw.githubusercontent.com/alepeh/todo.txt-cli/note/todo.actions.d/archive > archive && \
    curl -L https://raw.githubusercontent.com/alepeh/todo.txt-cli/note/todo.actions.d/del > del && \
    curl -L https://raw.githubusercontent.com/alepeh/todo.txt-cli/note/todo.actions.d/rm > rm
RUN chmod +x -R /home/user/.todo.actions.d
RUN mkdir $HOME/workspace
RUN chown -R user:user $HOME 
WORKDIR $HOME
USER user
ENTRYPOINT ["sh","-c"]
CMD ["todo.sh","-d /home/user/.todo/config"]
