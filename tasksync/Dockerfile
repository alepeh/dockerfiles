FROM alpine:3.12 
LABEL maintainer "Alexander Pehm <alexander@alexanderpehm.at>"

RUN apk add --no-cache bash curl tar gzip git tzdata task py3-virtualenv

ENV HOME /home/user

RUN addgroup -g 1000 -S user && \
    adduser -u 1000 -S user -G user -h $HOME

COPY entrypoint.sh $HOME
RUN chmod +x $HOME/entrypoint.sh
COPY config $HOME/.taskrc
RUN mkdir $HOME/.task/
RUN chown -R user:user $HOME 
RUN cd $HOME
#RUN curl -L https://github.com/burnison/tasksync/archive/master.tar.gz | tar xvz && \
WORKDIR $HOME
COPY tasksync ./tasksync/
RUN chown -R user:user $HOME 
USER user
RUN cd tasksync && virtualenv .venv && . .venv/bin/activate && \
   cp tasksync/config.py.example tasksync/config.py && python setup.py install
ENTRYPOINT ["/home/user/entrypoint.sh"]
CMD []
