FROM alepeh/emacs25-lucid:latest

# Enable graphviz dot for
RUN apt-get update && apt-get install graphviz

# Install plantuml (requires java)
RUN apt-get install default-jdk
ADD plantuml.jar /home/emacs/plantuml.jar

ADD ./.spacemacs.el /home/emacs/.spacemacs

# Install fonts
ADD ./SourceSansVariable-Roman.ttf /usr/share/fonts/truetype
ADD ./SourceSansVariable-Italic.ttf /usr/share/fonts/truetype
RUN fc-cache

# spacemacser:emacs == 1000:1000
RUN chown 1000:1000 /home/emacs/.spacemacs

# Install all packages
RUN asEnvUser emacs -batch -u ${UNAME} -kill