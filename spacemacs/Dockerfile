FROM alepeh/spacemacs-lucid:1.0

# Enable graphviz dot for
RUN apt-get update && apt-get install graphviz

# Install plantuml (requires java)
RUN apt-get install default-jdk
ADD plantuml.jar /home/emacs/plantuml.jar

ADD ./.spacemacs.el /home/emacs/.spacemacs
# Add my custom layers
ADD ./layers/ap-org /home/emacs/.emacs.d/private/ap-org

# Install fonts
ADD ./SourceSansVariable-Roman.ttf /usr/share/fonts/truetype
ADD ./SourceSansVariable-Italic.ttf /usr/share/fonts/truetype
RUN fc-cache

# spacemacser:emacs == 1000:1000
RUN chown -R 1000:1000 /home/emacs/

# Install all packages
RUN asEnvUser emacs -batch -u ${UNAME} -kill

# Delete all compiled elisp files due to loading issues, see e.g. https://github.com/syl20bnr/spacemacs/issues/11801
RUN find /home/emacs/.emacs.d/elpa/ -name "*.elc" -print0 | xargs -0 rm
