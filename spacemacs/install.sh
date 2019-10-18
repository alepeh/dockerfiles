#!/bin/bash
# Preconditions
# emacs (tested with 26.3), installed via brew cask install emacs
# java (for plantuml)
# Clone the spacemacs development branch
git clone --single-branch --branch develop git@github.com:syl20bnr/spacemacs.git ~/.emacs.d

# Copy the plantuml binary
cp ./plantuml.jar ~

# Copy the spacemacs config file
# By default, we are using the spacemacs-docker distribution, we need to switch to the default one for local installation
sed 's/dotspacemacs-distribution/;; dotspacemacs-distribution/g' .spacemacs.el > ~/.spacemacs

# Copy my private org-layer 
cp -r ./layers/ap-org ~/.emacs.d/private/ap-org

# After starting emacs for the first time, you may need to run the following command:
# find ~/.emacs.d -name '*.elc' -delete
