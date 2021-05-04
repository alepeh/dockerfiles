#!/bin/bash
# Preconditions
# emacs (tested with 26.3)
if which  emacs; then
    echo "Emacs is already installed"
else 
    brew cask install emacs
fi
# java (for plantuml)
if which  java; then
    echo "Java is already installed"
else 
    brew cask install java
fi
# gnuplot (for plotting graphs from tables)
if which  gnuplot; then
    echo "Gnuplot is already installed"
else 
    brew install gnuplot
fi
# graphviz dot (for org-roam graphs)
if which  dot; then
    echo "Graphviz dot is already installed"
else 
    brew install graphviz
fi
# Backup the .emacs.d directory
zip -rq  $(eval date "+/tmp/.emacs.d_%Y-%m-%d-%H-%M-%S.zip") ~/.emacs.d
# Remove the current .emacs.d directory
rm -rf ~/.emacs.d
rm -rf ~/.plantuml_aws
# Clone the spacemacs development branch
git clone --single-branch --branch develop git@github.com:syl20bnr/spacemacs.git ~/.emacs.d

# Copy the plantuml binary (version 1.2019.13)
cp ./plantuml.jar ~
git clone --single-branch --branch master git@github.com:awslabs/aws-icons-for-plantuml.git ~/.plantuml_aws

# Copy the spacemacs config file
# By default, we are using the spacemacs-docker distribution, we need to switch to the default one for local installation
sed 's/dotspacemacs-distribution/;; dotspacemacs-distribution/g' .spacemacs.el > ~/.spacemacs

# Copy my private org-layer 
cp -r ./layers/ap-org ~/.emacs.d/private/ap-org
# Copy the org-roam layer 
cp -r ./layers/org-roam ~/.emacs.d/private/org-roam
# Copy the emacs launch script for use with e.g. platypus
cp ./launch_emacs.sh ~/.emacs.d/

cp ./org-recoll.el  ~/.emacs.d/
cp ./ox-confluence-en.el ~/.emacs.d/

# After starting emacs for the first time, you may need to run the following command:
# find ~/.emacs.d -name '*.elc' -delete
