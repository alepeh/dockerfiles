cp -r ./layers/ap-org ~/.emacs.d/private/
sed 's:ORG_DIRECTORY:'"$SPACEMACS_WORKSPACE"':g' ~/.emacs.d/private/ap-org/packages.el > ~/.emacs.d/private/ap-org/packages.el.bak
sed 's:PUBLISH_DIRECTORY:'"$SPACEMACS_PUBLISH_DIRECTORY"':g' ~/.emacs.d/private/ap-org/packages.el.bak > ~/.emacs.d/private/ap-org/packages.el
rm ~/.emacs.d/private/ap-org/packages.el.bak
cp -r ./layers/org-roam ~/.emacs.d/private/
cp ./org-recoll.el ~/.emacs.d/