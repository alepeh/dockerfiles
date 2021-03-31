(defconst org-roam-packages
  '(org-roam))

(defun org-roam/init-org-roam ()
  (use-package org-roam
    :hook
    (after-init . org-roam-mode)
    :custom
    (defvar spacemacs-workspace        (getenv "SPACEMACS_WORKSPACE"))
    (org-roam-directory spacemacs-workspace)
    :init
    (require 'org-roam-protocol)
    (setq org-roam-completion-system 'helm)
    (setq org-roam-graph-viewer "/usr/bin/open")
    (setq org-roam-graph-exclude-matcher "-journal")
    (setq org-roam-graph-executable "/usr/local/bin/neato")
    (progn
      (spacemacs/declare-prefix "ar" "org-roam")
      (spacemacs/set-leader-keys
       "arl" 'org-roam
       "art" 'org-roam-dailies-today
       "arf" 'org-roam-find-file
       "arg" 'org-roam-graph)

      (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
                                                "rl" 'org-roam
                                                "rt" 'org-roam-dailies-today
                                                "rb" 'org-roam-switch-to-buffer
                                                "rf" 'org-roam-find-file
                                                "ri" 'org-roam-insert
                                                "rg" 'org-roam-graph))))
