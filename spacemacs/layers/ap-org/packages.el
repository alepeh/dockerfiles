;;; packages.el --- ap-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: alepeh <alexander@pehm.biz>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `ap-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ap-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ap-org/pre-init-PACKAGE' and/or
;;   `ap-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst ap-org-packages
  '(
   org
))

(defun ap-org/post-init-org ()

;; indent text corresponding with the headline
(setq org-startup-indented t)
;; hides leading/trailing formatting characters like *bold*, /italic/, =code=
(setq org-hide-emphasis-markers t)
;; use bullet points for all headline levels
(setq org-bullets-bullet-list '("○" "○" "○" "○"))
;; font face and color, make sure you have the fonts installed.
(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Variable") '(:font "Source Sans Variable"))
                           (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Variable."))))
      (base-font-color     (face-foreground 'default nil 'default))
      (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;; deeper levels of headlines are smaller
(custom-theme-set-faces 'user
                        `(org-level-8 ((t (,@headline ,@variable-tuple))))
                        `(org-level-7 ((t (,@headline ,@variable-tuple))))
                        `(org-level-6 ((t (,@headline ,@variable-tuple))))
                        `(org-level-5 ((t (,@headline ,@variable-tuple))))
                        `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                        `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
                        `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
                        `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4))))
                        `(org-document-title ((t (,@headline ,@variable-tuple :height 1.3 :underline nil))))))

;; supported languages for code blocks
(org-babel-do-load-languages
  'org-babel-load-languages
    '((shell . t)
      (js . t)
      (plantuml . t)
      (dot . t)
      (java . t)))

) ;;ap-org/post-init-org ends here
