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

(defvar spacemacs-workspace        (getenv "SPACEMACS_WORKSPACE"))
(defvar org-default-inbox-file     (concat spacemacs-workspace "/inbox.org")         "New stuff collects in this file")
(defvar org-default-tasks-file     (concat spacemacs-workspace "/tasks.org")         "Tasks, TODOs and little projects")
(defvar org-default-incubate-file  (concat spacemacs-workspace "/incubating.org")    "Stuff that's not ready yet")

(spacemacs/set-leader-keys "oo" 'hydra-organizer/body)
(spacemacs/set-leader-keys "oz" 'hydra-zoom/body)

(setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
(setq org-agenda-files (list
                          spacemacs-workspace
                          )
        org-agenda-default-appointment-duration 120
        org-icalendar-combined-agenda-file (concat spacemacs-workspace "/agenda.ics")
        org-attach-set-inherit t
        )

(setq org-lowest-priority ?D)
(setq org-default-priority ?E)

(defvar org-capture-templates (list))
(setq org-capture-default-template "i")
(add-to-list 'org-capture-templates
             `("i" "Inbox from nowhere"        entry
               (file ,org-default-inbox-file)
               "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:"
               :empty-lines 0))
(add-to-list 'org-capture-templates
             `("r" "Inbox with back-ref"        entry
               (file ,org-default-inbox-file)
               "* %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n\n%i\nFrom: %a"
               :empty-lines 0))

(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-targets '((org-default-incubate-file :level . 0)
                           (org-default-tasks-file :level . 0)))

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
                        `(org-document-title ((t (,@headline ,@variable-tuple :height 1.3 :underline nil))))
                        '(deft-title-face ((t (:inherit default :weight bold))))))

;; Do not ask for confirmation when executing code blocks
(setq org-confirm-babel-evaluate nil)

  ;; supported languages for code blocks
(org-babel-do-load-languages
  'org-babel-load-languages
    '((shell . t)
      (js . t)
      (plantuml . t)
      (dot . t)
      (java . t)))

;; Set the first day of the week to Monday
(setq calendar-week-start-day 1)

;; Customize the emacs calendar to show week numbers
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-warning-face))

(setq calendar-intermonth-header
      (propertize "Wk"                  ; or e.g. "KW" in Germany
                  'font-lock-face 'font-lock-keyword-face))

(require 'org-drill)

(use-package org-brain :ensure t
  :init
  (setq org-brain-path spacemacs-workspace)
  ;; For Evil users
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))

(defhydra hydra-organizer (nil nil)
"
^Navigate^      ^Agenda^          ^Go To^
^^^^^^^------------------------------------------------
_k_: ↑ previous _t_: All Todos    _g i_: Inbox
_j_: ↓ next                       _g s_: Layer config
"
  ("t" org-todo-list)
  ("g i" (find-file-other-window org-default-inbox-file))
  ("g s" (find-file-other-window (concat (getenv "DOCKERFILES_DIR") "/spacemacs/layers/ap-org/ap-org.org")))
  ("<up>" org-previous-visible-heading)
  ("<down>" org-next-visible-heading)
  ("k" org-previous-visible-heading)
  ("j" org-next-visible-heading)
)

(defhydra hydra-zoom (nil nil)
  "zoom2"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(setq org-todo-keyword-faces
          '(("TODO" . (:foreground "white" :weight bold)) ("STARTED" . "yellow")
            ("DONE" . (:foreground "green" :weight bold))))

      (setq org-image-actual-width '(300))

      ;;(setq org-refile-targets '((nil :maxlevel . 9)
      ;;(org-agenda-files :maxlevel . 9)))

      (setq org-plantuml-jar-path
            (expand-file-name "~/plantuml.jar"))

      (add-hook 'org-babel-after-execute-hook
                (lambda ()
                  (when org-inline-image-overlays
                    (org-redisplay-inline-images))))

    ;; Auto sorting
    (require 'cl)
    (require 'dash)

    (defun todo-to-int (todo)
        (first (-non-nil
                (mapcar (lambda (keywords)
                          (let ((todo-seq
                                 (-map (lambda (x) (first (split-string  x "(")))
                                       (rest keywords)))) 
                            (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                        org-todo-keywords))))

    (defun my/org-sort-key ()
      (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
             (todo (org-entry-get (point) "TODO"))
             (todo-int (if todo (todo-to-int todo) todo-max))
             (priority (org-entry-get (point) "PRIORITY"))
             (priority-int (if priority (string-to-char priority) org-default-priority)))
        (format "%03d %03d" todo-int priority-int)
        ))

    (defun my/org-sort-entries ()
      (interactive)
      (org-sort-entries nil ?f #'my/org-sort-key))


    ;; Export using Jekyll
    (defun org-export-table-cell-starts-colgroup-p (table-cell info))
    (defun org-export-table-cell-ends-colgroup-p (table-cell info))

    (setq org-publish-project-alist
          '(
      ("all-org-files-to-html"
             ;; Path to your org files.
             :base-directory spacemacs-workspace 
             :base-extension "org"
             :publishing-function org-html-publish-to-html
             :publishing-directory (concat spacemacs-workspace "/exports/")
      )
            ("all-org-attachments"
             :base-directory (concat spacemacs-workspace "/data/")
             :base-extension "css\\|js\\|png\\|jpeg\\|jpg\\|gif\\|pdf\\|mp3\\|ogg"
             :publishing-directory (concat spacemacs-workspace "/exports/data/")
             :recursive t
             :publishing-function org-publish-attachment)
            ("all-org-and-attachments" :components ("all-org-files-to-html" "all-org-attachments"))
            ("rfk-jekyll-html"
             ;; Path to your org files.
             :base-directory (concat spacemacs-workspace "/rfk/web/")
             :base-extension "org"

             ;; Path to your Jekyll project.
             :publishing-directory (concat spacemacs-workspace "/rfk/jekyll/")
             :recursive t
             :publishing-function org-html-publish-to-html
             :headline-levels 4 
             :html-extension "html"
             :body-only t ;; Only export section between <body> </body>
             )
            ("rfk-jekyll-attachments"
             :base-directory (concat spacemacs-workspace "/rfk/web/")
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
             :publishing-directory (concat spacemacs-workspace "/rfk/jekyll/")
             :recursive t
             :publishing-function org-publish-attachment)
            ("rfk-jekyll-all" :components ("rfk-jekyll-html" "rfk-jekyll-attachments"))

            ("blog-jekyll-html"
             ;; Path to your org files.
             :base-directory (concat spacemacs-workspace "/alepeh/blog/source/")
             :base-extension "org"

             ;; Path to your Jekyll project.
             :publishing-directory (concat spacemacs-workspace "/alepeh/blog/jekyll/thinkspace/")
             :recursive t
             :publishing-function org-html-publish-to-html
             :headline-levels 4 
             :html-extension "html"
             :with-toc nil ;; Otherwise using headkines will break the layout
             :body-only t ;; Only export section between <body> </body>
             )
            ("blog-jekyll-attachments"
             :base-directory (concat spacemacs-workspace "/alepeh/blog/source/")
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
             :publishing-directory (concat spacemacs-workspace "alepeh/blog/jekyll/thinkspace/")
             :recursive t
             :publishing-function org-publish-attachment)
            ("blog-jekyll-all" :components ("blog-jekyll-attachments" "blog-jekyll-html"))

            )
        )
      ;; In org 9.2 we need org-tempo to expand src and example blocks
      ;; they have been replaced with structure templates
      (require 'org-tempo)

      ;; org-journal configuration
      (setq org-journal-file-format "%Y%m%d.org")
      (setq org-journal-dir spacemacs-workspace)
      (setq org-journal-date-format "%Y-%m-%d, %A")
      (setq org-journal-file-type 'yearly)

      ;;drag and drop configuration
      ;; http://kitchingroup.cheme.cmu.edu/blog/2015/07/10/Drag-images-and-files-onto-org-mode-and-insert-a-link-to-them/
      (defun my-dnd-func (event)
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fname (cadr payload))
         (img-regexp "\\(png\\|jp[e]?g\\)\\>"))
    (cond
     ;; insert image link
     ((and  (eq 'drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; insert image link with caption
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert "#+ATTR_ORG: :width 300\n")
      (insert (concat  "#+CAPTION: " (read-input "Caption: ") "\n"))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; C-drag-n-drop to open a file
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type))
      (find-file fname))
     ((and (eq 'M-drag-n-drop (car event))
           (eq 'file type))
      (insert (format "[[attachfile:%s]]" fname)))
     ;; regular drag and drop on file
     ((eq 'file type)
      (insert (format "[[%s]]\n" fname)))
     (t
      (error "I am not equipped for dnd on %s" payload)))))


(define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<C-drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<M-drag-n-drop>") 'my-dnd-func)

) ;;ap-org/post-init-org ends here
