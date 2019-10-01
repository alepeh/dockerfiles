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

(defvar org-default-inbox-file     "/mnt/workspace/inbox.org"         "New stuff collects in this file")
(defvar org-default-tasks-file     "/mnt/workspace/tasks.org"         "Tasks, TODOs and little projects")
(defvar org-default-incubate-file  "/mnt/workspace/incubating.org"   "Stuff that's not ready yet")

(setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
(setq org-agenda-files (list
                          "/mnt/workspace"
                          )
        org-agenda-default-appointment-duration 120
        org-icalendar-combined-agenda-file "/mnt/workspace/agenda.ics"
        org-attach-set-inherit t
        )

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
         :base-directory "/mnt/workspace/"
         :base-extension "org"
         :publishing-function org-html-publish-to-html
         :publishing-directory "/mnt/workspace/exports/"
  )
        ("all-org-attachments"
         :base-directory "/mnt/workspace/data/"
         :base-extension "css\\|js\\|png\\|jpeg\\|jpg\\|gif\\|pdf\\|mp3\\|ogg"
         :publishing-directory "/mnt/workspace/exports/data/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("all-org-and-attachments" :components ("all-org-files-to-html" "all-org-attachments"))
        ("rfk-jekyll-html"
         ;; Path to your org files.
         :base-directory "/mnt/workspace/rfk/web/"
         :base-extension "org"

         ;; Path to your Jekyll project.
         :publishing-directory "/mnt/workspace/rfk/jekyll/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4 
         :html-extension "html"
         :body-only t ;; Only export section between <body> </body>
         )
        ("rfk-jekyll-attachments"
         :base-directory "/mnt/workspace/rfk/web/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "/mnt/workspace/rfk/jekyll/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("rfk-jekyll-all" :components ("rfk-jekyll-html" "rfk-jekyll-attachments"))

        ("blog-jekyll-html"
         ;; Path to your org files.
         :base-directory "/mnt/workspace/alepeh/blog/source/"
         :base-extension "org"

         ;; Path to your Jekyll project.
         :publishing-directory "/mnt/workspace/alepeh/blog/jekyll/thinkspace/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4 
         :html-extension "html"
         :with-toc nil ;; Otherwise using headkines will break the layout
         :body-only t ;; Only export section between <body> </body>
         )
        ("blog-jekyll-attachments"
         :base-directory "/mnt/workspace/alepeh/blog/source/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "/mnt/workspace/alepeh/blog/jekyll/thinkspace/"
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
  (setq org-journal-dir "/mnt/workspace/")
  (setq org-journal-date-format "%Y-%m-%d, %A")
  (setq org-journal-file-type 'yearly)

) ;;ap-org/post-init-org ends here
