;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:

(require 'org-tempo)
(require 'ox-latex)

;; config listings for source block
;; below line is depreciated by including package in org_header.org
;; (add-to-list 'org-latex-packages-alist '("" "listings" nil))
;; (setq org-latex-listings 'listings)
;; (setq org-latex-listings-options '(("breaklines" "true")
;;                                    ("frame" "lines")
;;                                    ("basicstyle" "\\footnotesize")
;;                                    ;; ("numbers" "left")
;;                                    ;; ("numberstyle" "\\tiny")
;;                                    ))

;; config minted for source block
(setq org-latex-listings 'minted)
(setq org-latex-custom-lang-environments
      '(
        (emacs-lisp "common-lispcode")
        ))
(setq org-latex-minted-options
      '(("frame" "lines")
        ("fontsize" "\\scriptsize")
        ("breaklines" "true")
        ;; ("linenos" "")
        ))
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;; (setq org-latex-pdf-process
;;        (let
;;            ((cmd (concat "pdflatex -shell-escape -interaction nonstopmode"
;;                  " --synctex=1"
;;                  " -output-directory %o %f")))
;;          (list cmd
;;            "cd %o; if test -r %b.idx; then makeindex %b.idx; fi"
;;            "cd %o; bibtex %b"
;;            cmd
;;            cmd)))
;; my org insert ref link and relative functions
;; reused org-ref functions
(defvar-local org-ref-labels '()
  "Known labels Stores a list of strings.")

(defvar-local org-ref-headings '()
  "Known headings Stores a list of strings.")

(defvar org-ref-debug nil "If non-nil print debug messages.")
(defvar org-ref-label-debug nil "If non-nil print debug messages.")

;; (defvar org-ref-headings-regexps
;;   '(
;;     ;; heading
;;     ;; "^\s*\\*+\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
;;     ;; "^\s*\\(\\*+\\s-+[+a-zA-Z0-9\\._-]*\\)\\_>"
;;     "^\s*\\(?1:\\*+\\s-+.*\\)\\_>")
;;   "List of regexps that are headings in org-ref.")

(defvar org-ref-label-regexps
  '(;; #+label:
    ;; "^#\\+label:\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
    "^\s*#\\+label:\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
    ;; CUSTOM_ID in a heading
    ":CUSTOM_ID:\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
    ;; #+name
    "^\\s-*#\\+name:\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
    ;; radio targets
    "<<\\(?1:[+a-zA-Z0-9:\\._-]*\\)>>"
    ;; #+tblname:
    "^\\s-*#\\+tblname:\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
    ;; label links
    "label:\\(?1:[+a-zA-Z0-9:\\._-]*\\)"
    ;; labels in latex
    "\\\\label{\\(?1:[+a-zA-Z0-9:\\._-]*\\)}")
  "List of regexps that are labels in org-ref.")

(defun org-ref-add-labels (start end)
  "Get lebels around START and END."
  (interactive "r")
  (save-excursion
    (save-match-data
      (cl-loop for rx in org-ref-label-regexps
	       do
	       (goto-char start)
	       (while (re-search-forward rx end t)
		 (let ((label (match-string-no-properties 1)))
		   ;; I don't know why this gets found, but some labels are
		   ;; empty strings. we don't store these.
		   (unless (string= "" label)
		     (with-silent-modifications
		       (put-text-property (match-beginning 1)
					  (match-end 1)
					  'org-ref-label t)
		       (put-text-property (match-beginning 1)
					  (match-end 1)
					  'rear-nonsticky '(org-ref-label)))

		     (when org-ref-label-debug
		       (message "oral: adding %s" label)
		       (message "%S\n" org-ref-labels))
		     (cl-pushnew label
				 org-ref-labels :test 'string=)
		     (when org-ref-label-debug
		       (message "  oral: added %s" label)
		       (message "  %S\n" org-ref-labels))
		     ;; now store the last end so we can tell for the next run
		     ;; if we are adding to a label.
		     (setq org-ref-last-label-end end))))))))

(defun org-ref-get-labels ()
  "Get ref labels."
  (save-excursion
    (org-ref-add-labels (point-min) (point-max)))
  (reverse org-ref-labels))

;;** context around org-ref links
(defun org-ref-get-label-context (label)
  "Return a string of context around a LABEL."
  (save-excursion
    (save-restriction
      (widen)
      (catch 'result
	(goto-char (point-min))
	(when (re-search-forward
	       (format "label:%s\\b" label) nil t)
	  (throw 'result (buffer-substring
			  (progn
			    (forward-line -1)
			    (beginning-of-line)
			    (point))
			  (progn
			    (forward-line 4)
			    (point)))))

	(goto-char (point-min))
	(when (re-search-forward
	       (format "\\label{%s}" label) nil t)
	  (throw 'result (buffer-substring
			  (progn
			    (forward-line -1)
			    (beginning-of-line)
			    (point))
			  (progn
			    (forward-line 4)
			    (point)))))

	(goto-char (point-min))
	(when (re-search-forward
	       (format "^\\( \\)*#\\+label:\\s-*\\(%s\\)\\b" label) nil t)
	  (throw 'result (buffer-substring
			  (progn
			    (forward-line -1)
			    (beginning-of-line)
			    (point))
			  (progn
			    (forward-line 4)
			    (point)))))

	(goto-char (point-min))
	(when (re-search-forward
	       (format "^\\( \\)*#\\+tblname:\\s-*\\(%s\\)\\b" label) nil t)
	  (throw 'result (buffer-substring
			  (progn
			    (forward-line -1)
			    (beginning-of-line)
			    (point))
			  (progn
			    (forward-line 4)
			    (point)))))

	(goto-char (point-min))
	(when (re-search-forward
	       (format "^\\( \\)*#\\+name:\\s-*\\(%s\\)\\b" label) nil t)
	  (throw 'result (buffer-substring
			  (progn
			    (forward-line -1)
			    (beginning-of-line)
			    (point))
			  (progn
			    (forward-line 4)
			    (point)))))
	;; ;; CUSTOM_ID
	(goto-char (point-min))
	;; do we have a CUSTOM-ID?
	(let ((heading (org-map-entries
			(lambda ()
			  (buffer-substring
			   (progn
			     (forward-line -1)
			     (beginning-of-line)
			     (point))
			   (progn
			     (forward-line 4)
			     (point))))
			(format  "CUSTOM_ID=\"%s\"" label))))
	  ;; (message-box heading)
	  (when heading
	    (throw 'result (car heading))))
	;; radio target
	(goto-char (point-min))
	(when (re-search-forward (format "<<%s>>" (regexp-quote label)) nil t)
	  (throw 'result (match-string 0)))


	(throw 'result "!!! NO CONTEXT FOUND !!!")))))

(defun my-org-insert-ref-link-context ()
  "Insert reference with contents for current file."
  (interactive)
  (let* ((labels (org-ref-get-labels))
         (contexts (mapcar 'org-ref-get-label-context labels))
         (cb (current-buffer)))

    (helm :input (thing-at-point 'word)
          :sources `(((name . "Available labels to ref")
                      (multiline)
                      (candidates . ,(cl-loop for label in labels
                                              for context in contexts
                                              ;; we do some kludgy adding spaces
                                              ;; and bars to make it "easier" to
                                              ;; see in helm.
                                              collect (cons (concat
                                                             label "\n"
                                                             (mapconcat
                                                              (lambda (x)
                                                                (concat "   |" x))
                                                              (split-string context "\n")
                                                              "\n"
                                                              ) "\n\n") label)))
                      ;; default action to replace or insert ref link.
                      (action . (lambda (label)
                                  (switch-to-buffer ,cb)
                                  (insert (format "[[%s]]" label)))))))))

(defun org-ref-add-string (ref-string start end)
  "Get ref string at REF-STRING, START and END."
  (interactive "r")
  (if (equal ref-string "labels")
      (setq ref-regexps
            '(;; #+label:
              ;; "^#\\+label:\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
              "^\s*#\\+label:\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
              ;; CUSTOM_ID in a heading
              ":CUSTOM_ID:\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
              ;; #+name
              "^\\s-*#\\+name:\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
              ;; radio targets
              "<<\\(?1:[+a-zA-Z0-9:\\._-]*\\)>>"
              ;; #+tblname:
              "^\\s-*#\\+tblname:\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
              ;; label links
              "label:\\(?1:[+a-zA-Z0-9:\\._-]*\\)"
              ;; labels in latex
              "\\\\label{\\(?1:[+a-zA-Z0-9:\\._-]*\\)}"))
    (setq ref-regexps
          '(
            ;; heading
            ;; "^\s*\\*+\\s-+\\(?1:[+a-zA-Z0-9:\\._-]*\\)\\_>"
            ;; "^\s*\\(\\*+\\s-+[+a-zA-Z0-9\\._-]*\\)\\_>"
            "^\s*\\(?1:\\*+\\s-+.*\\)\\_>")))
  (save-excursion
    (save-match-data
      (cl-loop for rx in ref-regexps
	       do
	       (goto-char start)
	       (while (re-search-forward rx end t)
		 (let ((label (match-string-no-properties 1)))
		   ;; I don't know why this gets found, but some labels are
		   ;; empty strings. we don't store these.
		   (unless (string= "" label)
		     (with-silent-modifications
                       (if (equal ref-string "labels")
                           (progn
                             (put-text-property (match-beginning 1)
                                              (match-end 1)
                                              'org-ref-label t)
                             (put-text-property (match-beginning 1)
                                                (match-end 1)
                                                'rear-nonsticky '(org-ref-label)))
                         (progn
                           (put-text-property (match-beginning 1)
                                             (match-end 1)
                                             'org-ref-heading t)
                           (put-text-property (match-beginning 1)
                                              (match-end 1)
                                              'rear-nonsticky '(org-ref-heading)))
                         )
                       )

		     (when org-ref-debug
		       (message "oral: adding %s" label)
                       (if (equal ref-string "labels")
                           (message "%S\n" org-ref-labels)
                         (message "%S\n" org-ref-headings)))
                     (if (equal ref-string "labels")
                         (cl-pushnew label
                                     org-ref-labels :test 'string=)
                       (cl-pushnew label
                                 org-ref-headings :test 'string=))
		     (when org-ref-debug
		       (message "  oral: added %s" label)
                       (if (equal ref-string "labels")
                           (message "%S\n" org-ref-labels)
                         (message "%S\n" org-ref-headings)))
		     ;; now store the last end so we can tell for the next run
		     ;; if we are adding to a label.
		     (setq org-ref-last-label-end end))))))))

(defun org-ref-get-contents (org-ref-string)
  "Get reference content of ORG-REF-STRING."
  (save-excursion
    (org-ref-add-string org-ref-string (point-min) (point-max)))
  (if (equal org-ref-string "labels")
      (reverse org-ref-labels)
    (reverse org-ref-headings)))

(defun my-org-insert-ref-link ()
  "Generate references for current org file.
And generate link for selected reference."
  (interactive)
  (let ((labels (org-ref-get-contents "labels")))
    (message "%S\n" labels)
    (helm :sources `(,(helm-build-sync-source "Existing labels"
			:candidates labels
			:action (lambda (label)
				  (with-helm-current-buffer
                                    (insert (format "[[%s]]" label))))))
	  :buffer "*helm labels*")))

(defun my-org-insert-ref-link-multi_files ()
  "Generate references for all org files and generate link for selected reference."
  (interactive)
  (let ((files (f-entries "." (lambda (f) (f-ext? f "org")) t))
    (labels '())
    choice)
    (cl-loop for file in files do
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (cl-loop for rx in org-ref-label-regexps do
                 (while (re-search-forward rx nil t)
                   (cl-pushnew (list
                                (format "%-80s (%s)"
                                        (match-string 1)
                                        (file-name-nondirectory file))
                                :file (f-relative file)
                                :label (match-string 1))
                               labels))
                 )
        ))
    (setq choice
      (completing-read "Existing labels: " (reverse labels)))
    (setq file (plist-get (cdr (assoc choice labels)) :file))
    (setq label (plist-get (cdr (assoc choice labels)) :label))
    ;; (setq str0 (string-join (cdr (split-string heading)) " "))
    (insert (format "[[file:%s::*%s]]" file label))
    ))

(defun my-org-insert-headings-link ()
  "Insert heading link of current file."
  (interactive)
  (let ((labels (org-ref-get-contents "headings")))
    (helm :sources `(,(helm-build-sync-source "Existing headings"
			:candidates labels
			:action (lambda (label)
				  (with-helm-current-buffer
                                    (setq heading (butlast (nthcdr 1 (split-string label " ")) -1))
                                    ;; (print (string-join heading " "))
                                    (insert (format "[[%s]]" (string-join heading " ")))))))
	  :buffer "*helm labels*")))

(defun my-org-insert-headings-link-multi_files ()
  "Generate headings for all org files and generate link for selected heading."
  (interactive)
  (let ((files (f-entries "." (lambda (f) (f-ext? f "org")) t))
    (headlines '())
    choice)
    (cl-loop for file in files do
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward org-heading-regexp nil t)
          (cl-pushnew (list
               (format "%-80s (%s)"
                   (match-string 0)
                   (file-name-nondirectory file))
               :file (f-relative file)
               :heading (match-string 0))
              headlines))))
    ;; (message "%s" (reverse headlines))
    (setq choice
      (completing-read "Headline: " (reverse headlines)))
    ;; (message "%s" choice)
    (setq file (plist-get (cdr (assoc choice headlines)) :file))
    (setq heading (plist-get (cdr (assoc choice headlines)) :heading))
    ;; (message "1: %s" heading)
    (setq str0 (string-join (cdr (split-string heading)) " "))
    ;; (message "2: %s" str0)
    (insert (format "[[file:%s::*%s][%s]]" file str0 str0))
    ))

(defun my-org-navigate-headings-multi-files ()
  "Navigate headings for all org files under the same folder."
  (interactive)
  (let ((files (f-entries "." (lambda (f) (f-ext? f "org")) t))
    (headlines '())
    choice)
    (cl-loop for file in files do
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward org-heading-regexp nil t)
          (cl-pushnew (list
               (format "%-80s (%s)"
                   (match-string 0)
                   (file-name-nondirectory file))
               :file file
               :position (match-beginning 0))
              headlines))))
    ;; (message "%s" (reverse headlines))
    (setq choice
      (completing-read "Headline: " (reverse headlines)))
    (find-file (plist-get (cdr (assoc choice headlines)) :file))
    (goto-char (plist-get (cdr (assoc choice headlines)) :position))))


(defun my-org-generate-headings-multi-files ()
  "Generate headings with input TOC level for all org files."
  (interactive)
  (setq inputlvl (read-string "TOC level:"))
  (let ((headings (delq nil (cl-loop for f in (f-entries "." (lambda (f) (f-ext? f "org")) t)
                  append
                  (with-current-buffer (find-file-noselect f)
                    (org-map-entries
                     (lambda ()
                       (when (> (1+ (string-to-number inputlvl)) (car (org-heading-components)))
                         (setq lev (make-string (nth 0 (org-heading-components)) ?*))
                         (cons (f-relative f) (concat lev "#" (nth 4 (org-heading-components))))
                         ))))))))
    ;; (message "%s" headings)
    ;; (switch-to-buffer (get-buffer-create "*toc*"))
    ;; (erase-buffer)
    ;; (org-mode)
    (cl-loop for (file . heading) in headings
      do
      (setq splitstr (split-string heading "#"))
      (setq str0 (nth 0 splitstr))
      (setq str1 (nth 1 splitstr))
      (insert (format "%s [[file:%s::*%s][%s]]\n" str0 file str1 str1))
      )))
;; (defun org-toc ()
;;   "Generate a table of contents for org-files in this directory."
;;   (interactive)
;;   (let ((org-agenda-files (f-entries "." (lambda (f) (f-ext? f "org")) t)))
;;     (helm-org-agenda-files-headings)))

;; config org-ref, but doesn't work properly for HTML export
;; (require-package 'org-ref)
;; (require 'org-ref)
;; (setq org-latex-prefer-user-labels t)
;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;; config org-super-link, it works but doesn't work as expected
;; (require-package 'use-package)
;; (require-package 'quelpa-use-package)
;; (require 'quelpa-use-package)
;; (require 'use-package)
;; (use-package org-super-links
;;   :quelpa (org-super-links :repo "toshism/org-super-links" :fetcher github :commit "develop")
;;   :bind (("C-c s s" . org-super-links-link)
;;          ("C-c s l" . org-super-links-store-link)
;;          ("C-c s C-l" . org-super-links-insert-link)
;;          ("C-c s d" . org-super-links-quick-insert-drawer-link)
;;          ("C-c s i" . org-super-links-quick-insert-inline-link)
;;          ("C-c s C-d" . org-super-links-delete-link))
;;   :config
;;   (setq org-super-links-related-into-drawer t
;;   	org-super-links-link-prefix 'org-super-links-link-prefix-timestamp))

;; (when (not (eq system-type 'darwin))
(when (not *is-a-mac*)
  (setq org-file-apps
        (quote
         ((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . "/usr/bin/chromium %s")
          ("\\.pdf\\'" . default))))
)

(require-package 'org-download)
(require 'org-download)

;; load and enable languages
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '(
;;    (python . t)
;;    ;; (ipython . t)
;;    ;; (sh . t)
;;    (shell . t)
;;    ;; Include other languages here...
;;    ))
(eval-after-load "org"
  '(org-babel-do-load-languages
    'org-babel-load-languages
    '(
      ;; (sh . t)
      (python . t)
      (emacs-lisp . t)
      (plantuml . t)
      (julia . t)
      ;; (ditaa . t)
      )))
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(setq org-ascii-underline (quote ((ascii) (latin1) (utf-8))))
(setq org-ascii-headline-spacing (quote (0 . 0)))
;; (setq org-ascii-inner-margin 0)

(setq-default org-download-display-inline-images nil)
(setq-default org-image-actual-width nil)
;; (setq-default org-download-heading-lvl nil)
(setq-default org-list-allow-alphabetical t)
(setq org-startup-indented t)
(setq org-startup-numerated t)
(setq org-babel-python-command "python3")
(setq-default org-confirm-babel-evaluate nil)
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face))
(set-face-attribute 'org-block nil :inherit '(shadow))
(set-face-attribute 'org-code nil :inherit '(shadow))
(set-face-attribute 'org-table nil :inherit nil)
(set-face-attribute 'org-date nil :inherit nil)
;; (setq org-src-preserve-indentation t)
;; (setq org-html-indent nil)
(add-to-list 'org-src-lang-modes '("latex-macros" . latex))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(setq org-plantuml-exec-mode 'plantuml)
(setq org-html-mathjax-template
   "<script>\12  MathJax.Hub.Config = {\12    tex: {\12      ams: {\12        multlineWidth: '%MULTLINEWIDTH'\12      },\12      tags: '%TAGS',\12      tagSide: '%TAGSIDE',\12      tagIndent: '%TAGINDENT'\12    },\12    chtml: {\12      scale: %SCALE,\12      displayAlign: '%ALIGN',\12      displayIndent: '%INDENT'\12    },\12    svg: {\12      scale: %SCALE,\12      displayAlign: '%ALIGN',\12      displayIndent: '%INDENT'\12    },\12    output: {\12      font: '%FONT',\12      displayOverflow: '%OVERFLOW'\12    }\12  };\12</script>\12\12<script\12  id=\"MathJax-script\"\12  async\12  src=\"%PATH\">\12</script>")

;; (defun org-insert-clipboard-image (&optional file)
(defun my-org-insert-clipboard-image ()
  "Insert image from clipboad."
  ;; (interactive "F")
  (interactive)
  ;; (setq foldername (concat "./" (file-name-sans-extension (file-name-nondirectory buffer-file-name)) "_IMG/"))
  (setq foldername org-download-image-dir)
  (if (not (file-exists-p foldername))
      (mkdir foldername))

  (setq inputName (read-string "File name:"))
  (if (equal inputName "")
      (setq inputName (format-time-string "%Y_%m_%d__%H_%M_%S")))

  (setq imgName (concat "img_" inputName ".png"))

  ;; (setq imgname (concat "img_" (format-time-string "%y_%m_%d__%h_%m_%s") ".png"))
  ;; (setq imgPath (cocat (buffer-file-name) "IMG/" imgName))

  (setq relativeFilename (concat foldername imgName))

  (if *is-a-mac*
      (shell-command (concat "pngpaste " relativeFilename))
    (shell-command (concat "xclip -selection clipboard -t image/png -o > " relativeFilename)))

  (setq ipt (progn (back-to-indentation) (point)))
  (setq bol (progn (move-beginning-of-line 1) (point)))
  (back-to-indentation)
  (if (y-or-n-p "With indent:")
      (setq indent (buffer-substring bol ipt))
      ;; (progn
      ;;   (setq indent (buffer-substring bol ipt))
        ;; (back-to-indentation))
    (progn
      (setq indent "")
      (move-beginning-of-line 1)
      (delete-region bol ipt)))
  (insert "#+Caption:\n")
  (insert indent)
  (insert (concat "#+Label: fig:" inputName "\n"))
  (insert indent)
  (insert "#+attr_org: :width 750px\n")
  (insert indent)
  (insert "#+attr_html: :width 100%\n")
  (insert indent)
  (insert "#+attr_latex: :float nil\n")
  (insert indent)
  (insert (concat "[[file:" relativeFilename "]]"))
  (org-display-inline-images)
  )

(defun my-org-insert-image-setting ()
  "Insert image setting."
  (interactive)
  (setq foldername org-download-image-dir)
  (if (not (file-exists-p foldername))
      (mkdir foldername))

  (setq inputName (read-string "File name:"))
  (if (equal inputName "")
      (setq inputName (format-time-string "%Y_%m_%d__%H_%M_%S")))

  (setq imgName (concat "img_" inputName))

  (setq relativeFilename (concat foldername imgName))
  (if (not (file-exists-p (concat relativeFilename ".svg")))
      (make-empty-file (concat relativeFilename ".svg")))

  (setq ipt (progn (back-to-indentation) (point)))
  (setq bol (progn (move-beginning-of-line 1) (point)))
  (back-to-indentation)
  (if (y-or-n-p "With indent:")
      (setq indent (buffer-substring bol ipt))
    (progn
      (setq indent "")
      (move-beginning-of-line 1)
      (delete-region bol ipt)))
  (insert (concat "#+name: fn:" inputName "\n"))
  (insert indent)
  (insert (concat "#+CALL: get-filename-by-backend(filename=\"" relativeFilename "\")\n"))
  (insert indent)
  (insert "#+Caption:\n")
  (insert indent)
  (insert (concat "#+Label: fig:" inputName "\n"))
  (insert indent)
  (insert "#+attr_org: :width 750px\n")
  (insert indent)
  (insert "#+attr_html: :width 100%\n")
  (insert indent)
  (insert "#+attr_latex: :float nil\n")
  (insert indent)
  (insert (concat "#+RESULTS: fn:" inputName "\n"))
  (insert indent)
  (insert (concat "[[file:" relativeFilename ".svg]]"))
  (org-display-inline-images)
  )

(defun my-org-insert-table-setting ()
  "Insert table setting."
  (interactive)
  (setq ipt (progn (back-to-indentation) (point)))
  (setq bol (progn (move-beginning-of-line 1) (point)))
  (back-to-indentation)
  (if (y-or-n-p "With indent:")
      (setq indent (buffer-substring bol ipt))
    (progn
      (setq indent "")
      (move-beginning-of-line 1)
      (delete-region bol ipt)))
  (insert "#+Caption:\n")
  (insert indent)
  (insert "#+Label:\n")
  (insert indent)
  (insert "#+attr_latex: :align |c|c|l|p{6cm}| :float nil")
  )

(defvar org-babel-default-header-args:latex-macros
  '((:results . "raw")
    (:exports . "results")))

(defun prefix-all-lines (pre body)
  "Latex macros with PRE and BODY ."
  (with-temp-buffer
    (insert body)
    (string-insert-rectangle (point-min) (point-max) pre)
    (buffer-string)))

(defun org-babel-execute:latex-macros (body _params)
  "Latex macros with BODY ."
  (concat
   (prefix-all-lines "#+LATEX_HEADER: " body)
   "\n#+HTML_HEAD_EXTRA: <div style=\"display: none\"> \\(\n"
   (prefix-all-lines "#+HTML_HEAD_EXTRA: " body)
   "\n#+HTML_HEAD_EXTRA: \\)</div>\n"))

;; (defun my-org-download-image (link)
;;   ;; (interactive)
;;   (interactive "sUrl: ")
;;   (setq-default org-download-image-dir (concat "./" (file-name-sans-extension (file-name-nondirectory buffer-file-name)) "_IMG/"))
;;   (org-download-image link)
;;   )

(provide 'init-org)
;;; init-org.el ends here
