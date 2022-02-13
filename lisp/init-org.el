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
  (save-excursion
    (org-ref-add-string org-ref-string (point-min) (point-max)))
  (if (equal org-ref-string "labels")
      (reverse org-ref-labels)
    (reverse org-ref-headings)))

(defun my-org-insert-ref-link ()
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
(setq org-babel-python-command "python3")
(setq-default org-confirm-babel-evaluate nil)

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

  (insert "#+Caption:\n")
  (insert (concat "#+Label: fig:" inputName "\n"))
  (insert "#+attr_org: :width 750px\n")
  (insert "#+attr_html: :width 50%\n")
  (insert "#+attr_latex: :float nil\n")
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

  (insert (concat "#+Label: fig:" inputName "\n"))
  (insert (concat "#+CALL: get-filename-by-backend(filename=\"" relativeFilename "\")\n"))
  (insert "#+Caption:\n")
  (insert (concat "#+Label: fig:" inputName "\n"))
  (insert "#+attr_org: :width 750px\n")
  ;; (insert "#+attr_html: :width 50%\n")
  (insert "#+attr_latex: :float nil\n")
  (insert (concat "#+RESULTS: fig:" inputName "\n"))
  (insert (concat "[[file:" relativeFilename ".svg]]"))
  (org-display-inline-images)
  )

(defun my-org-insert-table-setting ()
  "Insert table setting."
  (interactive)
  (insert "#+Caption:\n")
  (insert "#+Label:\n")
  (insert "#+attr_latex: :align |c|c|l|p{6cm}| :float nil")
  )

;; (defun my-org-download-image (link)
;;   ;; (interactive)
;;   (interactive "sUrl: ")
;;   (setq-default org-download-image-dir (concat "./" (file-name-sans-extension (file-name-nondirectory buffer-file-name)) "_IMG/"))
;;   (org-download-image link)
;;   )

;; (when *is-a-mac*
;;   (maybe-require-package 'grab-mac-link))

;; (maybe-require-package 'org-cliplink)

;; (define-key global-map (kbd "C-c l") 'org-store-link)
;; (define-key global-map (kbd "C-c a") 'org-agenda)

;; (defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
;;   "A keymap for handy global access to org helpers, particularly clocking.")

;; (define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-jump-to-current-clock)
;; (define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
;; (define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
;; (define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
;; (define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)


;; ;; Various preferences
;; (setq org-log-done t
;;       org-edit-timestamp-down-means-later t
;;       org-hide-emphasis-markers t
;;       org-catch-invisible-edits 'show
;;       org-export-coding-system 'utf-8
;;       org-fast-tag-selection-single-key 'expert
;;       org-html-validation-link nil
;;       org-export-kill-product-buffer-when-displayed t
;;       org-tags-column 80)


;; ;; Lots of stuff from http://doc.norang.ca/org-mode.html

;; ;; Re-align tags when window shape changes
;; (with-eval-after-load 'org-agenda
;;   (add-hook 'org-agenda-mode-hook
;;             (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))


;; 

;; (maybe-require-package 'writeroom-mode)

;; (define-minor-mode prose-mode
;;   "Set up a buffer for prose editing.
;; This enables or modifies a number of settings so that the
;; experience of editing prose is a little more like that of a
;; typical word processor."
;;   nil " Prose" nil
;;   (if prose-mode
;;       (progn
;;         (when (fboundp 'writeroom-mode)
;;           (writeroom-mode 1))
;;         (setq truncate-lines nil)
;;         (setq word-wrap t)
;;         (setq cursor-type 'bar)
;;         (when (eq major-mode 'org)
;;           (kill-local-variable 'buffer-face-mode-face))
;;         (buffer-face-mode 1)
;;         ;;(delete-selection-mode 1)
;;         (setq-local blink-cursor-interval 0.6)
;;         (setq-local show-trailing-whitespace nil)
;;         (setq-local line-spacing 0.2)
;;         (setq-local electric-pair-mode nil)
;;         (ignore-errors (flyspell-mode 1))
;;         (visual-line-mode 1))
;;     (kill-local-variable 'truncate-lines)
;;     (kill-local-variable 'word-wrap)
;;     (kill-local-variable 'cursor-type)
;;     (kill-local-variable 'blink-cursor-interval)
;;     (kill-local-variable 'show-trailing-whitespace)
;;     (kill-local-variable 'line-spacing)
;;     (kill-local-variable 'electric-pair-mode)
;;     (buffer-face-mode -1)
;;     ;; (delete-selection-mode -1)
;;     (flyspell-mode -1)
;;     (visual-line-mode -1)
;;     (when (fboundp 'writeroom-mode)
;;       (writeroom-mode 0))))

;; ;;(add-hook 'org-mode-hook 'buffer-face-mode)


;; (setq org-support-shift-select t)
;; 
;; ;;; Capturing

;; (global-set-key (kbd "C-c c") 'org-capture)

;; (setq org-capture-templates
;;       `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
;;          "* NEXT %?\n%U\n" :clock-resume t)
;;         ("n" "note" entry (file "")
;;          "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
;;         ))


;; 
;; ;;; Refiling

;; (setq org-refile-use-cache nil)

;; ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
;; (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

;; (with-eval-after-load 'org-agenda
;;   (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

;; (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; ;; Exclude DONE state tasks from refile targets
;; (defun sanityinc/verify-refile-target ()
;;   "Exclude todo keywords with a done state from refile targets."
;;   (not (member (nth 2 (org-heading-components)) org-done-keywords)))
;; (setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

;; (defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
;;   "A version of `org-refile' which allows refiling to any subtree."
;;   (interactive "P")
;;   (let ((org-refile-target-verify-function))
;;     (org-refile goto default-buffer rfloc msg)))

;; (defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
;;   "A version of `org-agenda-refile' which allows refiling to any subtree."
;;   (interactive "P")
;;   (let ((org-refile-target-verify-function))
;;     (org-agenda-refile goto rfloc no-update)))

;; ;; Targets start with the file name - allows creating level 1 tasks
;; ;;(setq org-refile-use-outline-path (quote file))
;; (setq org-refile-use-outline-path t)
;; (setq org-outline-path-complete-in-steps nil)

;; ;; Allow refile to create parent tasks with confirmation
;; (setq org-refile-allow-creating-parent-nodes 'confirm)

;; 
;; ;;; To-do settings

;; (setq org-todo-keywords
;;       (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
;;               (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
;;               (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
;;       org-todo-repeat-to-state "NEXT")

;; (setq org-todo-keyword-faces
;;       (quote (("NEXT" :inherit warning)
;;               ("PROJECT" :inherit font-lock-string-face))))


;; 
;; ;;; Agenda views

;; (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


;; (let ((active-project-match "-INBOX/PROJECT"))

;;   (setq org-stuck-projects
;;         `(,active-project-match ("NEXT")))

;;   (setq org-agenda-compact-blocks t
;;         org-agenda-sticky t
;;         org-agenda-start-on-weekday nil
;;         org-agenda-span 'day
;;         org-agenda-include-diary nil
;;         org-agenda-sorting-strategy
;;         '((agenda habit-down time-up user-defined-up effort-up category-keep)
;;           (todo category-up effort-up)
;;           (tags category-up effort-up)
;;           (search category-up))
;;         org-agenda-window-setup 'current-window
;;         org-agenda-custom-commands
;;         `(("N" "Notes" tags "NOTE"
;;            ((org-agenda-overriding-header "Notes")
;;             (org-tags-match-list-sublevels t)))
;;           ("g" "GTD"
;;            ((agenda "" nil)
;;             (tags "INBOX"
;;                   ((org-agenda-overriding-header "Inbox")
;;                    (org-tags-match-list-sublevels nil)))
;;             (stuck ""
;;                    ((org-agenda-overriding-header "Stuck Projects")
;;                     (org-agenda-tags-todo-honor-ignore-options t)
;;                     (org-tags-match-list-sublevels t)
;;                     (org-agenda-todo-ignore-scheduled 'future)))
;;             (tags-todo "-INBOX"
;;                        ((org-agenda-overriding-header "Next Actions")
;;                         (org-agenda-tags-todo-honor-ignore-options t)
;;                         (org-agenda-todo-ignore-scheduled 'future)
;;                         (org-agenda-skip-function
;;                          '(lambda ()
;;                             (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
;;                                 (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
;;                         (org-tags-match-list-sublevels t)
;;                         (org-agenda-sorting-strategy
;;                          '(todo-state-down effort-up category-keep))))
;;             (tags-todo ,active-project-match
;;                        ((org-agenda-overriding-header "Projects")
;;                         (org-tags-match-list-sublevels t)
;;                         (org-agenda-sorting-strategy
;;                          '(category-keep))))
;;             (tags-todo "-INBOX/-NEXT"
;;                        ((org-agenda-overriding-header "Orphaned Tasks")
;;                         (org-agenda-tags-todo-honor-ignore-options t)
;;                         (org-agenda-todo-ignore-scheduled 'future)
;;                         (org-agenda-skip-function
;;                          '(lambda ()
;;                             (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
;;                                 (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
;;                         (org-tags-match-list-sublevels t)
;;                         (org-agenda-sorting-strategy
;;                          '(category-keep))))
;;             (tags-todo "/WAITING"
;;                        ((org-agenda-overriding-header "Waiting")
;;                         (org-agenda-tags-todo-honor-ignore-options t)
;;                         (org-agenda-todo-ignore-scheduled 'future)
;;                         (org-agenda-sorting-strategy
;;                          '(category-keep))))
;;             (tags-todo "/DELEGATED"
;;                        ((org-agenda-overriding-header "Delegated")
;;                         (org-agenda-tags-todo-honor-ignore-options t)
;;                         (org-agenda-todo-ignore-scheduled 'future)
;;                         (org-agenda-sorting-strategy
;;                          '(category-keep))))
;;             (tags-todo "-INBOX"
;;                        ((org-agenda-overriding-header "On Hold")
;;                         (org-agenda-skip-function
;;                          '(lambda ()
;;                             (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
;;                                 (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
;;                         (org-tags-match-list-sublevels nil)
;;                         (org-agenda-sorting-strategy
;;                          '(category-keep))))
;;             ;; (tags-todo "-NEXT"
;;             ;;            ((org-agenda-overriding-header "All other TODOs")
;;             ;;             (org-match-list-sublevels t)))
;;             )))))


;; (add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; 
;; ;;; Org clock

;; ;; Save the running clock and all clock history when exiting Emacs, load it on startup
;; (with-eval-after-load 'org
;;   (org-clock-persistence-insinuate))
;; (setq org-clock-persist t)
;; (setq org-clock-in-resume t)

;; ;; Save clock data and notes in the LOGBOOK drawer
;; (setq org-clock-into-drawer t)
;; ;; Save state changes in the LOGBOOK drawer
;; (setq org-log-into-drawer t)
;; ;; Removes clocked tasks with 0:00 duration
;; (setq org-clock-out-remove-zero-time-clocks t)

;; ;; Show clock sums as hours and minutes, not "n days" etc.
;; (setq org-time-clocksum-format
;;       '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))


;; 
;; ;;; Show the clocked-in task - if any - in the header line
;; (defun sanityinc/show-org-clock-in-header-line ()
;;   (setq-default header-line-format '((" " org-mode-line-string " "))))

;; (defun sanityinc/hide-org-clock-from-header-line ()
;;   (setq-default header-line-format nil))

;; (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
;; (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
;; (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

;; (with-eval-after-load 'org-clock
;;   (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
;;   (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


;; 
;; (when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
;;   (add-hook 'org-clock-in-hook
;;             (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
;;                                 (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
;;   (add-hook 'org-clock-out-hook
;;             (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
;;                                 "tell application \"org-clock-statusbar\" to clock out"))))


;; 
;; ;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; ;; TODO: nested projects!


;; 
;; ;;; Archiving

;; (setq org-archive-mark-done nil)
;; (setq org-archive-location "%s_archive::* Archive")



;; 

;; (require-package 'org-pomodoro)
;; (setq org-pomodoro-keep-killed-pomodoro-time t)
;; (with-eval-after-load 'org-agenda
;;   (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; ;; Show iCal calendars in the org agenda
;; ;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;; ;;   (setq org-agenda-include-diary t
;; ;;         org-agenda-custom-commands
;; ;;         '(("I" "Import diary from iCal" agenda ""
;; ;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;; ;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;; ;;             (lambda ()
;; ;;               (goto-char (point-min))
;; ;;               (save-excursion
;; ;;                 (while (re-search-forward "^[a-z]" nil t)
;; ;;                   (goto-char (match-beginning 0))
;; ;;                   (insert "0:00-24:00 ")))
;; ;;               (while (re-search-forward "^ [a-z]" nil t)
;; ;;                 (goto-char (match-beginning 0))
;; ;;                 (save-excursion
;; ;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;; ;;                 (insert (match-string 0))))))


;; (with-eval-after-load 'org
;;   (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
;;   (when *is-a-mac*
;;     (define-key org-mode-map (kbd "M-h") nil)
;;     (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

;; (with-eval-after-load 'org
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    `((R . t)
;;      (ditaa . t)
;;      (dot . t)
;;      (emacs-lisp . t)
;;      (gnuplot . t)
;;      (haskell . nil)
;;      (latex . t)
;;      (ledger . t)
;;      (ocaml . nil)
;;      (octave . t)
;;      (plantuml . t)
;;      (python . t)
;;      (ruby . t)
;;      (screen . nil)
;;      (,(if (locate-library "ob-sh") 'sh 'shell) . t)
;;      (sql . t)
;;      (sqlite . t))))


(provide 'init-org)
;; ;;; init-org.el ends here
