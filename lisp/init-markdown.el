;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'markdown-mode)
  (add-auto-mode 'markdown-mode "\\.md\\.html\\'")
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode))
  (with-eval-after-load 'markdown-mode
    (setq markdown-command "pandoc")
    (define-key markdown-mode-map (kbd "C-c C-e h") #'my-markdown-export-html)
    (define-key markdown-mode-map (kbd "C-c C-e p") #'my-markdown-export-pdf)
    (define-key markdown-mode-map (kbd "C-c C-e H") #'my-markdown-export-html-open)
    (define-key markdown-mode-map (kbd "C-c C-e P") #'my-markdown-export-pdf-open)))

(defun my-markdown-export-html ()
  "Export current Markdown buffer to HTML using pandoc."
  (interactive)
  (let* ((input (buffer-file-name))
         (output (concat (file-name-sans-extension input) ".html")))
    (if (executable-find "pandoc")
        (progn
          (shell-command (format "pandoc -s -mathjax %s -o %s" (shell-quote-argument input) (shell-quote-argument output)))
          (message "Exported to %s" output))
      (error "pandoc not found; install with: brew install pandoc"))))

(defun my-markdown-export-html-open ()
  "Export current Markdown to HTML and open in browser."
  (interactive)
  (my-markdown-export-html)
  (let ((output (concat (file-name-sans-extension (buffer-file-name)) ".html")))
    (when (file-exists-p output)
      (browse-url (concat "file://" output)))))

(defun my-markdown-export-pdf ()
  "Export current Markdown buffer to PDF using pandoc and pdflatex."
  (interactive)
  (let* ((input (buffer-file-name))
         (output (concat (file-name-sans-extension input) ".pdf")))
    (if (executable-find "pandoc")
        (progn
          (shell-command (format "pandoc %s -o %s --pdf-engine=pdflatex" (shell-quote-argument input) (shell-quote-argument output)))
          (message "Exported to %s" output))
      (error "pandoc not found; install with: brew install pandoc"))))

(defun my-markdown-export-pdf-open ()
  "Export current Markdown to PDF and open it."
  (interactive)
  (my-markdown-export-pdf)
  (let ((output (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
    (when (file-exists-p output)
      (shell-command (format "open %s" (shell-quote-argument output))))))

(require-package 'impatient-mode)
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

(defun markdown-preview-like-god ()
  (interactive)
  (impatient-mode 1)
  (setq imp-user-filter #'markdown-html)
  (cl-incf imp-last-state)
  (imp--notify-clients))

(provide 'init-markdown)
;;; init-markdown.el ends here
