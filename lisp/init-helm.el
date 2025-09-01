;;; package --- Summary:
;;; Commentary:
;;; Code:
(require-package 'helm)

;; (require-package 'helm-ag)
(require-package 'helm-rg)
;; (require-package 'ag)

(require-package 'helm-projectile)

;; (require 'helm-config)
(require 'helm-projectile)
;; (require 'ag)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-unset-key (kbd "C-x c"))

(helm-mode t)
(projectile-mode)
;; (setq projectile-indexing-method 'native)
;; (setq projectile-indexing-method 'alien)
;; (setq projectile-enable-caching t)
;; (setq projectile-svn-command "find . -type f -print0")
(helm-projectile-on)
;; (setq helm-ag-always-set-extra-option t)
(setq helm-candidate-number-limit nil)
;; (setq helm-ag-base-command "rg --no-heading")
;; (setq helm-ag-command-option "--smart-case")

(define-key helm-map "<escape>" 'helm-keyboard-quit)

(define-key helm-comp-read-map "<escape>" 'helm-keyboard-quit)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "C-c a") 'helm-do-ag-this-file)
;; (global-set-key (kbd "C-c g a") 'helm-do-ag)
(global-set-key (kbd "C-c a") 'helm-occur)
(global-set-key (kbd "C-c g a") 'helm-rg)
;; (global-set-key (kbd "C-c f s") 'projectile--find-file)

(require 'helm-fd)

;; Custom helm-fd implementation with space-separated AND search support
;; Similar to projectile-find-file behavior: "init helm" finds files containing both "init" AND "helm"

(defun helm-fd-and-fct (candidates _source)
  "Custom filtered-candidate-transformer for helm-fd AND search.
   Removes ANSI color codes to prevent display conflicts with helm highlighting."
  (cl-loop for i in candidates
           for fname = (ansi-color-apply i)
           collect fname))

(defun helm-fd-and-process ()
  "Initialize fd process with AND search support for space-separated terms.

   Example usage:
   - Input: 'key1 key2'
   - Creates regex: '(key1).*?(key2)'
   - Finds files containing both 'key1' AND 'key2' in their paths

   Uses --color=never to prevent green text conflicts with helm highlighting."
  (let* (process-connection-type
         (terms (split-string helm-pattern))
         ;; Convert space-separated terms to regex pattern for AND search
         ;; "foo bar" becomes "(foo).*?(bar)|(bar).*?(foo)" to match files containing both
         (regex-pattern (if (> (length terms) 1)
                            (let ((quoted-terms (mapcar 'regexp-quote terms)))
                              (concat "(" (mapconcat 'identity quoted-terms ").*?(") ")"
                                      "|"
                                      "(" (mapconcat 'identity (reverse quoted-terms) ").*?(") ")"))
                          (car terms)))
         ;; Use fd switches without color to prevent display conflicts
         (fd-switches-no-color '("--no-ignore" "--hidden" "--type" "f" "--type" "d" "--color=never"))
         (cmd (append fd-switches-no-color (list regex-pattern)))
         (proc (apply #'start-process "fd" nil helm-fd-executable cmd))
         (start-time (float-time))
         (fd-version (replace-regexp-in-string
                      "\n" ""
                      (shell-command-to-string
                       (concat helm-fd-executable " --version")))))
    (helm-log "helm-fd-and-process" "Fd command:\nfd %s"
              (mapconcat 'identity cmd " "))
    (helm-log "helm-fd-and-process" "VERSION: %s" fd-version)
    (prog1
        proc
      (set-process-sentinel
       proc (lambda (process event)
              (if (or (string= event "finished\n")
                      (process-get process 'reach-limit))
                  (with-helm-window
                    (when helm-fd-mode-line-function
                      (funcall helm-fd-mode-line-function start-time fd-version)
                      (force-mode-line-update)))
                (helm-log "helm-fd-and-process sentinel" "Error: Fd %s"
                          (replace-regexp-in-string "\n" "" event))))))))
(defun helm-fd (arg)
  "Enhanced helm-fd with space-separated AND search cpability.

   Usage:
   - C-c f d: Search in current directory with AND search support
   - C-u C-c f d: Choose directory first, then search

   Search examples:
   - 'key' -> finds files containing 'key'
   - 'key1 key2' -> finds files containing both 'key1' AND 'key2'. key1 and key2 can be any order.
   - 'config emacs lisp' -> finds files containing all three terms

   This mimics projectile-find-file's space-separated search behavior. "
  (interactive "P")
  (let ((directory
         (if arg
             (file-name-as-directory
              (read-directory-name "DefaultDirectory: "))
           default-directory)))
    (let ((default-directory directory))
      (helm :sources (helm-make-source "Fd AND Search" 'helm-source-async
                       :candidates-process #'helm-fd-and-process
                       :requires-pattern 2
                       :candidate-number-limit 20000
                       :nohighlight nil
                       :help-message 'helm-fd-help-message
                       :filtered-candidate-transformer #'helm-fd-and-fct
                       :action helm-type-file-actions
                       :keymap helm-fd-map
                       :header-name
                       (lambda (name)
                         (format "%s (%s)"
                                 name (abbreviate-file-name default-directory))))
            :buffer "*helm fd*"))))

(global-set-key (kbd "C-c f d") 'helm-fd)

(provide 'init-helm)
