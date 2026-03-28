;;; init-ediff.el --- ediff config

;;; Commentary:

;;; Code:

(require-package 'evil)
(require-package 'undo-tree)

;; CRITICAL: Enable undo-tree BEFORE requiring evil
(global-undo-tree-mode t)
(setq undo-tree-auto-save-history nil)

;; Set undo system to undo-tree BEFORE requiring evil
(setq evil-undo-system 'undo-tree)

;; (setq evil-want-C-u-scroll t)
(require 'evil)

(evil-mode 1)
;(setq evil-default-state 'emacs)

;; Fix: Clear modified flag after undo if buffer matches saved file
(defun my-check-undo-modified-state ()
  "After undo, check if buffer matches saved file and clear modified flag."
  (when (and (buffer-file-name)
             (file-exists-p (buffer-file-name))
             (buffer-modified-p))
    (condition-case err
        (let* ((file-name (buffer-file-name))
               (coding-system (or buffer-file-coding-system 'undecided))
               (current-content (buffer-substring-no-properties (point-min) (point-max)))
               (saved-content (with-temp-buffer
                                (let ((coding-system-for-read coding-system))
                                  (insert-file-contents file-name))
                                (buffer-substring-no-properties (point-min) (point-max)))))
          (when (and current-content saved-content
                     (string= current-content saved-content))
            (set-buffer-modified-p nil)))
      (error nil))))  ; Silently ignore errors

;; Hook into evil undo/redo
(advice-add 'evil-undo :after (lambda (&rest _)
                                (my-check-undo-modified-state)))
(advice-add 'evil-redo :after (lambda (&rest _)
                                (my-check-undo-modified-state)))

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; (define-key evil-normal-state-map (kbd "C-]") 'helm-gtags-find-tag) (define-key evil-normal-state-map (kbd "C-u C-]") 'helm-gtags-find-tag-other-window)
;; (define-key evil-normal-state-map (kbd "M-]") 'helm-gtags-find-rtag)
;; (define-key evil-normal-state-map (kbd "C-u M-]") 'helm-gtags-find-symbol)
;; (define-key evil-normal-state-map (kbd "C-t") 'helm-gtags-pop-stack)
;; (define-key evil-normal-state-map (kbd "M-.") 'helm-gtags-find-tag-from-here)
;; (define-key evil-normal-state-map (kbd "C-p") 'helm-gtags-find-files)
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
(define-key evil-normal-state-map (kbd "C-.") 'xref-find-references)
(define-key evil-normal-state-map (kbd "C-M-.") 'xref-find-definitions-other-window)
(define-key evil-normal-state-map (kbd "C-'") 'comment-or-uncomment-region)

;; Remove Evil key bindings - set to nil to restore Emacs default behavior
(define-key evil-insert-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-a") nil)
;; (define-key evil-normal-state-map (kbd "C-e") nil)
;; (define-key evil-motion-state-map (kbd "C-e") nil)
;; (define-key evil-visual-state-map (kbd "C-e") nil)
;; (define-key evil-operator-state-map (kbd "C-e") nil)

(evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
(evil-set-initial-state 'helm-ag-mode 'emacs)
(evil-set-initial-state 'grep-mode 'emacs)
(evil-set-initial-state 'ztree-mode 'emacs)

;; (eval-after-load 'evail-maps
;;   '(define-key evil-normal-state-map (kbd "M-.") nil))

(provide 'init-evil)
;;; init-evil.el ends here
