(require-package 'evil)
;; (setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
;(setq evil-default-state 'emacs)

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
(define-key evil-normal-state-map (kbd "C-'") 'comment-or-uncomment-region)
(evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

;; (eval-after-load 'evail-maps
;;   '(define-key evil-normal-state-map (kbd "M-.") nil))

;; reset undo syste to undo-tree
;; could be deprecated for Emacs beyond v.28
(evil-set-undo-system 'undo-tree)

(provide 'init-evil)
