(require-package 'evil)
;; (setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
;(setq evil-default-state 'emacs)

(define-key evil-normal-state-map (kbd "C-]") 'helm-gtags-find-tag)
(define-key evil-normal-state-map (kbd "C-u C-]") 'helm-gtags-find-tag-other-window)
(define-key evil-normal-state-map (kbd "M-]") 'helm-gtags-find-rtag)
(define-key evil-normal-state-map (kbd "C-u M-]") 'helm-gtags-find-symbol)
(define-key evil-normal-state-map (kbd "C-t") 'helm-gtags-pop-stack)
(define-key evil-normal-state-map (kbd "M-.") 'helm-gtags-find-tag-from-here)

;; (eval-after-load 'evail-maps
;;   '(define-key evil-normal-state-map (kbd "M-.") nil))

(provide 'init-evil)
