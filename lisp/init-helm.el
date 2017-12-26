(require-package 'helm)

(require-package 'helm-ag)

(require-package 'helm-projectile)

(require-package 'helm-gtags)

(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(helm-mode t)
(projectile-mode)
;; (setq projectile-indexing-method 'native)
;; (setq projectile-indexing-method 'alien)
;; (setq projectile-enable-caching t)
(helm-projectile-on)

(define-key helm-map "<escape>" 'helm-keyboard-quit)

(define-key helm-comp-read-map "<escape>" 'helm-keyboard-quit)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(provide 'init-helm)
