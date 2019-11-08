(require-package 'helm)

(require-package 'helm-ag)

(require-package 'helm-projectile)

;; (require-package 'helm-gtags)

(require 'helm-config)
(require 'helm-projectile)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-unset-key (kbd "C-x c"))

(helm-mode t)
(projectile-mode)
;; (setq projectile-indexing-method 'native)
;; (setq projectile-indexing-method 'alien)
;; (setq projectile-enable-caching t)
(setq projectile-svn-command "find . -type f -print0")
(helm-projectile-on)
(setq helm-ag-always-set-extra-option t)

(define-key helm-map "<escape>" 'helm-keyboard-quit)

(define-key helm-comp-read-map "<escape>" 'helm-keyboard-quit)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c a") 'helm-do-ag-this-file)
(global-set-key (kbd "C-c g a") 'helm-do-ag)

(provide 'init-helm)
