;;; package --- Summary:
;;; Commentary:
;;; Code:
(require-package 'helm)

;; (require-package 'helm-ag)
(require-package 'helm-rg)
;; (require-package 'ag)

(require-package 'helm-projectile)

;; (require-package 'helm-gtags)

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
(setq projectile-svn-command "find . -type f -print0")
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
(global-set-key (kbd "C-c f s") 'projectile--find-file)

(require 'helm-fd)
(defun helm-fd (arg)
  (interactive "P")
  (let ((directory
         (if arg
             (file-name-as-directory
              (read-directory-name "DefaultDirectory: "))
           default-directory)))
    (helm-fd-1 directory)))

(global-set-key (kbd "C-c f d") 'helm-fd)
(provide 'init-helm)
