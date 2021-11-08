;; (require-package 'company-ycmd)

;; (require 'company-ycmd)
;; (company-ycmd-setup)
;; (set-variable 'ycmd-server-command '("python" "/home/jinchao/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd/"))
;; (set-variable 'ycmd-startup-timeout' 10)
;; (set-variable 'ycmd-global-config "../ycm_extra_conf.py")

;; (setq company-backends (delete 'company-semantic company-backends))
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends 'company-ycmd))

;; (defun my-ycmd-common-hook()
;;   (company-mode t)
;;   (ycmd-mode t))

;; (add-hook 'c++-mode-hook 'my-ycmd-common-hook)
;; (add-hook 'c-mode-hook 'my-ycmd-common-hook)
;; (add-hook 'objc-mode-hook 'my-ycmd-common-hook)
;; (add-hook 'emacs-lisp-mode-hook 'my-ycmd-common-hook)
;; (add-hook 'python-mode-hook 'my-ycmd-common-hook)

(require-package 'company)
(require-package 'lsp-mode)
(require 'lsp-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'python-mode 'lsp)

(require-package 'lsp-ui)
(add-hook 'lsp 'lsp-ui-mode)
(setq lsp-ui-doc-position 'bottom)
;; (setq lsp-diagnostics-provider :none)

(setq lsp-ui-doc-enable t
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)

;; LSP
;; (use-package lsp-mode
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook ((python-mode . lsp-deferred)
;;          (go-mode . lsp-deferred)
;;          (rust-mode . lsp-deferred)
;;          (typescript-mode . lsp-deferred)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands (lsp lsp-deferred))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; (use-package company-lsp
;;   :commands company-lsp)

;; ;;Optional - provides snippet support.

;; (use-package yasnippet
;;   :commands yas-minor-mode
;;   :hook (
;;          (go-mode . yas-minor-mode)
;;          (python-mode . yas-minor-mode)
;;          ))

;; (setq lsp-ui-doc-enable t
;;       lsp-ui-peek-enable t
;;       lsp-ui-sideline-enable t
;;       lsp-ui-imenu-enable t
;;       lsp-ui-flycheck-enable t)

(provide 'init-lsp)
