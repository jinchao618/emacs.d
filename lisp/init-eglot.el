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
(global-company-mode 1)
(require-package 'eglot)
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "/opt/homebrew/opt/llvm/bin/clangd"))
(add-to-list 'eglot-stay-out-of 'company)
(defun my-eglot-common-hook()
  (company-mode t)
  eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-hook 'c-mode-hook 'my-eglot-common-hook)
;; (add-hook 'c++-mode-hook 'my-eglot-common-hook)
;; (add-hook 'emacs-lisp-mode-hook 'my-eglot-common-hook)

(provide 'init-eglot)
