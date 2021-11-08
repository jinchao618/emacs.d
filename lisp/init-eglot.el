(require-package 'company)
(global-company-mode 1)

(require-package 'eglot)
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; (add-to-list 'eglot-stay-out-of 'imenu)
;; (add-to-list 'eglot-stay-out-of 'company)

(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

(defun my-eglot-python-hook()
  ;; (cl-set-difference 'company eglot-stay-out-of)
  ;; (pop eglot-stay-out-of)
  (setq eglot-stay-out-of '(imenu))
  (flymake-mode -1)
  (eglot-ensure))

(defun my-eglot-cc-hook()
  (setq eglot-stay-out-of '(company imenu))
  ;; (message "%s" eglot-stay-out-of)
  ;; (flymake-mode -1)
  (eglot-ensure))

(add-hook 'c-mode-hook 'my-eglot-cc-hook)
(add-hook 'c++-mode-hook 'my-eglot-cc-hook)
(add-hook 'python-mode-hook 'my-eglot-python-hook)
;; (add-hook 'c++-mode-hook
;;           (lambda()
;;             (setq eglot-stay-out-of '(company imenu))
;;             (eglot-ensure))
;;           )
;; (add-hook 'emacs-lisp-mode-hook 'eglot-ensure)

(provide 'init-eglot)
