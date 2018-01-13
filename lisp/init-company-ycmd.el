(require-package 'company-ycmd)

(require 'company-ycmd)
(company-ycmd-setup)
(set-variable 'ycmd-server-command '("python" "/home/jinchao/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd/"))

(setq company-backends (delete 'company-semantic company-backends))
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-ycmd))

(defun my-ycmd-common-hook()
  (company-mode t)
  (ycmd-mode t))

(add-hook 'c++-mode-hook 'my-ycmd-common-hook)
(add-hook 'c-mode-hook 'my-ycmd-common-hook)
(add-hook 'objc-mode-hook 'my-ycmd-common-hook)
(add-hook 'emacs-lisp-mode-hook 'my-ycmd-common-hook)
(add-hook 'python-mode-hook 'my-ycmd-common-hook)

(provide 'init-company-ycmd)
