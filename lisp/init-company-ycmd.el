(require-package 'company-ycmd)

(require 'company-ycmd)
(company-ycmd-setup)
(set-variable 'ycmd-server-command '("python2" "/home/jinchao/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd/"))

(global-company-mode 1)
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;; (setq company-backends (delete 'company-semantic company-backends))
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends 'company-ycmd))
(add-hook 'c++-mode-hook 'ycmd-mode)
(add-hook 'c-mode-hook 'ycmd-mode)
(add-hook 'objc-mode-hook 'ycmd-mode)

(provide 'init-company-ycmd)
