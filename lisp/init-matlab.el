(require-package 'matlab-mode)
;; (require 'matlab-mode-autoloads)
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")
(provide 'init-matlab)
