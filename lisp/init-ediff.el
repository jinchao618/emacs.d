;;; init-ediff.el

;;; Commentary:

;;; Code:

(require-package 'ztree)
(require 'ztree)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; (setq ediff-split-window-function (if (> (frame-width) 150)
;;                                       'split-window-vertically
;;                                     'split-window-horizontally))
(setq ediff-split-window-function 'split-window-horizontally)
(provide 'init-ediff)
;;; init-ediff.el ends here
