(require-package 'undo-tree)
;; (require 'undo-tree-autoloads)
(global-set-key "\C-xu" 'undo-tree-visualize)
(global-undo-tree-mode t)
(setq undo-tree-auto-save-history nil)

(provide 'init-undo-tree)
;;; init-undo-tree.el ends here
