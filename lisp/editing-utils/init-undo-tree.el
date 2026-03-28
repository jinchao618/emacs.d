(require-package 'undo-tree)
;; Enable undo-tree for Evil compatibility
(global-undo-tree-mode t)
(setq undo-tree-auto-save-history nil)

;; Fix: Ensure undo-tree properly tracks saved state interminal mode
(setq undo-tree-enable-undo-in-regin nil)

(provide 'init-undo-tree)
;;; init-undo-tree.el ends here
