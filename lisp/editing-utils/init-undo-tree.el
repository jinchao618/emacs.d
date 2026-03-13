(require-package 'undo-tree)
;; Do not enable global-undo-tree-mod - Evil use built-in undo-redo (Emacs 28+)
;; undo-tree-visualize still avilable via M-x if needed
;; (global-set-key "\C-xu" 'undo-tree-visualize)
;; (global-undo-tree-mode t)
;; (setq undo-tree-auto-save-history nil)

(provide 'init-undo-tree)
;;; init-undo-tree.el ends here
