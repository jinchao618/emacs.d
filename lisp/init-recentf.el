(recentf-mode 1)
(setq recentf-max-saved-items 500
      recentf-exclude '("/tmp/" "/ssh:"))
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(provide 'init-recentf)
