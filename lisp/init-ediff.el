
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (if (> (frame-width) 150)
                                      'split-window-vertically
                                    'split-window-horizontally))
(provide 'init-ediff)
