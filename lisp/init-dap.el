(require-package 'dap-mode)

(dap-ui-mode 1)
;; enables muse nover support
(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled 'dap-mode' will use the minibuffer
(tooltip-mode 1)
;; display floating panel with debug buttons
;; requires emacs 26+
(dap-ui-controls-mode 1)
(require 'dap-python)

(provide 'init-dap)
