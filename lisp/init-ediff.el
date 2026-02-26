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

;; Save window configuration before ediff and restore it after quitting
(defvar my-ediff-saved-window-config nil
  "Window configuration before ediff.")

(defun my-ediff-save-window-config ()
  "Save the current window configuration before ediff starts."
  (setq my-ediff-saved-window-config (current-window-configuration)))

(defun my-ediff-restore-window-config ()
  "Restore the window configuration after ediff quits."
  (when my-ediff-saved-window-config
    (set-window-configuration my-ediff-saved-window-config)
    (setq my-ediff-saved-window-config nil)))

(add-hook 'ediff-before-setup-hook 'my-ediff-save-window-config)
(add-hook 'ediff-quit-hook 'my-ediff-restore-window-config)

(provide 'init-ediff)
;;; init-ediff.el ends here
