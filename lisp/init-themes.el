(require-package 'color-theme-sanityinc-solarized)
(require-package 'zenburn-theme)
(require-package 'anti-zenburn-theme)
(require-package 'hc-zenburn-theme)
;(require-package 'color-theme-sanityinc-tomorrow)
;(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/color-theme-sanityinc-solarized-20160429.1903")
;; themes list, 1st is for graphic, 2nd is for terminal
;; (setq-default custom-enabled-themes '(hc-zenburn sanityinc-solarized-dark))
(setq-default custom-enabled-themes '(hc-zenburn hc-zenburn))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun reapply-themes-new ()
  ;; different theme for terminal and graphic
  (if (display-graphic-p)
      (load-theme (nth 0 custom-enabled-themes))
    (load-theme (nth 1 custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes-new)

;; ;; Toggle between light and dark
;; (defun light ()
;;   "Activate a light color theme."
;;   (interactive)
;;   (color-theme-sanityinc-solarized-light))

;; (defun dark ()
;;   "Activate a dark color theme."
;;   (interactive)
;;   (color-theme-sanityinc-solarized-dark))


(provide 'init-themes)
