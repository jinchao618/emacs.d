(require-package 'color-theme-sanityinc-solarized)
(require-package 'zenburn-theme)
(require-package 'anti-zenburn-theme)
(require-package 'hc-zenburn-theme)
;(require-package 'color-theme-sanityinc-tomorrow)
;(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/color-theme-sanityinc-solarized-20160429.1903")
;; if you don't customize it, this is the theme you get
;(setq-default custom-enabled-themes '(sanityinc-solarized-dark))
(setq-default custom-enabled-themes '(hc-zenburn))
;(setq-default custom-enabled-themes '(sanitynic-tomorrow-bright))

;; Ensure that themes will be applied even if they have not been customized
;; (if (display-graphic-p)
;;     ;; (enable-theme '(hc-zenburn))
;;     (enable-theme 'sanityinc-solarized-dark)
;;   (enable-theme '(sanityinc-solarized-dark)))
;; (defun reapply-themes ()
;;   "Forcibly load the themes listed in `custom-enabled-themes'."
;;   (dolist (theme custom-enabled-themes)
;;     (unless (custom-theme-p theme)
;;       (load-theme theme)))
;;   (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun reapply-themes ()
  ;; different theme for terminal and graphic
  (if (display-graphic-p)
      (load-theme 'hc-zenburn)
    (load-theme 'sanityinc-solarized-dark)))

(add-hook 'after-init-hook 'reapply-themes)

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
