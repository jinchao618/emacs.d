(add-to-list 'load-path (expand-file-name "lisp/editing-utils" user-emacs-directory))
;; enhanced help mechanism
;; (require-package 'help-fns+)
;; (require 'help-fns+)

;; dimish minor mode name to save mode line space
(require-package 'diminish)

;; some default value
(setq-default
 blink-cursor-delay 0.5
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 compilation-scroll-output t
 delete-selection-mode t
 grep-scroll-output t
 indent-tabs-mode nil
 line-spacing 0.2
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
 scroll-preserve-screen-position 'always
 scroll-step 1
 scroll-margin 3
 scroll-conservatively 10000
 set-mark-command-repeat-pop t
 show-trailing-whitespace t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 visible-bell t)
(set-scroll-bar-mode nil)
(tool-bar-mode 0)

;; (add-hook 'after-init-hook 'delete-selection-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

;; enable auto-pairing
;; (require 'init-autopair)
;; (diminish 'autopair-mode)

;; some personal key bindings
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-0") 'scroll-other-window)
(global-set-key (kbd "C-9") 'scroll-other-window-down)

;; newline behavior
;; (global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;; (global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)



;; display time
(display-time-mode t)
(setq display-time-24hr-format t)

;; display line number
;; (require 'linum)
(require-package 'hlinum)
;; (require 'hlinum)
(hlinum-activate)
(global-linum-mode t)

;; visual line
(global-visual-line-mode t)
;; (diminish 'global-visual-line-mode)
(diminish 'visual-line-mode)

;; expand-region
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; enable uppercase and lowercase transform for region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; whole-line-or-region-mode
;; (require-package 'whole-line-or-region)
;; (whole-line-or-region-mode t)
;; (diminish 'whole-line-or-region-mode)
;; (make-variable-buffer-local 'whole-line-or-region-mode)

;; enable cua mode without prefix key
(cua-selection-mode t)

;; use page-break-line to handle the ^L page-breaking symbol
(require-package 'page-break-lines)
(global-page-break-lines-mode)
(diminish 'page-break-lines-mode)

;; semantic stickyfnc enhance
(require-package 'stickyfunc-enhance)
(require-package 'srefactor)

;; xclip mode
(require-package 'xclip)
(require 'xclip)
(xclip-mode t)
;; (setq select-enable-clipboard nil)

;; enable subword-mode
;; (global-subword-mode t)

;; multiple-cursors-mode
;; (require-package 'multiple-cursors)
;; multiple-cursors
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
;; (global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
;; (global-set-key (kbd "C-c m c") 'mc/edit-lines)
;; (global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
;; (global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)

;; switch-window
;(require 'init-switch-window)
;; Set "C-x p" to select the previous window
;(defun other-window-backward (&optional n)
;  "Select the Nth previous window"
;  (interactive "P")
;  (other-window (- (prefix-numeric-value n))))
;(global-set-key "\C-xp" 'other-window-backward)


;; undo-tree
(require 'init-undo-tree)
(diminish 'undo-tree-mode)

;; outline-minor-mode
;(require 'init-outl-minor)

;; use C-u C-u C-s/r to trigger the flexible search action
;(require 'init-flex-isearch)

;; set some compilation shortcuts
;;(require 'init-compile)

;; spell checking
;; (when (executable-find "hunspell")
;;   (setq-default ispell-program-name "hunspell")
;;   (setq ispell-really-hunspell t)
;;   (require 'init-flyspell))
(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  (setq ispell-really-aspell t)
  (require 'init-flyspell)
  )

;; maximize window
(if (window-system)
    (progn
      (setq default-frame-alist
            '((width . 90)
              (height . 40)
              ;(font . "Monospace-11")
              (fullscreen . maximized)))
      (tool-bar-mode -1))
  (menu-bar-mode -1))

(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (concat
                   (file-name-nondirectory buffer-file-name)
                   " ("
                   (directory-file-name
                    (abbreviate-file-name
                     (file-name-directory buffer-file-name)))
                   ") - emacs")
                "%b")))

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
