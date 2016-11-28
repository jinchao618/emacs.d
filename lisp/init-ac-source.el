;; add ac-sources for latex mode
(require-package 'ac-math)
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)
(defun ac-latex-mode-setup ()
  (setq ac-sources
        (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                ac-sources))
  )
(add-hook 'latex-mode-hook 'ac-latex-mode-setup)
(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

;; add ac-source for clang
;; (require-package 'auto-complete-clang)
;; (require 'auto-complete-clang)
;; (setq ac-clang-flags
;;       (append '("-std=c++11")
;;               (mapcar (lambda (item) (concat "-I" item))
;;                       (split-string
;;                        "
;;  /usr/include/c++/6.2.1
;;  /usr/include/c++/6.2.1/x86_64-pc-linux-gnu
;;  /usr/include/c++/6.2.1/backward
;;  /usr/lib/gcc/x86_64-pc-linux-gnu/6.2.1/include
;;  /usr/local/include
;;  /usr/lib/gcc/x86_64-pc-linux-gnu/6.2.1/include-fixed
;;  /usr/include
;; "
;;                        ))))
;; (defun my-ac-cc-mode-setup ()
;;   (setq ac-sources (append '(ac-source-clang) ac-sources)))
;; (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

(provide 'init-ac-source)
