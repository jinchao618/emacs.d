;;; init-plantuml.el  ---- plantuml
;;; Commentary:
;;; Code:

(require-package 'plantuml-mode)

;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; Sample jar configuration
;; (setq plantuml-jar-path "/path/to/your/copy/of/plantuml.jar")
;; (setq plantuml-default-exec-mode 'jar)

;; Sample executable configuration
;; (setq plantuml-executable-path "/path/to/your/copy/of/plantuml.bin")
(setq plantuml-default-exec-mode 'executable)

(provide 'init-plantuml)

;;; init-plantuml.el ends here
