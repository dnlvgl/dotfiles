;;; calcite-dark-theme.el --- Distractionless dark theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Distractionless dark theme with minimal syntax highlighting.
;; Use `M-x list-faces-display` to check which faces are customized.

;;; Code:

(let ((themes-file (locate-file "calcite-themes.el" custom-theme-load-path)))
  (unless themes-file
    (error "Cannot find calcite-themes.el in custom-theme-load-path"))
  (load themes-file nil t))

(deftheme calcite-dark
  "Distractionless dark theme with minimal syntax highlighting")

;; Get dark variant colors (index 1)
(let ((colors (calcite-themes-get-colors 1)))
  (calcite-themes-apply 'calcite-dark colors))

(provide-theme 'calcite-dark)
;;; calcite-dark-theme.el ends here
