;;; calcite-light-theme.el --- Distractionless light theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Distractionless light theme with minimal syntax highlighting.
;; Use `M-x list-faces-display` to check which faces are customized.

;;; Code:

(let ((themes-file (locate-file "calcite-themes.el" custom-theme-load-path)))
  (unless themes-file
    (error "Cannot find calcite-themes.el in custom-theme-load-path"))
  (load themes-file nil t))

(deftheme calcite-light
  "Distractionless light theme with minimal syntax highlighting")

;; Get light variant colors (index 0)
(let ((colors (calcite-themes-get-colors 0)))
  (calcite-themes-apply 'calcite-light colors))

(provide-theme 'calcite-light)
;;; calcite-light-theme.el ends here
