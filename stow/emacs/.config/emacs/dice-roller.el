;;; dice-roller.el --- TTRPG dice roller with roll history -*- lexical-binding: t; -*-

;;; Commentary:
;; Dice roller for TTRPG sessions.  Accepts standard dice notation such as
;; `1d20+5', `2d6+1d4+3', or `1d100-10'.  Each roll is appended to a
;; persistent side-window buffer so recent results stay visible while you
;; work.
;;
;; Usage:
;;   M-x dice-roll   (or the bound key, default C-c d)
;;
;; Buffer keybindings (dice-roller-mode):
;;   r   new roll (minibuffer prompt)
;;   R   re-roll last expression
;;   c   clear history (y-or-n-p confirm)
;;   w   copy last total to kill-ring
;;   n/p jump to next/prev result block
;;   q   bury buffer (inherited from special-mode)

;;; Code:

;;;; State

(defvar dnl/dice-roller--last-expression nil
  "Last dice notation string that was rolled.")

(defvar dnl/dice-roller--last-total nil
  "Integer total of the last roll.")

;;;; Parser

(defun dnl/dice-roller--parse (expression)
  "Parse dice EXPRESSION string into a list of term plists.
Each plist has keys :notation :count :sides :constant :sign.
Signals `user-error' on invalid input."
  (let ((str (string-trim expression))
        (pos 0)
        (sign 1)
        terms)
    (when (string-empty-p str)
      (user-error "Empty dice expression"))
    ;; Reject letters that aren't 'd'
    (when (string-match "[a-ce-z]" str)
      (user-error "Invalid dice expression: %s" expression))
    (while (< pos (length str))
      (cond
       ;; Skip whitespace
       ((= (aref str pos) ?\s)
        (cl-incf pos))
       ;; Sign operator
       ((and (> pos 0) (or (= (aref str pos) ?+) (= (aref str pos) ?-)))
        (setq sign (if (= (aref str pos) ?+) 1 -1))
        (cl-incf pos)
        ;; Reject double operators
        (when (and (< pos (length str))
                   (or (= (aref str pos) ?+) (= (aref str pos) ?-)))
          (user-error "Double operator in dice expression: %s" expression)))
       ;; Dice term: [N]dS
       ((string-match "\\([0-9]*\\)d\\([0-9]+\\)" str pos)
        (when (/= (match-beginning 0) pos)
          (user-error "Unexpected characters in dice expression: %s" expression))
        (let* ((count-str (match-string 1 str))
               (sides-str (match-string 2 str))
               (count (if (string-empty-p count-str) 1 (string-to-number count-str)))
               (sides (string-to-number sides-str)))
          (when (= sides 0)
            (user-error "Dice must have at least 1 side: d0 is invalid"))
          (push (list :notation (match-string 0 str)
                      :count count
                      :sides sides
                      :constant nil
                      :sign sign)
                terms)
          (setq pos (match-end 0))
          (setq sign 1)))
       ;; Constant term
       ((string-match "\\([0-9]+\\)" str pos)
        (when (/= (match-beginning 0) pos)
          (user-error "Unexpected characters in dice expression: %s" expression))
        ;; Make sure this isn't actually the start of a dice term (e.g. "2d6")
        (let ((num-end (match-end 0)))
          (if (and (< num-end (length str)) (= (aref str num-end) ?d))
              ;; It's a dice term — let the cond retry at same pos but now the
              ;; dice branch will match because we check "Nd" explicitly.
              ;; Actually the dice regex above should have matched already;
              ;; if we reach here something is wrong.
              (user-error "Invalid dice expression: %s" expression)
            (push (list :notation (match-string 1 str)
                        :count nil
                        :sides nil
                        :constant (string-to-number (match-string 1 str))
                        :sign sign)
                  terms)
            (setq pos num-end)
            (setq sign 1))))
       (t
        (user-error "Invalid character in dice expression at position %d: %s"
                    pos expression))))
    (when (null terms)
      (user-error "No valid terms found in dice expression: %s" expression))
    (nreverse terms)))

;;;; Roller

(defun dnl/dice-roller--roll-term (term)
  "Roll a single parsed TERM plist.  Returns updated plist with :rolls and :subtotal."
  (if (plist-get term :constant)
      (let ((val (* (plist-get term :sign) (plist-get term :constant))))
        (append term (list :rolls nil :subtotal val)))
    (let* ((count (plist-get term :count))
           (sides (plist-get term :sides))
           (sign  (plist-get term :sign))
           (rolls (mapcar (lambda (_) (1+ (random sides))) (make-list count nil)))
           (subtotal (* sign (apply #'+ rolls))))
      (append term (list :rolls rolls :subtotal subtotal)))))

(defun dnl/dice-roller--evaluate (expression)
  "Parse and roll EXPRESSION.  Returns result plist with :terms :total :expression."
  (let* ((terms   (dnl/dice-roller--parse expression))
         (rolled  (mapcar #'dnl/dice-roller--roll-term terms))
         (total   (apply #'+ (mapcar (lambda (t) (plist-get t :subtotal)) rolled))))
    (list :expression expression :terms rolled :total total)))

;;;; Formatter

(defun dnl/dice-roller--format-result (result)
  "Format a RESULT plist into a display string."
  (let* ((expression (plist-get result :expression))
         (terms      (plist-get result :terms))
         (total      (plist-get result :total))
         (timestamp  (format-time-string "%H:%M"))
         (sep        (propertize (make-string 36 ?─) 'face 'shadow))
         lines)
    ;; Header line: timestamp + expression
    (push (concat (propertize (format "[%s] " timestamp) 'face 'font-lock-comment-face)
                  expression)
          lines)
    ;; One line per term
    (dolist (term terms)
      (if (plist-get term :constant)
          (let* ((sign (plist-get term :sign))
                 (val  (plist-get term :constant))
                 (prefix (if (< sign 0) "  -" "  +")))
            (push (format "%s%d" prefix val) lines))
        (let* ((notation (plist-get term :notation))
               (rolls    (plist-get term :rolls))
               (subtotal (plist-get term :subtotal))
               (sign     (plist-get term :sign))
               (rolls-str (propertize
                           (concat "[" (mapconcat #'number-to-string rolls ", ") "]")
                           'face 'shadow))
               (prefix (if (< sign 0) "  -" "   ")))
          (push (format "%s%-6s %s = %d"
                        prefix
                        (if (< sign 0)
                            (substring notation) ; remove leading sign if present
                          notation)
                        rolls-str
                        (abs subtotal))
                lines))))
    ;; Separator and total
    (push (concat "  " sep) lines)
    (push (concat "  " (propertize (format "Total: %d" total) 'face 'bold)) lines)
    ;; Empty line after block
    (push "" lines)
    (mapconcat #'identity (nreverse lines) "\n")))

;;;; Buffer management

(defun dnl/dice-roller--get-or-create-buffer ()
  "Return the *Dice Roller* buffer, creating and initialising it if needed."
  (let ((buf (get-buffer "*Dice Roller*")))
    (unless buf
      (setq buf (get-buffer-create "*Dice Roller*"))
      (with-current-buffer buf
        (dice-roller-mode)))
    buf))

(defun dnl/dice-roller--display-buffer (buf)
  "Display BUF in a right side window without stealing focus."
  (display-buffer-in-side-window
   buf
   '((side . right)
     (window-width . 45)
     (slot . 0))))

(defun dnl/dice-roller--append-result (result)
  "Append formatted RESULT to the *Dice Roller* buffer."
  (let* ((buf (dnl/dice-roller--get-or-create-buffer))
         (text (dnl/dice-roller--format-result result)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (at-end (= (point) (point-max))))
        (goto-char (point-max))
        (insert text "\n")
        (when at-end
          (goto-char (point-max)))))
    ;; Scroll the side window to end without changing selected window
    (let ((win (get-buffer-window buf)))
      (when win
        (with-selected-window win
          (goto-char (point-max)))))))

;;;; Navigation helpers

(defun dnl/dice-roller--next-result ()
  "Move point to the next result separator line."
  (interactive)
  (if (search-forward "──" nil t)
      (beginning-of-line)
    (message "No more results")))

(defun dnl/dice-roller--prev-result ()
  "Move point to the previous result separator line."
  (interactive)
  (let ((start (point)))
    (beginning-of-line)
    (if (search-backward "──" nil t)
        (beginning-of-line)
      (goto-char start)
      (message "No previous results"))))

;;;; Public commands

(defun dnl/dice-roller-reroll-last ()
  "Re-roll the last dice expression."
  (interactive)
  (if dnl/dice-roller--last-expression
      (dnl/dice-roll dnl/dice-roller--last-expression)
    (user-error "No previous roll to repeat")))

(defun dnl/dice-roller-clear-history ()
  "Clear the *Dice Roller* buffer after confirmation."
  (interactive)
  (when (y-or-n-p "Clear dice roll history? ")
    (let ((buf (get-buffer "*Dice Roller*")))
      (when buf
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)))))))

(defun dnl/dice-roller-copy-total ()
  "Copy the last roll total to the kill ring."
  (interactive)
  (if dnl/dice-roller--last-total
      (progn
        (kill-new (number-to-string dnl/dice-roller--last-total))
        (message "Copied %d to kill ring" dnl/dice-roller--last-total))
    (user-error "No roll total to copy yet")))

;;;; Mode

(defvar-keymap dice-roller-mode-map
  :doc "Keymap for `dice-roller-mode'."
  "r" #'dnl/dice-roll
  "R" #'dnl/dice-roller-reroll-last
  "c" #'dnl/dice-roller-clear-history
  "w" #'dnl/dice-roller-copy-total
  "n" #'dnl/dice-roller--next-result
  "p" #'dnl/dice-roller--prev-result)

(define-derived-mode dice-roller-mode special-mode "Dice"
  "Major mode for the *Dice Roller* history buffer.

\\{dice-roller-mode-map}"
  :interactive nil
  (setq buffer-read-only t
        truncate-lines t))

;;;; Entry point

;;;###autoload
(defun dnl/dice-roll (expression)
  "Roll EXPRESSION (standard dice notation) and display the result.
Prompts for EXPRESSION interactively.  Results accumulate in a
right-side *Dice Roller* buffer that does not steal focus."
  (interactive
   (list (read-string
          (if dnl/dice-roller--last-expression
              (format "Dice notation [%s]: " dnl/dice-roller--last-expression)
            "Dice notation: ")
          nil nil dnl/dice-roller--last-expression)))
  (when (string-empty-p (string-trim expression))
    (user-error "No dice expression provided"))
  (let ((result (dnl/dice-roller--evaluate expression)))
    (setq dnl/dice-roller--last-expression expression
          dnl/dice-roller--last-total (plist-get result :total))
    (let ((buf (dnl/dice-roller--get-or-create-buffer)))
      (dnl/dice-roller--display-buffer buf)
      (dnl/dice-roller--append-result result))
    (message "%s → %d" expression dnl/dice-roller--last-total)))

(defalias 'dice-roll #'dnl/dice-roll)

(provide 'dice-roller)
;;; dice-roller.el ends here
