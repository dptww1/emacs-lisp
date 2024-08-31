(require 'subr-x) ; needed for string-trim

;; Fun hack, completely dependent on my diary.org org-table conventions.
;; Move point to a line within my releases table, where the first
;; column is a ticket number and the second column is a brief description.
(defun bsp-start-task (omit-time)
  (interactive "*P")
  (unless (org-at-table-p)
    (error "Cursor should be on a table with a ticket"))
  (beginning-of-line)
  (org-table-next-field)
  (let* ((ticket-id (string-trim (org-table-get-field)))
         (project-code
          (progn
            (string-match "\\w+:\\(\\w+\\)-" ticket-id)
            (match-string 1 ticket-id)))
         (ticket-desc
          (progn
            (org-table-next-field)
            (string-trim (org-table-get-field)))))
    (if (equal project-code "UT")
        (setq project-code "HG"))
    (search-forward "Work Log")
    (search-forward "*****")
    (search-forward "***")
    (previous-line)
    (insert
     (if omit-time
         "        "
       (format-time-string "%_I:%0M %^p" (bsp-round-time-to-nearest-5-min)))
     " "
     project-code
     " - "
     ticket-id
     " "
     ticket-desc
     ?\n)
    (previous-line)
    (beginning-of-line)
    (search-forward (concat project-code " - " nil nil))))

(defun bsp-round-time-to-nearest-5-min ()
  (let* ((num_secs (time-convert (current-time) 'integer))
         (five-min-modulo (mod num_secs 300)))
    (if (< five-min-modulo 150)
        (- num_secs five-min-modulo)
      (+ num_secs (- 300 five-min-modulo)))))

(defun dt/delete-json-field-safely ()
  "Deletes the current line and removes any trailing comma
on the previous line if the now-current line starts with a
`}' or `]' character, which would result in a JSON syntax error."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-region (point) (1+ (pos-eol)))
    ;; is this was the last field in an array or object,
    ;; get rid of any trailing comma on the previous line
    (if (looking-at "[[:space:]]*[]}]")
        (progn
          (previous-line)
          (replace-regexp-in-region ",[[:space:]]*$" "" (point) (pos-eol))))))

(defun bsp-format-hbs-log ()
  "Format the output of HBS `{{log}}' statement on the current line.

Useful when analyzing logged objects, whose attributes are otherwise all
mashed together, which can be difficult to read."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "{")
      (error "current line doesn't look like an HBS log line"))
    (forward-char)
    (insert "\n  ")
    (let ((indent-level 2))
      (while (> indent-level 0)
        (cond
         ((looking-at "=")
          (insert " ")
          (forward-char)
          (insert " "))
         ((looking-at ", ")
          (forward-char)
          (delete-char 1)
          (insert "\n" (make-string indent-level ? )))
         ((looking-at "{")
          (forward-char)
          (insert "\n")
          (setq indent-level (+ indent-level 2))
          (insert (make-string indent-level ? )))
         ((looking-at "}, ")
          (setq indent-level (- indent-level 2))
          (insert "\n" (make-string indent-level ? ))
          (search-forward "}," nil t)
          (forward-char)
          (insert "\n" (make-string indent-level ? )))
         ((looking-at "}$")
          (setq indent-level (- indent-level 2))
          (insert "\n" (make-string indent-level ? ))
          (search-forward "}" nil t))
         (t
          (forward-char)))))))

;;; http://irreal.org/blog/?p=354
;(defun json-format-region ()
;  (interactive)
;  (save-excursion
;    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(defun bsp-decode-enhancement (rgn-start rgn-end)
  "Convert RTE enhancement HTML string in region to something more readable.

Requires function `json-format-region` (example commented out above in the source file)
which in turn requires Python."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region rgn-start rgn-end)
      ; Unencode quotes
      (goto-char (point-min))
      (while (search-forward "&quot;" (point-max) t)
        (replace-match "\""))
      (goto-char (point-min))
      (set-mark (1- (search-forward "{")))
      (goto-char (point-max))
      (search-backward "}")
      (forward-char)
      (json-format-region))))

(define-key global-map "\C-co" 'bsp-styleguide-other-file)
(define-key global-map "\C-cy" 'bsp-styleguide-other-type)
(define-key global-map "\C-ci" 'bsp-styleguide-goto-include)
(define-key global-map "\C-ct" 'bsp-start-task)
(define-key global-map "\C-cj"
            (lambda ()
              (interactive)
              (insert "|-----+-------------+------+-----+----+------+-------|\n")
              (insert "| BUG | DESCRIPTION | DEV? | PR? | QA | PROD | NOTES |\n")
              (insert "|-----+-------------+------+-----+----+------+-------|\n")
              (insert "| \n")
              (insert "|-----+-------------+------+-----+----+------+-------|\n")))

(require 'bsp-styleguide)

(provide 'bsp-work)
