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

(provide 'bsp-work)
