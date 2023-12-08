(require 'subr-x) ; needed for string-trim

;; Fun hack, completely dependent on my diary.org org-table conventions.
;; Move point to a line within my releases table, where the first
;; column is a ticket number and the second column is a brief description.
(defun bsp-start-task ()
  (interactive)
  (unless (org-at-table-p)
    (error "Cursor should be on a table with a ticket"))
  (beginning-of-line)
  (org-table-next-field)
  (let* ((ticket-id (string-trim (org-table-get-field)))
         (ticket-desc
          (progn
            (org-table-next-field)
            (string-trim (org-table-get-field)))))
    (search-forward "Work Log")
    (search-forward "*****")
    (search-forward "***")
    (previous-line)
    (insert
     (format-time-string "%_I:%0M %^p" (bsp-round-time-to-nearest-5-min))
     " HG - "
     ticket-id
     " "
     ticket-desc
     ?\n)
    (previous-line)
    (beginning-of-line)
    (search-forward "HG - " nil nil)))

(defun bsp-round-time-to-nearest-5-min ()
  (let* ((num_secs (time-convert (current-time) 'integer))
         (five-min-modulo (mod num_secs 300)))
    (if (< five-min-modulo 150)
        (- num_secs five-min-modulo)
      (+ num_secs (- 300 five-min-modulo)))))

(provide 'bsp-work)
