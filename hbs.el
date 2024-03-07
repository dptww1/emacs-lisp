(require 'cl-lib)

(cl-defstruct hbs-tag name start-pos end-pos)

(defun hbs-add-overlay (tag)
  (let ((ov (make-overlay (hbs-tag-start-pos tag) (hbs-tag-end-pos tag) (current-buffer) t)))
    (overlay-put ov 'face '(foreground-color . "red"))))

(defun hbs-handle-start-tag (stack tag)
  (push tag stack))

(defun hbs-handle-end-tag (stack tag)
  (cond
   ((null stack)
    (hbs-add-overlay tag)
    nil)
   ((equal (hbs-tag-name tag) (hbs-tag-name (car stack)))
    (cdr stack))
   (t
    (hbs-add-overlay tag)
    ;; need better logic here but for now...
    (cdr stack))))

(defun hbs-syntax-check-next-tag (symbol-stack start-pos)
  (save-excursion
    (goto-char start-pos)
    ;; tags in comments can fool this....
    (if (re-search-forward "{{{?~?\\(#\\|/\\)\\([a-z]+\\).*?}}}?" nil t)
        (progn
          (let ((end-tag-p (equal (match-string-no-properties 1) "/"))
                (tag (make-hbs-tag
                      :name (match-string-no-properties 2)
                      :start-pos (match-beginning 0)
                      :end-pos (match-end 0))))
            (setq symbol-stack
                  (if end-tag-p
                      (hbs-handle-end-tag symbol-stack tag)
                    (hbs-handle-start-tag symbol-stack tag))))
          (list symbol-stack (point))))))

(defun hbs-syntax-check ()
  (remove-overlays)
  (setq-local stack ()
              start-pos (point-min)
              parse-state t)
  (while (and parse-state (not (input-pending-p)))
    (setq parse-state (hbs-syntax-check-next-tag stack start-pos))
    (if parse-state
        ;;(cl-multiple-value-bind (stack start-pos) parse-state)) ; locks up for some reason
        (progn
          (setq stack (car parse-state)
                start-pos (cadr parse-state))))))

(defvar hbs-syntax-check-timer nil)

(defun hbs-syntax-check-start ()
  (interactive)
  (setq hbs-syntax-check-timer (run-with-idle-timer 1 t 'hbs-syntax-check)))

(defun hbs-syntax-check-stop ()
  (interactive)
  (when (timerp hbs-syntax-check-timer)
    (cancel-timer hbs-syntax-check-timer)
    (setq hbs-syntax-check-timer nil)))


{{fluds}}
{{#if Fdfdfoo}}
{{ex}}
{{~#with expo}}
   {{#nested}}
   {{/nested}}
foosdf
{{/with}}
{{/if}}
