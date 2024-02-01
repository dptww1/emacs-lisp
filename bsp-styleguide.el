(require 'cl-extra)

(defvar bsp-styleguide-roots
      '(("/styleguide" . "/themes/hg-ucms-theme-default/styleguide") ; Healthgrades
        ("/frontend/styleguide" . "/frontend/bundles/dispatch-health-bundle-default/styleguide")) ; DispatchHealth
      "\
List of cons cells, pairing root styleguide and root theme directories.\
\
Each unique project directory structure requires its own cons cell\
in the list.  The paths within the cons cell should be relative\
to the project root directory.  It doesn't matter which path\
(styleguide or theme) is in the car and which is in the cdr.")

(defun -bsp-styleguide-get-project-root-path ()
  (if (not buffer-file-name)
      (error "No file for this buffer"))

  (let ((root (locate-dominating-file buffer-file-name ".git")))
    (if (not root)
        (error "Not in a project directory"))
    root))

(defun -bsp-styleguide-get-root-cell (root-path)
  (cl-some
   (lambda (cell)
     (let ((path-lhs (expand-file-name (concat root-path (car cell))))
           (path-rhs (expand-file-name (concat root-path (cdr cell)))))
       (cond
        ((string-prefix-p path-lhs buffer-file-name)
         cell)
        ((string-prefix-p path-rhs buffer-file-name)
         cell)
        (t nil))))
   bsp-styleguide-roots))

(defun -bsp-styleguide-get-normalized-root-cell ()
  (let* ((root-path (-bsp-styleguide-get-project-root-path))
         (root-cell (-bsp-styleguide-get-root-cell root-path))
         (path-lhs (expand-file-name (concat root-path (car root-cell))))
         (path-rhs (expand-file-name (concat root-path (cdr root-cell)))))
    (if (string-prefix-p path-lhs buffer-file-name)
        root-cell
      (cons (cdr root-cell) (car root-cell)))))

;;; TODO: Only works on styleguide .json file, not theme .json file
(defun bsp-styleguide-add-to-modules-json ()
  "If current buffer is a JSON file in styleguide, or if it's in the theme \
directory and has a matching file in in the styleguide, add its name to \
the styleguide /group/_Modules.json, if it's not already there."
  (interactive)
  (let* ((root-path (-bsp-styleguide-get-project-root-path))
         (root-cell (-bsp-styleguide-get-root-cell root-path))
         (styleguide-path-part
           (cond
            ((< (length (car root-cell)) (length (cdr root-cell)))
             (car root-cell))
            (t (cdr root-cell))))
         (styleguide-path (concat root-path styleguide-path-part))
         (modules-json-file-name
          (expand-file-name (concat styleguide-path "/_group/Modules.json")))
         (current-buffer-relative-path
          (concat "/" (file-relative-name (buffer-file-name) styleguide-path))))
    (find-file-other-window modules-json-file-name)
    (beginning-of-buffer)
    (if (search-forward (concat "\"" current-buffer-relative-path "\"") nil t)
        (message (concat current-buffer-relative-path " already present"))
      (end-of-buffer)
      (search-backward "}")
      (forward-char)
      (insert ",")
      (newline)
      (insert "  {")
      (newline)
      (insert "    \"_include\": \"" current-buffer-relative-path "\"")
      (newline)
      (insert "  }")
      (message (concat "Added " current-buffer-relative-path)))))

(defun bsp-styleguide-other-file ()
  "Visits the styleguide/theme file shadowing the current buffer.\
\
If in styleguide, visits the theme file with the same name, or\
vice versa.\
\
The variable `bsp-styleguide-roots` must be configured to\
defines the root styleguide and theme directories for the project."
  (interactive)
  (let ((root (-bsp-styleguide-get-normalized-root-cell)))
    (if (not root)
        (error "Not in a configured project directory"))

    (let ((other-file-name (string-replace (car root) (cdr root) buffer-file-name)))
      (if (file-exists-p other-file-name)
          (find-file-other-window other-file-name)
        (if (y-or-n-p (format "File %s doesn't exist; should I create it?" other-file-name))
            (progn
             (make-directory (file-name-directory other-file-name) t)
             (write-region nil nil other-file-name)
             (find-file-other-window other-file-name)
             (message (format "Created %s" other-file-name))))))))

(defun bsp-styleguide-other-type ()
  "Visit the companion HBS or JSON file for the current buffer."
  (interactive)
  (if (not buffer-file-name)
      (error "No file for this buffer"))
  (let ((other-file-name
         (cond
          ((string-suffix-p ".hbs" buffer-file-name)
           (string-replace ".hbs" ".json" buffer-file-name))
          ((string-suffix-p ".json" buffer-file-name)
           (string-replace ".json" ".hbs" buffer-file-name))
          (t
           (error "Not an .hbs or .json file")))))
    (find-file-other-window other-file-name)))

(defun -bsp-styleguide-parse-include-path (prefix-regexp)
  (let* ((project-root (-bsp-styleguide-get-project-root-path))
         (styleguide-root (concat project-root (car (-bsp-styleguide-get-normalized-root-cell)))))
    (save-excursion
      (beginning-of-line)
      (unless (re-search-forward prefix-regexp (line-end-position) t)
        (error "No %s on current line!" prefix-regexp))
      (let ((path-start (point)))
        (unless (search-forward "\"" (line-end-position) t)
          (error "No terminating double quote on include path!"))
        (let ((path (buffer-substring path-start (1- (point)))))
          (if (string-prefix-p "/" path) ; absolute(ish) path?
              (expand-file-name (concat styleguide-root path))
            (concat (file-name-directory (buffer-file-name)) path)))))))

(defun bsp-styleguide-goto-include ()
  "Visits the file on the current line, if that line is a \
JSON \"_include\", JSON \"_template\", or HBS {{include}}, \
creating it if it doesn't already exist."
  (interactive)
  (let ((include-file-path
         (cond
          ((string-suffix-p ".json" (buffer-file-name))
           (-bsp-styleguide-parse-include-path "\"\\(_include\\|_template\\)\": \""))
          ((string-suffix-p ".hbs" (buffer-file-name))
           (-bsp-styleguide-parse-include-path "{{~*include \"")))))
    (if (file-exists-p include-file-path)
        (find-file-other-window include-file-path)
      (if (y-or-n-p (format "File %s doesn't exist; should I create it?" include-file-path))
          (progn
            (make-directory (file-name-directory include-file-path) t)
            (write-region "" nil include-file-path)
            (find-file-other-window include-file-path)
            (message "Created %s" include-file-path))))))

(provide 'bsp-styleguide)
