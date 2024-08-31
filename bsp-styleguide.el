(require 'cl-extra)

(defvar bsp-styleguide-roots
  '(("ucms-app" ("/styleguide" . "/themes/hg-ucms-theme-default/styleguide"))
    ("dispatch-health" ("/frontend/styleguide" . "/frontend/bundles/dispatch-health-bundle-default/styleguide"))
    ("syndsearch-app" ("styleguide" . "/themes/hg-search/theme/default/styleguide")))
  "\
List of string / cons cells pairs. The strings are project root names
and the corresponding cons cells are pairs of paths relative to the
project root giving the styleguide and theme directories for that
project.  (The order doesn't matter.)

This unfortunately doesn't work for multi-theme projects.")

(defun -bsp-styleguide-get-project-root-path ()
  (if (not buffer-file-name)
      (error "No file for this buffer"))

  (let ((root (locate-dominating-file buffer-file-name ".git")))
    (if (not root)
        (error "Not in a project directory"))
    root))

(defun -bsp-styleguide-get-root-cell (root-path)
  (let ((root-dirname (file-name-nondirectory (directory-file-name root-path))))
    (cl-some
     (lambda (root-list)
       (if (equal (car root-list) root-dirname)
           (cadr root-list)
         nil))
     bsp-styleguide-roots)))

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

(defun bsp-styleguide-other-file (where)
  "Visits the styleguide/theme file shadowing the current buffer.\
\
If in styleguide, visits the theme file with the same path, or\
vice versa.\
\
The variable `bsp-styleguide-roots` must be configured to\
defines the root styleguide and theme directories for the project.

With no prefix argument, visits the file in the current window.

With prefix argument `4', visits the file in a new window.

With prefix argument `5', visits the file in a new frame."
  (interactive "p")
  (let ((root (-bsp-styleguide-get-normalized-root-cell)))
    (if (not root)
        (error "Not in a configured project directory"))

    (let ((other-file-name (string-replace (car root) (cdr root) buffer-file-name)))
      (if (file-exists-p other-file-name)
          (-bsp-styleguide-find-file other-file-name where)
        (if (y-or-n-p (format "File %s doesn't exist; should I create it?" other-file-name))
            (progn
             (make-directory (file-name-directory other-file-name) t)
             (write-region nil nil other-file-name)
             (-bsp-styleguide-find-file other-file-name where)
             (message (format "Created %s" other-file-name))))))))

(defun bsp-styleguide-other-type (where)
  "Visit the companion HBS or JSON file for the current buffer.

With no prefix argument, visits the file in the current window.

With prefix argument `4', visits the file in a new window.

With prefix argument `5', visits the file in a new frame."
  (interactive "p")
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
    (-bsp-styleguide-find-file other-file-name where)))

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

(defun bsp-styleguide-goto-include (where)
  "Visits the file on the current line, if that line is a
JSON \"_include\", JSON \"_template\", JSON \"_styledTemplate\",
or HBS {{include}}, optionally creating it if it doesn't already exist.

With no prefix argument, visits the file in the current window.

With prefix argument `4', visits the file in a new window.

With prefix argument `5', visits the file in a new frame."
  (interactive "p")
  (let ((include-file-path
         (cond
          ((string-suffix-p ".json" (buffer-file-name))
           (-bsp-styleguide-parse-include-path "\"\\(_include\\|_template\\|_styledTemplate\\)\": \""))
          ((string-suffix-p ".hbs" (buffer-file-name))
           (-bsp-styleguide-parse-include-path "{{~*include \"")))))
    (if (file-exists-p include-file-path)
        (-bsp-styleguide-find-file include-file-path where)
      (if (y-or-n-p (format "File %s doesn't exist; should I create it?" include-file-path))
          (progn
            (make-directory (file-name-directory include-file-path) t)
            (write-region "" nil include-file-path)
            (-bsp-styleguide-find-file include-file-path where)
            (message "Created %s" include-file-path))))))

(defun -bsp-styleguide-find-file (filename where)
  (cond
   ((= 4 where)
    (find-file-other-window filename))
   ((= 5 where)
    (find-file-other-frame filename))
   (t
    (find-file filename))))

(defun bsp-styleguide-convert-to-styled-template ()
  "Convert an \"_include\" on the current line to a \"_styledTemplate\"."
  (interactive)
  (save-excursion
    (let ((b (line-beginning-position))
          (e (line-end-position)))
      (replace-string-in-region ".json" ".hbs" b e)
      (unless (replace-regexp-in-region "_include\\|_template" "_styledTemplate" b e)
        (error "No _include on current line!")))))

(provide 'bsp-styleguide)
