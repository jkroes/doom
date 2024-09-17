;; NOTE Functions are named alt-dendroam-* to avoid the effects of
;; dendroam-insert-display-separator

(require 'cl-lib)  ; Required for cl-block and cl-return

(defvar alt-dendroam-hierarchy-tree nil)

(defun alt-dendroam-open-note ()
  "Interactively navigate the dendroam hierarchy and open the selected file."
  (interactive)
  (setq alt-dendroam-hierarchy-tree
        (alt-dendroam--parse-filename-hierarchy-tree org-roam-directory))
  (let* ((selection (completing-read
                     "Note: "
                     #'alt-dendroam--all-completions nil #'alt-dendroam--file-p))
         (filename (alt-dendroam--expand selection)))
    (find-file filename)))

(defvar alt-dendroam--intermediate-non-note-face font-lock-warning-face)
(defvar alt-dendroam--intermediate-note-face font-lock-preprocessor-face)

(defun alt-dendroam--parse-filename-hierarchy-tree (directory)
  "Parse period-separated filenames in DIRECTORY into an alist hierarchy-tree.
The filenames are assumed to express a hierarchy from left to right,
and the file extension is removed before processing."
  ;; Sort filenames without extensions because order matters for propertizing
  ;; notes later
  (let ((names (sort (mapcar #'file-name-sans-extension
                             (directory-files directory nil "^[^.].*"))
                     #'string<))
        hierarchy-tree)
    (dolist (name names)
      (let ((components (split-string name "\\.")))
        ;; Propertize the last component of each name
        (if (seq-filter (lambda (other) (string-prefix-p name other)) (remove name names))
            (setcar (last components)
                    (propertize (car (last components)) 'face alt-dendroam--intermediate-non-note-face))
          (setq components
                (append (mapcar (lambda (comp) (propertize comp 'face alt-dendroam--intermediate-note-face))
                                (butlast components))
                        (last components))))
        (setq hierarchy-tree (alt-dendroam--insert-into-alist components hierarchy-tree))))
    hierarchy-tree))

(defun alt-dendroam--insert-into-alist (components tree)
  "Recursively insert COMPONENTS into TREE, building a hierarchy-tree.
Return the updated TREE."
  (if (null components)
      tree
    (let* ((comp (car components))
           (rest (cdr components))
           ;; Look for the existing node for this component
           (node (assoc comp tree)))
      (if node
          ;; Component exists, insert into its subtree
          (setcdr node (alt-dendroam--insert-into-alist rest (cdr node)))
        ;; Component does not exist, create a new subtree
        (setq tree (cons (cons comp (alt-dendroam--insert-into-alist rest nil)) tree))))
    tree))

(defun alt-dendroam--all-completions (input predicate action)
  (let* ((components (split-string input "\\." t))
         (show-children? (string-suffix-p "." input))
         (traversed-components (if show-children? components (butlast components)))
         ;; (tested-input (if (or (null components) show-children?) "" (car (last components))))
         (node alt-dendroam-hierarchy-tree)
         candidates)
    (dolist (comp traversed-components)
      (setq node (assoc comp node)))
    (setq candidates (mapcar #'car (cdr node)))
    (setq candidates (mapcar
                      (lambda (cand)
                        (string-join (append traversed-components (list cand)) "."))
                      candidates))
    (complete-with-action action candidates input predicate)))

(defun alt-dendroam--file-p (input)
  (let ((filename (alt-dendroam--expand input)))
    (file-exists-p filename)))

(defun alt-dendroam--expand (input)
  (expand-file-name (concat input ".org") org-roam-directory))
