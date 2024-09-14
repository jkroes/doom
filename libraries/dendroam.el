;;; libraries/dendroam.el -*- lexical-binding: t; -*-

;; This code is based on https://github.com/vicrdguez/dendroam/blob/main/dendroam.el

;; Functions defined via cl-defgeneric, as in vertico.el, can be extended
;; through cl-defmethod, similar to how advice works. See:
;; - https://github.com/minad/vertico/wiki
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Generic-Functions.html
;; - https://www.gnu.org/software/emacs/manual/html_node/eieio/index.html
;; This macro is also used below to dispatch on org-roam-node structs:
;; - https://nullprogram.com/blog/2018/02/14/ and
;; - https://www.orgroam.com/manual.html#Accessing-and-Modifying-Nodes

(require 'org-roam)
(require 'citar)
(require 'citar-org-roam)

(defvar dendroam-separator ".")

(defvar dendroam-display-separator
  (propertize org-eldoc-breadcrumb-separator 'face 'shadow 'rear-nonsticky t))

(defvar dendroam-hidden-tags
  (list org-archive-tag org-attach-auto-tag))

(setq org-roam-node-display-template "${dendroam-display-hierarchy}")

(setq org-roam-capture-templates
      '(("d" "dendroam" plain "%?"
         :target (file+head
                  "${dendroam-slug}.org" "#+title: %(car (last (dendroam-split \"${dendroam-slug}\")))")
         :immediate-finish t)))

;;; Convert minibuffer input to title for new node -------------------------------

(cl-defmethod org-roam-node-dendroam-slug ((node org-roam-node))
  "For use in `org-roam-capture-templates'. Return the node title
with non-alphanumeric characters replaced with underscores,
except for periods, spaces, and dashes. Additionally replace
`dendroam-display-separator' with `dendroam-separator'."
  (let* ((title (org-roam-node-title node))
         (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                            768 ; U+0300 COMBINING GRAVE ACCENT
                            769 ; U+0301 COMBINING ACUTE ACCENT
                            770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                            771 ; U+0303 COMBINING TILDE
                            772 ; U+0304 COMBINING MACRON
                            774 ; U+0306 COMBINING BREVE
                            775 ; U+0307 COMBINING DOT ABOVE
                            776 ; U+0308 COMBINING DIAERESIS
                            777 ; U+0309 COMBINING HOOK ABOVE
                            778 ; U+030A COMBINING RING ABOVE
                            779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                            780 ; U+030C COMBINING CARON
                            795 ; U+031B COMBINING HORN
                            803 ; U+0323 COMBINING DOT BELOW
                            804 ; U+0324 COMBINING DIAERESIS BELOW
                            805 ; U+0325 COMBINING RING BELOW
                            807 ; U+0327 COMBINING CEDILLA
                            813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                            814 ; U+032E COMBINING BREVE BELOW
                            816 ; U+0330 COMBINING TILDE BELOW
                            817 ; U+0331 COMBINING MACRON BELOW
                            )))
    (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
               (strip-nonspacing-marks (s) (string-glyph-compose
                                            (apply #'string
                                                   (seq-remove #'nonspacing-mark-p
                                                               (string-glyph-decompose s)))))
               (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `((,dendroam-display-separator . ,dendroam-separator)
                      ("[^[:alnum:][:digit:][:space:].-]" . "_")
                      ("__*" . "_")   ; remove sequential underscore
                      ("^_*" . "")    ; remove starting underscore
                      ("_*$" . "")    ; remove ending underscore
                      ))
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        slug))))

;;; Annotations (searchable tags) ------------------------------------------------

(setq org-roam-node-annotation-function #'my/org-roam-node-read--annotation)

(defun my/org-roam-node-read--annotation (node)
  "Replaces org-roam's dummy annotation function for org-roam-node-read"
  (let ((tags (org-roam-node-dendroam-tags node)))
    (when tags
      (concat
       " :"
       (mapconcat
        (lambda (tag)
          (propertize tag 'face '(:inherit org-tag :box nil)))
        tags ":")
       ":"))))

(cl-defmethod org-roam-node-dendroam-tags ((node org-roam-node))
  "When this function is used in `org-roam-node-display-template',
node: tags will be displayed and searchable unless they are
explicitly excluded here. To exclude nodes by tag, see
`org-roam-db-node-include-function'"
  (cl-remove-if (doom-rpartial
                 #'member
                 (delq nil (ensure-list dendroam-hidden-tags)))
                (org-roam-node-tags node)))

;;; Smarter vertico candidates and minibuffer input ------------------------------

(defvar my-last-minibuffer-command nil
  "The last command that invoked the minibuffer.")

;; Shorten candidates as you type matching prefixes. Prefixes can match
;; anywhere in the hierarchy.

(cl-defmethod vertico--setup :before ()
  (setq my-last-minibuffer-command (symbol-name this-command)))

;; NOTE For an alternative implementation: replace ".*" with "^" to match from
;; the start of the candidate
(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index _start
        &context ((not (string-match-p "^dendroam-" my-last-minibuffer-command))
                  null))
  "Trim candidate string from its start to `vertico--input' when the
latter ends with `dendroam-display-separator'. Input can include
regexp characters. This method is dispatched when the current
command is part of the dendroam library."
  (let ((parent (dendroam-display-up-hierarchy (car vertico--input))))
    (when (not (string-empty-p parent))
      (setq cand (replace-regexp-in-string
                  (concat ".*" parent (regexp-quote dendroam-display-separator))
                  ""
                  cand))))
  (cl-call-next-method cand prefix suffix index _start))

;; Insert `dendroam-display-separator' when typing `dendroam-separator'

(map! :map vertico-map "." #'dendroam-insert-display-separator)

(defun dendroam-insert-display-separator ()
  (interactive)
  (if (string-match-p "^dendroam-" my-last-minibuffer-command)
      (insert dendroam-display-separator)
    (call-interactively #'self-insert-command)))

;; Delete `dendroam-display-separator' preceding point

(let ((cmd (lookup-key vertico-map (kbd "DEL"))))
  (define-key vertico-map (kbd "DEL")
              (cmd! (let ((separator-length (length dendroam-display-separator)))
                      (if (and (string-match-p "^dendroam-" my-last-minibuffer-command)
                               (>= (point) separator-length)
                               (string= (buffer-substring-no-properties
                                         (- (point) separator-length) (point))
                                        dendroam-display-separator))
                          (delete-region (- (point) separator-length) (point))
                        (call-interactively cmd))))))

;;; Displaying nodes ----------------------------------------------------------

(cl-defmethod org-roam-node-dendroam-display-hierarchy ((node org-roam-node))
  (replace-regexp-in-string (regexp-quote dendroam-separator)
                            dendroam-display-separator
                            (org-roam-node-dendroam-hierarchy node)))

(cl-defmethod org-roam-node-dendroam-hierarchy ((node org-roam-node))
  (let* ((level (org-roam-node-level node))
         ;; Assumes there is only one alias for citar notes
         (alias (car-safe (org-roam-node-aliases node)))
         ;; title is either a file title or a heading. When level is not 0, it
         ;; is a heading
         (title (org-roam-node-title node))
         ;; outline path to a heading node
         (olp (dendroam-join (org-roam-node-olp node)))
         (file-first (dendroam-join
                      (butlast (dendroam-split
                                (file-name-base (org-roam-node-file node))))))
         ;; Prefer alias or file title over final component of hierarchy
         (file-last (or alias (org-roam-node-file-title node))))
    (cl-case level
      (0 (dendroam-join-strings file-first file-last))
      (1 (dendroam-join-strings file-first file-last title))
      (t (dendroam-join-strings file-first file-last olp title)))))

;;; Capturing nodes from anywhere ------------------------------------------------

(defun dendroam-find ()
  "Find dendroam nodes. These are org-roam nodes, excluding citar
reference notes."
  (interactive)
  (org-roam-node-find nil nil (lambda (node) (not (dendroam--citar-note-p node)))))

(defun dendroam-find-master-scratch ()
  "Create an entry in scratch.org"
  (interactive)
  (org-roam-capture-
   :node (org-roam-node-create :title "")
   :templates '(("s" "scratch" entry "* %<%Y-%m-%d.%H:%M:%S>\n%?"
                 :target (file+head "scratch.org" "#+title: scratch\n\n")
                 :prepend t
                 :empty-lines-after 1))))


(defun dendroam-find-scratch ()
  "Create an entry in a scratch file derived from the
selected node. Initial input defaults to the current node."
  (interactive)
  (dendroam--find
   "scratch"
   '("s" "scratch" entry "* %<%Y-%m-%d.%H:%M:%S>\n%?"
     :prepend t
     :empty-lines-after 1)))

(defun dendroam-find-meeting ()
  "Create a meeting file derived from the selected node. Initial
input defaults to the current node."
  (interactive)
  (dendroam--find
   (format-time-string "%Y%m%d")
   '("m" "meeting" plain ""
      :immediate-finish t
      :jump-to-captured t)))

(defun dendroam--find (title template)
  (interactive)
  (let* ((selected-node (org-roam-node-read
                         (when (org-roam-file-p (buffer-file-name))
                           (org-roam-node-dendroam-display-hierarchy
                            (org-roam-node-at-point)))
                         #'dendroam--exclude-nodes
                         #'dendroam-sort-by-display-hierarchy
                         t))
         ;; Filename should be used instead of the hierarchy here
         (selected-file (file-name-sans-extension (org-roam-node-file selected-node)))
         (slug (org-roam-node-slug selected-node)))
    (org-roam-capture-
     :node (org-roam-node-create :title title)
     :templates (list
                 (append
                  template
                  `(:target (file+head ,(concat (dendroam-join-strings selected-file title) ".org")
                                       "#+title: ${title}\n\n")))))))

;;; Node navigation --------------------------------------------------------------------

;; NOTE Using cl-defmethod instead of defun means an error message will be
;; thrown if the user tries to invoke this on a normal org file that is not an
;; org-roam node. You could use a single defun and additionally test whether
;; org-roam-node-at-point returns nil.

(defun dendroam-find-parent ()
  "Find and visit parent node, creating one if nonexistent.
This is a convenience function that skips a prompt."
  (interactive)
  (if (eq major-mode 'org-mode)
      (dendroam--find-parent (org-roam-node-at-point))
    (message "%s only works within org-mode." this-command)))

(cl-defmethod dendroam--find-parent ((node org-roam-node))
  (let* ((file (org-roam-node-file node))
         (dir (f-dirname file))
         (ext (f-ext file))
         (parent-hierarchy (dendroam-up-hierarchy (org-roam-node-dendroam-hierarchy node)))
         (parent-file (file-name-concat dir (concat parent-hierarchy "." ext))))
    ;; Do nothing if we're at a file at the top of a hierarchy
    (if (length= parent-hierarchy 0)
        (error "Top-level nodes have no parent.")
      (if (file-exists-p parent-file)
          (find-file parent-file)
        (org-roam-capture-
         :node (org-roam-node-create :title parent-hierarchy)
         ;; NOTE Same as :jump-to-captured t within a template. See
         ;; `org-roam-capture--finalize-find-file'
         :props '(:finalize find-file))))))

(defun dendroam-find-siblings ()
  "Find sibling nodes at the same hierarchical level as input, excluding the current node."
  (interactive)
  (if (eq major-mode 'org-mode)
      (dendroam--find-siblings (org-roam-node-at-point))
    (message "%s only works within org-mode." this-command)))

(cl-defmethod dendroam--find-siblings ((node org-roam-node))
  (let* ((hierarchy (org-roam-node-dendroam-hierarchy node))
         (parent-hierarchy (dendroam-up-hierarchy hierarchy)))
    (if (length= parent-hierarchy 0)
        (error "Top-level nodes have no siblings.")
      (let* ((prefix (string-replace dendroam-separator "\\." (concat "^" parent-hierarchy dendroam-separator)))
             (ncomponents (length (split-string hierarchy (regexp-quote dendroam-separator))))
             (new-node
              (org-roam-node-read
               nil
               (lambda (other-node)
                 (let* ((other-hierarchy (org-roam-node-dendroam-hierarchy other-node))
                        (other-ncomponents (length (split-string other-hierarchy (regexp-quote dendroam-separator)))))
                   (and (string-match-p prefix other-hierarchy)
                        (= ncomponents other-ncomponents)
                        (not (string= (org-roam-node-file node)
                                      (org-roam-node-file other-node))))))
               #'dendroam-sort-by-display-hierarchy
               t)))
        (org-roam-node-visit new-node)))))

(defun dendroam-find-children ()
  "Find children nodes one hierarchical level beneath input."
  (interactive)
  (if (eq major-mode 'org-mode)
      (dendroam--find-children (org-roam-node-at-point))
    (message "%s only works within org-mode." this-command)))

(cl-defmethod dendroam--find-children ((node org-roam-node))
  (let* ((hierarchy (org-roam-node-dendroam-hierarchy node))
         (prefix (string-replace dendroam-separator "\\." (concat "^" hierarchy dendroam-separator)))
         (new-node
          (org-roam-node-read
           nil
           (lambda (other-node)
             (let* ((other-hierarchy (org-roam-node-dendroam-hierarchy other-node)))
               (and (string-match-p prefix other-hierarchy)
                    ;; Exclude the current node
                    (not (string= (org-roam-node-file node)
                                  (org-roam-node-file other-node))))))
           #'dendroam-sort-by-display-hierarchy
           t)))
    (org-roam-node-visit new-node)))

;;; Helpers -------------------------------------------------------------------

(defun dendroam-split (str)
  (when (stringp str)
    (split-string str (regexp-quote dendroam-separator))))

(defun dendroam-display-split (str)
  (when (stringp str)
    (split-string str (regexp-quote dendroam-display-separator))))

(defun dendroam-join (strings)
  (setq strings (remove nil strings))
  (setq strings (remove "" strings))
  (string-join strings dendroam-separator))

(defun dendroam-join-strings (&rest strings)
  (setq strings (remove nil strings))
  (setq strings (remove "" strings))
  (string-join strings dendroam-separator))

(defun dendroam-display-join (strings)
  (setq strings (remove nil strings))
  (setq strings (remove "" strings))
  (string-join strings dendroam-display-separator))

(defun dendroam-up-hierarchy (hierarchy)
  (dendroam-join (butlast (dendroam-split hierarchy))))

(defun dendroam-display-up-hierarchy (hierarchy)
  (dendroam-display-join (butlast (dendroam-display-split hierarchy))))

(defun dendroam--exclude-nodes (node)
  "Scrath and meeting nodes should only be made on normal dendroam
notes, not other meeting, scratch, or reference notes."
  (not (or (dendroam--meeting-note-p node)
           (dendroam--scratch-note-p node)
           (dendroam--citar-note-p node))))

(cl-defmethod dendroam--meeting-note-p ((node org-roam-node))
  "Return t if org-roam note is a dendroam meeting note"
  (require 'dash)
  (let* ((title (car (last (dendroam-split (org-roam-node-dendroam-hierarchy node)))))
         (dmy (butlast (nthcdr 3 (parse-time-string title)) 3)))
    (not (-any 'null dmy))))

(cl-defmethod dendroam--scratch-note-p ((node org-roam-node))
  "Return t if org-roam note is a dendroam datetime note"
  (string= "scratch" (org-roam-node-title node)))

(cl-defmethod dendroam--citar-note-p ((node org-roam-node))
  (string=
   (directory-file-name (file-name-directory (org-roam-node-file node)))
   (file-name-concat org-roam-directory citar-org-roam-subdir)))

(defun dendroam-sort-by-display-hierarchy (completion-a completion-b)
  (let ((node-a (cdr completion-a))
        (node-b (cdr completion-b)))
    (string> (org-roam-node-dendroam-display-hierarchy node-b)
             (org-roam-node-dendroam-display-hierarchy node-a))))

(provide 'dendroam)

;;; NODE REFACTOR --------------------------------------------------------------------

;; ;; TODO Also refactor citar notes' aliases that are part of the hierarchy
;; (defun dendroam-refactor-hierarchy ()
;;   "Rename current note and all of its children"
;;   (interactive)
;;   (dendroam--refactor-hierarchy (org-roam-node-at-point)))

;; (cl-defmethod dendroam--refactor-hierarchy ((node org-roam-node))
;;   (let* ((hierarchy (org-roam-node-dendroam-hierarchy node))
;;          (new-hierarchy (read-string "Refactor: " hierarchy))
;;          ;; regexp functions ignore case by default (see case-fold-search)
;;          (case-fold-search)
;;          (files (directory-files-recursively org-roam-directory (concat "^" hierarchy)))
;;          (new-title (car (last (split-string new-hierarchy (regexp-quote dendroam-separator))))))
;;     (dolist (file files)
;;       (let ((new-file (replace-regexp-in-string hierarchy new-hierarchy file))
;;             (buf (get-file-buffer file)))
;;         (save-some-buffers)
;;         (rename-file file new-file)
;;         (if (equal buffer-file-name file)
;;             (progn
;;               (kill-current-buffer)
;;               (find-file new-file)
;;               ;; Update the title of the current node
;;               (org-roam-set-keyword "title" new-title)
;;               (save-buffer))
;;           ;; TODO This should update open buffers for all modified files, not
;;           ;; just the current buffer. See azr/org-roam-modify-title. In the
;;           ;; meantime, here is my hack.
;;           (when buf (kill-buffer buf)))))))

;; ;; TODO Add a warning when the node to rename is a parent node, in case we want
;; ;; to use refactor instead.
;; (defun dendroam-rename-note ()
;;   "Rename current note only (i.e., preserve hierarchy) and change title to match."
;;   (interactive)
;;   (dendroam--rename-note (org-roam-node-at-point)))

;; (cl-defmethod dendroam--rename-note ((node org-roam-node))
;;   ;; These types of notes' titles and filenames are not linked, so you can
;;   ;; simply rename those files using built-in commands.
;;   (unless (or (dendroam--meeting-note-p node)
;;               (dendroam--scratch-note-p node)
;;               (dendroam--citar-note-p node))
;;     (let* ((hierarchy (org-roam-node-dendroam-hierarchy node))
;;            (new-hierarchy (read-string "Rename: " hierarchy))
;;            (new-title (car (last (split-string new-hierarchy (regexp-quote dendroam-separator)))))
;;            (file (buffer-file-name))
;;            (new-file (replace-regexp-in-string hierarchy new-hierarchy file)))
;;       (save-buffer)
;;       (rename-file file new-file)
;;       (kill-current-buffer)
;;       (find-file new-file)
;;       (org-roam-set-keyword "title" new-title)
;;       (save-buffer))))
