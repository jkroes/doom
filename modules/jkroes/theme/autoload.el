;;; -*- lexical-binding: t; -*-

(defconst embark--verbose-indicator-buffer " *Embark Actions*")

;;;###autoload
(defun jkroes/instead-force-only-if-predicates-falsy ()
  "Advise to only force process if no predicate is truthy."
  (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                         dimmer-prevent-dimming-predicates)))
    (unless ignore
      (when (fboundp 'dimmer-process-all)
        (dimmer-process-all t)))))

;;;###autoload
(defun jkroes/dimmer-configure-embark ()
  (add-to-list
   'dimmer-buffer-exclusion-regexps
   (regexp-quote embark--verbose-indicator-buffer)))

;;;###autoload
(defun jkroes/dimmer-configure-corfu ()
  (add-to-list
   'dimmer-prevent-dimming-predicates
   #'jkroes/corfu-frame-p))

(defun jkroes/corfu-frame-p ()
  "Check if the buffer is a corfu frame buffer."
  (string-match-p "\\` \\*corfu" (buffer-name)))
