;; Commands

(defun mode-specific-C-h ()
  "Programming language-specific help for company-active-map"
  (interactive)
  (pcase major-mode
    ('ess-r-mode (show-company-doc-as-ess-help))
    (_ (company-show-doc-buffer))))

(defun show-company-doc-as-ess-help ()
  "Show ess help if available, else show company help"
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
         (obj-help (ess-display-help-on-object selected)))
    (unless obj-help
      (company-show-doc-buffer))))
