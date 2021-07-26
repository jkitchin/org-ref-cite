;;; org-ref-cite-utils.el --- Utilities for org-ref-cite

;;; Commentary:
;;


(defun org-ref-cite-get-unique-keys ()
  "Get the unique keys in the buffer."
  (delete-dups
   (org-element-map (org-element-parse-buffer) 'citation-reference
     (lambda (cr)
       (org-element-property :key cr)))))


(defun org-ref-cite-extract-bibliography (format output)
  "Extract the bibliography from the current org-file.
FORMAT: formatted bibtex
OUTPUT:  buffer copy file"
  (interactive (list (completing-read "Format: " '(formatted bibtex) nil t)
		     (completing-read "Output: " '(buffer copy file) nil t)))
  (let* ((keys (org-ref-cite-get-unique-keys))
	 (strings (cl-loop for key in keys collect
			   (pcase format
			     ("formatted"
			      (bibtex-completion-apa-format-reference key))

			     ("bibtex"
			      (save-window-excursion
				(bibtex-completion-show-entry (list key))
				(bibtex-beginning-of-entry)
				(bibtex-copy-entry-as-kill)
				(pop bibtex-entry-kill-ring)))))))
    (pcase output

      ("buffer"
       (let ((buf (get-buffer-create "*org-ref-cite-export*")))
	 (with-current-buffer buf
	   (erase-buffer)
	   (insert (string-join strings "\n\n"))
	   (when (string= format "bibtex") (bibtex-mode))
	   (pop-to-buffer buf))))

      ("copy"
       (kill-new (string-join strings "\n\n")))

      ("file"
       (let ((fname (read-file-name "File: " nil nil nil (concat (file-name-base (buffer-file-name))
								 (pcase format
								   ("bibtex" ".bib")
								   ("formatted" ".txt"))))))
	 (with-temp-file fname
	   (insert (string-join strings "\n\n"))))))))


(defun org-ref-cite-s-insert (s &rest args)
  "Insert S formatted with ARGS."
  (insert (apply #'format s args)))

(defun org-ref-cite ()
  "Generate a summary buffer of the current buffer.
This buffer shows the current setup, shows bad citations, etc.

TODO: LaTeX setup
TODO: check mulitple citation references for prefix/suffix, which may be undefined."
  (interactive)

  (let* ((buf (get-buffer-create "*org-ref-cite*"))
	 (fname (buffer-file-name))
	 (data (org-element-parse-buffer))
	 (citations (org-element-map data 'citation 'identity))
	 (citation-references (org-element-map data 'citation-reference 'identity))
	 (bibtex-completion-bibliography (org-cite-list-bibliography-files))
	 (valid-keys (cl-loop for candidate in (bibtex-completion-candidates) collect
			      (cdr (assoc "=key=" (cdr candidate)))))
	 (bibliographystyle (cadr (assoc "BIBLIOGRAPHYSTYLE"
					 (org-collect-keywords '("BIBLIOGRAPHYSTYLE")))))
	 (bibliography (mapcar (lambda (f)
				 (format "[[%s]]" f))
			       (org-cite-list-bibliography-files)))
	 (natbib-options (or
			  ;; keyword settings
			  (cadr (assoc
				 "NATBIB_OPTIONS"
				 (org-collect-keywords
				  '("NATBIB_OPTIONS"))))

			  ;; settings in the org-file
			  (save-excursion
			    (goto-char (point-min))
			    (when (re-search-forward "\\\\usepackage\\[?\\([^]]*\\)\\]?{natbib}" nil t)
			      (match-string-no-properties 1)))

			  ;; default global settings
			  (cl-loop for (option package preview) in
				   (append org-latex-default-packages-alist org-latex-packages-alist)
				   when (string= package "natbib")
				   return option)))
	 (unique-keys (org-ref-cite-get-unique-keys))
	 (bad-references (cl-loop for ref in citation-references
				  if (not (member (org-element-property :key ref) valid-keys))
				  collect ref)))

    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (org-ref-cite-s-insert "#+title: org-ref-cite report for %s\n\n" fname)
      (org-ref-cite-s-insert "- bibliographystyle :: %s\n" bibliographystyle)
      (org-ref-cite-s-insert "- bibliography :: %s\n" bibliography)
      (org-ref-cite-s-insert "- natbib options :: %s\n" natbib-options)

      ;; natbib options

      (org-ref-cite-s-insert "- # unique references :: %s\n" (length unique-keys))

      (when bad-references
	(insert "\n* Bad references\n\n")
	(cl-loop for ref in bad-references do
		 (insert "- ")
		 (insert-button (org-element-property :key ref)
				'face '(:foreground "red")
				'keymap (let ((map (make-sparse-keymap)))
					  (define-key map (kbd "<mouse-1>")
					    `(lambda ()
					       (interactive)
					       (save-window-excursion
						 (find-file ,fname)
						 (goto-char ,(org-element-property :begin ref))
						 (org-ref-cite-replace-key-with-suggestions))))
					  map)
				'mouse-face 'highlight
				'help-echo "Bad key, click to replace.")
		 (org-ref-cite-s-insert " (Possible keys: %s)\n" (org-cite-basic--close-keys (org-element-property :key ref) valid-keys))))

      (read-only-mode +1))

    (display-buffer-in-side-window buf '((side . right)))))

(provide 'org-ref-cite-utils)

;;; org-ref-cite-utils.el ends here
