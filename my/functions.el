(defun show-trailing-whitespace()
  (interactive)
  (setq show-trailing-whitespace t)
  )

;;; It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line
of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "Takes a region and makes its paragraphs into a single line of
text."
  (interactive "r")
  (let ((fill-column (point-max)))
    (fill-region start end nil)))
