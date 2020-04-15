;; -*- lexical-binding: t -*-
(require 'json)

(defun resty--json-to-alist (begin end)
  (interactive "r")
  (resty--encode-json begin end 'alist))

(defun resty--json-to-plist (begin end)
  (interactive "r")
  (resty--encode-json begin end 'plist))

(defun resty--encode-json (begin end object-type)
  (atomic-change-group
    (let ((json-object-type object-type)
          (txt (delete-and-extract-region begin end)))
      (insert (prin1-to-string (json-read-from-string txt))))))

(provide 'resty-utils)
