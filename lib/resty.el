(require 'request)
(require 's)

(defgroup resty nil
  "An interactive HTTP client for Emacs."
  :group 'tools)

(defcustom resty-log-request t
  "Log resty requests to *Messages*."
  :group 'resty
  :type 'boolean)

(defcustom resty-buffer-response-name "*HTTP Response*"
  "Name for response buffer."
  :group 'resty
  :type 'string)

(defcustom resty-default-headers '(("Content-Type" . "application/json")
                                   ("Accept" . "application/json"))
  "Default headers"
  :group 'resty
  :type 'string)

(define-minor-mode resty-response-mode
  "Minor mode to allow additional keybindings in resty response buffer."
  :init-value nil
  :lighter nil
  :keymap '(("q" . (lambda ()
                     (interactive)
                     (quit-window (get-buffer-window (current-buffer))))))
  :group 'resty)

(defun resty--make-request--new (method path body &rest rest)
  (setq user-headers (or (plist-get rest :headers) '())
        base-url (or (plist-get rest :base-url) (resty--base-url))
        params (or (plist-get rest :params) '()))
  (let ((headers (append user-headers resty-default-headers))
        (url (concat base-url path)))
    (resty--http-do method url params headers (json-encode-plist body))))

(cl-defun resty--response-handler (&key data response &allow-other-keys)
  (with-current-buffer
      (get-buffer-create resty-buffer-response-name)
    (erase-buffer)
    (insert data)
    (js-mode)
    (json-pretty-print-buffer)
    (goto-char (point-max))
    (resty--print-headers response)
    (goto-char (point-min))
    (buffer-enable-undo)
    (resty-response-mode)
    (message "")
    (switch-to-buffer-other-window (current-buffer))))

(defun resty--http-do (method url params headers entity)
  (if resty-log-request
      (message "HTTP %s %s Headers:[%s] Body:[%s]" method url headers entity))
  (request url
           :params params
           :headers headers
           :type method
           :data (json-encode-plist body)
           :parser 'buffer-string
           :success #'resty--response-handler))

(defun resty--print (response key)
  (let ((value (plist-get (request-response-settings response) key)))
    (insert (format "%s\n" (or value "")))))

(defun resty--print-headers (response)
  (let ((hstart (point)))
    (insert "\n\n")
    (resty--print response :url)
    (resty--print response :headers)
    (insert "\n")
    (insert (request-response--raw-header response))
    (comment-region hstart (point))))

(defun POST (path body &rest rest)
  (apply #'resty--make-request--new (append (list "POST" path body) rest)))

(defun GET (path &rest rest)
  (apply #'resty--make-request--new (append (list "GET" path nil) rest)))

(defun PATCH (path body &rest rest)
  (apply #'resty--make-request--new (append (list "PATCH" path body) rest)))

(defun PUT (path body &rest rest)
  (apply #'resty--make-request--new (append (list "PUT" path body) rest)))

(defun DELETE (path body &rest rest)
  (apply #'resty--make-request--new (append (list "DELETE" path body) rest)))

;; config

(defvar resty--env-config '())

(defvar resty--current-env 'dev)

(defun resty--add-env (env-pair)
  (unless (consp env-pair) (setq env-pair `((global . ,env-pair))))
  (setq resty--env-config
        (cons (cons (buffer-name) env-pair)
              resty--env-config)))

(defun resty--get-env (name env)
  (alist-get env (alist-get name resty--env-config)))

;; (defun resty--use-env (env) (setq-local use-env env))

(defun resty--base-url ()
  (or (resty--get-env (buffer-name) resty--current-env)
      (resty--get-env (buffer-name) 'global)))

;; Enable hideshow minor mode
(hs-minor-mode)
