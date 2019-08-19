;; -*- lexical-binding: t -*-
(require 'request)

(defgroup resty nil
  "An interactive HTTP client for Emacs."
  :group 'tools)

(defcustom resty-log-request t
  "Log resty requests to *Messages*."
  :group 'resty
  :type 'boolean)

(defcustom resty-default-timeout 7
  "Default timeout for requests in seconds"
  :group 'resty
  :type 'integer)

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

(defun resty--url-params (alist)
  (if (consp alist)
      (concat "?" (string-join
                   (mapcar (lambda (pair) (concat (car pair) "=" (cdr pair))) alist)
                   "&"))
    ""))

(defun resty--make-request (method path body &rest rest)
  (setq user-headers (or (plist-get rest :headers) '())
        base-url (or (plist-get rest :base-url) (resty--base-url))
        success-handler (resty--create-success-handler (or (plist-get rest :output) t))
        params (resty--url-params (or (plist-get rest :params) '()))
        timeout (or (plist-get rest :timeout) resty-default-timeout))
  (let ((headers (append user-headers resty-default-headers))
        (url (concat base-url path params)))
    (resty--http-do
     method
     url
     headers
     (json-encode-plist body)
     timeout
     success-handler)))

(defun resty--create-success-handler (handler)
  (if (eq handler t)
      #'resty--success-response-handler
    (resty--make-succ-callback-handler handler)))

(defvar resty--request-state nil)
(defvar resty--request-time-start nil)

(defun resty--http-do (method url headers entity timeout success-handler)
  (if resty-log-request
      (message "HTTP %s %s Headers: %s Body: %s" method url headers entity))
  (if resty--request-state
      (message "[resty] A request is already in process... skipping")
    (setq resty--request-state 'attempt)
    (setq resty--request-time-start (current-time))
    (unwind-protect
        (progn
          (request url
                   :headers headers
                   :type method
                   :data entity
                   :parser 'buffer-string
                   ;; :sync t
                   :timeout timeout
                   :success success-handler
                   :error #'resty--error-response-handler)
          (setq resty--request-state 'started)
          (message "[resty] Request started"))
      (when (eq resty--request-state 'attempt) ;; if (request ...) itself fail
        (setq resty--request-state nil)
        (message "[resty] Request failed due to an error")))))

(cl-defun resty--success-response-handler (&key data response &allow-other-keys)
  (resty--cleanup)
  (with-current-buffer (get-buffer-create resty-buffer-response-name)
    (erase-buffer)
    (js-mode)
    (when (json-response? response)
      (insert data)
      (json-pretty-print-buffer))
    (when (csv-response? response)
      (insert data))
    (goto-char (point-max))
    (resty--print-headers response)
    (goto-char (point-min))
    (buffer-enable-undo)
    (resty-response-mode)
    (message "")
    (switch-to-buffer-other-window (current-buffer))))

(cl-defun resty--error-response-handler (&key data response &allow-other-keys)
  (resty--cleanup)
  (with-current-buffer (get-buffer-create resty-buffer-response-name)
    (erase-buffer)
    (js-mode)
    (when (json-response? response)
      (insert data)
      (json-pretty-print-buffer))
    (goto-char (point-max))
    (resty--print-headers response)
    (goto-char (point-min))
    (buffer-enable-undo)
    (resty-response-mode)
    (message "")
    (switch-to-buffer-other-window (current-buffer))))

(defun json-response? (response)
  (let ((content-type (request-response-header response "Content-Type")))
    (string-prefix-p "application/json" content-type)))

(defun csv-response? (response)
  (let ((content-type (request-response-header response "Content-Type")))
    (string-prefix-p "text/csv" content-type)))

(defun resty--cleanup ()
  (setq resty--request-state nil) ;; FIXME: dont use global variable
  (message "[resty] Request Completed"))

(defun resty--make-succ-callback-handler (func)
  (setq-local resty--callback func) ;; FIXME: dirty fix, since closure is not working
  (cl-function
   (lambda (&key data response &allow-other-keys)
     (resty--cleanup)
     (if (json-response? response)
         (funcall resty--callback (json-read-from-string data) response)
       (signal "Not json" response)))))


(defun resty--print-headers (response)
  (let ((hstart (point))
        (url (plist-get (request-response-settings response) :url))
        (request-headers (plist-get (request-response-settings response) :headers))
        (time (float-time (time-subtract (current-time) resty--request-time-start))))
    (insert
     (string-join
      (mapcar (lambda (s) (format "%s" s))
              (list "\n"
                    url
                    request-headers
                    (format "Request duration: %fs" time)
                    (or (request-response-error-thrown response) "")
                    (request-response--raw-header response)
                    ))
      "\n"))
    (comment-region hstart (point))))

(defun POST (path body &rest rest)
  (apply #'resty--make-request (append (list "POST" path body) rest)))

(defun GET (path &rest rest)
  (apply #'resty--make-request (append (list "GET" path nil) rest)))

(defun PATCH (path body &rest rest)
  (apply #'resty--make-request (append (list "PATCH" path body) rest)))

(defun PUT (path body &rest rest)
  (apply #'resty--make-request (append (list "PUT" path body) rest)))

(defun DELETE (path body &rest rest)
  (apply #'resty--make-request (append (list "DELETE" path body) rest)))

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
