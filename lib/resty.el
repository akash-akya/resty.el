;; -*- lexical-binding: t -*-
(require 'request)
(load "./request-helper")

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

(defcustom resty-buffer-response-name "Response"
  "Name for response buffer."
  :group 'resty
  :type 'string)

(defcustom resty-default-headers '(("Content-Type" . "application/json")
                                   ("Accept" . "application/json")
                                   ("cache-control" . "no-cache"))
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
      (concat "?"
              (string-join
               (mapcar (lambda (pair)
                         (concat (format "%s" (car pair))
                                 "="
                                 (format "%s" (cdr pair)))) alist)
               "&"))
    ""))

(defun resty--make-request (method path body &rest rest)
  (setq user-headers (or (plist-get rest :headers) '())
        base-url (or (plist-get rest :base-url)
                     (resty--base-url (plist-get rest :env)))
        success-callbacks (or (plist-get rest :callbacks) '())
        params (resty--url-params (or (plist-get rest :params) '()))
        id (or (plist-get rest :id) resty-buffer-response-name)
        timeout (or (plist-get rest :timeout) resty-default-timeout))
  (let ((headers (append user-headers resty-default-headers))
        (url (concat base-url path params)))
    (resty--http-do
     (list :id id :method method :url url :headers headers :entity (json-encode-plist body))
     timeout
     (cons #'resty--response-handler success-callbacks)
     (list #'resty--response-handler))))

(defun resty--response-handler (data response request duration)
  (let ((status-code (request-response-status-code response))
        (url (plist-get (request-response-settings response) :url))
        ;; (time (float-time duration))
        (method (plist-get (request-response-settings response) :type)))
    (with-current-buffer (resty--create-buffer (plist-get request :id))
      (erase-buffer)
      (funcall (resty--get-response-handler (resty--response-content-type response))
               data response request duration)
      (resty--set-header-line status-code method url)
      (switch-to-buffer-other-window (current-buffer))
      (other-window -1))))

(defun resty--set-header-line (status-code method url)
  (setq-local header-line-format
              (concat
               " "
               (propertize (format "%s" (or status-code "ERROR"))
                           'face (list :inherit (if (or (null status-code)
                                                        (> status-code 299))
                                                    'error
                                                  'success)
                                       :weight 'bold))
               " "
               (propertize method 'face 'bold)
               " "
               (propertize url))))

(defun resty--response-content-type (response)
  (let ((content-type (request-response-header response "Content-Type")))
    (message " --- content-type: %s" content-type)
    (and content-type (car (split-string content-type ";")))))

(defun resty--create-buffer (id)
  (get-buffer-create (format "*RESTY-%s*" id)))

(defvar resty--response-handlers (make-hash-table))
(defun resty--add-response-handler (content-type handler)
  (puthash content-type handler resty--response-handler))

(defun resty--get-response-handler (content-type)
  (or (gethash content-type resty--response-handlers)
      #'resty--default-response-handler))

(defun resty--default-response-handler (_data response request duration)
  (js-mode)
  (hs-minor-mode)
  (when (json-response? response)
    (insert (request-response-data response))
    (json-pretty-print-buffer))
  (when (csv-response? response)
    (insert (request-response-data response)))
  (goto-char (point-max))
  (resty--print-headers response duration)
  (goto-char (point-min))
  (buffer-enable-undo)
  (resty-response-mode)
  (message "[RESTY] DONE: %s" duration))

(defun json-response? (response)
  (let ((content-type (request-response-header response "Content-Type")))
    (string-prefix-p "application/json" content-type)))

(defun csv-response? (response)
  (let ((content-type (request-response-header response "Content-Type")))
    (string-prefix-p "text/csv" content-type)))

(defun resty--print-headers (response duration)
  (let ((hstart (point))
        (url (plist-get (request-response-settings response) :url))
        (method (plist-get (request-response-settings response) :type))
        (request-headers (plist-get (request-response-settings response) :headers))
        (time (float-time duration)))
    (insert
     (string-join
      (mapcar (lambda (s) (format "%s" s))
              (list "\n"
                    (concat method " " url)
                    (resty--format-request-headers request-headers)
                    (format "\nRequest duration = %fs" time)
                    (or (request-response-error-thrown response) "")
                    (request-response--raw-header response)))
      "\n"))
    (comment-region hstart (point))))

(defun resty--format-request-headers (headers)
  (string-join
   (mapcar (lambda (elem) (format "%s: %s" (car elem) (cdr elem)))
           headers)
   "\n"))

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

(defun resty--base-url (env)
  (if env
      (resty--get-env (buffer-name) env)
    (or (resty--get-env (buffer-name) resty--current-env)
        (resty--get-env (buffer-name) 'global))))

;; Enable hideshow minor mode
(hs-minor-mode)

(provide 'resty)
;;; resty.el ends here
