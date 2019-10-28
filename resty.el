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
                     (quit-window (get-buffer-window (current-buffer)))))
            ("h" . (lambda ()
                     (interactive)
                     (resty--show-headers-window))))
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
  (let* ((env (or (plist-get rest :env) resty--current-environment))
         (resty--buffer-name (or resty--buffer-name (buffer-name)))
         (base-url (or (plist-get rest :base-url)
                       (resty--base-url resty--environments resty--buffer-name env)
                       (error "No base URL" env resty--buffer-name)))
         (params (resty--url-params (or (plist-get rest :params) '())))
         (url (concat base-url path params))
         (user-headers (or (plist-get rest :headers) '()))
         (headers (append user-headers resty-default-headers))
         (success-callbacks (or (plist-get rest :callbacks) '()))
         (id (or (plist-get rest :id) resty-buffer-response-name))
         (timeout (or (plist-get rest :timeout) resty-default-timeout)))
    (resty--reset-response-buffer id method url)
    (resty--http-do
     (list :id id :method method :url url :headers headers :entity (json-encode-plist body))
     timeout
     (cons #'resty--response-handler success-callbacks)
     (list #'resty--response-handler)
     (list resty--environments resty--current-environment resty--buffer-name))))

(defun resty--response-handler (data response request duration)
  (let ((status-code (request-response-status-code response))
        (url (plist-get (request-response-settings response) :url))
        ;; (time (float-time duration))
        (method (plist-get (request-response-settings response) :type)))
    (resty--safe-buffer (resty--response-buffer-name (plist-get request :id))
      (erase-buffer)
      (funcall (resty--response-buffer-callback response)
               data response request duration)
      (buffer-enable-undo)
      (resty-response-mode)
      (message "[RESTY] DONE: %fs" (float-time duration))
      (setq-local resty-formated-headers (resty--formatted-headers response duration))
      (resty--set-header-line status-code method url)
      (resty--display-buffer (current-buffer)))))

(defun resty--display-buffer (buffer)
  (let ((window (display-buffer buffer nil)))
    (let* ((old-frame (selected-frame))
           (new-frame (window-frame window)))
      (select-window window)
      (unless (eq old-frame new-frame)
        (select-frame-set-input-focus new-frame)))))

(defun resty--response-buffer-name (name)
  (format "*RESTY-%s*" name))

(defun resty--reset-response-buffer (id method url)
  (with-current-buffer (get-buffer-create (resty--response-buffer-name id))
    (setq-local header-line-format
                (concat
                 (propertize (format " START %s " method) 'face 'bold)
                 (propertize url)))
    (erase-buffer)
    (switch-to-buffer-other-window (current-buffer))
    (other-window -1)))

(defmacro resty--safe-buffer (name &rest body)
  (let ((buffer (make-symbol "buffer"))
        (done (make-symbol "done")))
    `(let ((,buffer (get-buffer-create ,name)) ,done)
       (with-current-buffer ,buffer
         (unwind-protect
             (progn
               ,@body
               (setq ,done t))
           (unless ,done (kill-buffer ,buffer)))))))

(defun resty--set-header-line (status-code method url)
  (setq-local header-line-format
              (concat
               (propertize (format " %s " (or status-code "ERROR"))
                           'face (list :inherit (if (or (null status-code)
                                                        (> status-code 299))
                                                    'error
                                                  'success)
                                       :weight 'bold))
               (propertize method 'face 'bold) " "
               (propertize url))))

(defun resty--response-content-type (response)
  (let ((content-type (request-response-header response "Content-Type")))
    (and content-type (car (split-string content-type ";")))))

(defun resty--response-buffer-callback (response)
  (let ((status (request-response-symbol-status response)))
    (if (member status '(success error))
        (resty--response-content-type-dispatcher
         (resty--response-content-type response))
      #'resty--response-display-error)))

(defun resty--response-display-error (_data response _request _duration)
  (text-mode)
  (insert (format "Error: %s" (request-response-symbol-status response))))

(defvar resty--response-handlers (make-hash-table))
(defun resty--add-response-handler (content-type handler)
  (puthash content-type handler resty--response-handler))

(defun resty--response-content-type-dispatcher (content-type)
  (or (gethash content-type resty--response-handlers)
      #'resty--default-response-handler))

(defun resty--default-response-handler (_data response request duration)
  (js-mode)
  (hs-minor-mode)
  (when (json-response? response)
    (insert (request-response-data response))
    (json-pretty-print-buffer))
  (when (csv-response? response)
    (insert (request-response-data response))))

(defun json-response? (response)
  (string-prefix-p "application/json" (request-response-header response "Content-Type")))

(defun csv-response? (response)
  (string-prefix-p "text/csv" (request-response-header response "Content-Type")))

(defun resty--show-headers-window ()
  (let ((help-window-select t))
    (with-help-window (concat (buffer-name) " HEADERS")
      (princ resty-formated-headers))))

(defun resty--formatted-headers (response duration)
  (let ((request-headers (plist-get (request-response-settings response) :headers))
        (time (float-time duration)))
    (string-trim
     (string-join
      (mapcar (lambda (s) (format "%s" s))
              (list (resty--format-request-headers request-headers)
                    (format "\nRequest duration = %fs" time)
                    (or (request-response-error-thrown response) "")
                    (request-response--raw-header response)))
      "\n"))))

(defun resty--format-request-headers (headers)
  (string-join
   (mapcar (lambda (elem) (format "%s: %s" (car elem) (cdr elem))) headers)
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

;; cURL utils

(defun resty--top-level-sexp ()
  (interactive)
  (save-excursion
    (let ((beg (progn (beginning-of-defun) (point)))
          (end (progn (end-of-defun) (point))))
      (buffer-substring-no-properties beg end))))

(defun resty--copy-as-curl-command ()
  (interactive)
  (let ((sexp (read (resty--top-level-sexp))))
    (message "%s" sexp)
    (when (and (consp sexp) (memq (car sexp) '(POST GET DELETE PATCH PUT)))
      (resty--copy-as-curl sexp))))

(defun resty--copy-as-curl (request-sexp)
  ;; FIXME: unify parsing at request handing and convertion to curl
  (let* ((method (format "%s" (car request-sexp)))
         (path (nth 1 request-sexp))
         (body (if (not (string-equal method "GET"))
                   (json-encode-plist (cadr (nth 2 request-sexp)))
                 ""))
         (rest (nth (if (> (length body) 0) 3 2) request-sexp)))
    (let* ((env (or (plist-get rest :env) resty--current-environment))
           (resty--buffer-name (or resty--buffer-name (buffer-name)))
           (base-url (or (plist-get rest :base-url)
                         (resty--base-url resty--environments resty--buffer-name env)
                         (error "No base URL" env resty--buffer-name)))
           (params (resty--url-params (or (plist-get rest :params) '())))
           (url (concat base-url path params))
           (user-headers (or (plist-get rest :headers) '()))
           (headers (append user-headers resty-default-headers)))
      (let ((header-args
             (apply 'append
                    (mapcar (lambda (header)
                              (list "-H" (format "'%s: %s'" (car header) (cdr header))))
                            headers))))
        (kill-new (concat "curl "
                          (string-join
                           (list "-i"
                                 (concat "-X " method)
                                 (format "'%s'" url)
                                 (string-join header-args " ")
                                 (when (> (length body) 0)
                                   (format "-d '%s'" body)))
                           " ")
                          " ")))
      (message "curl command copied to clipboard."))))

;; config

;; buffer name captured when request starts, needed for request
;; chaining to preserve the context during consecutive requests.
;; *should* not be set manually, it will be set and used internally
(make-variable-buffer-local
 (defvar resty--buffer-name nil "Number of foos inserted into the current buffer."))

(defvar resty--environments (make-hash-table))
(defvar resty--current-environment :dev)

(defun resty-set-environments (plist)
  (map-put resty--environments (buffer-name)
           (if (consp plist) plist (list :default plist))))

(defun resty--base-url (environments name env)
  (let ((buffer-env (map-elt environments name)))
    (or (plist-get buffer-env env)
        (plist-get buffer-env :default))))

;; auto buffer initialization
(defun resty--buffer-init-name ()
  (intern (format "resty--init-%s" (buffer-name))))

(defmacro resty--init (&rest body)
  `(setq-local ,(resty--buffer-init-name) (lambda () ,@body)))

(defun resty--buffer-sexps ()
  (save-excursion
    (let (forms)
      (beginning-of-buffer)
      (while (not (eobp))
        (if-let ((str (ignore-errors (read (current-buffer)))))
            (push str forms)))
      forms)))

(defun resty--define-buffer-local-init ()
  (when-let
      ((form (seq-find (lambda (f) (eq (car f) 'resty--init)) (resty--buffer-sexps))))
    (message " From: %s" form)
    (message " Eval From: %s" (eval form))))

(defun resty--eval-buffer-init ()
  (resty--define-buffer-local-init)
  (message "  Eval Result: %s" (funcall (symbol-value (resty--buffer-init-name)))))

(add-hook 'resty-mode-hook #'resty--eval-buffer-init)

;; Environment setting helper
(defun resty--set-global-environment ()
  (interactive)
  (setq resty--current-environment
        (intern (completing-read "RESTY: set environment:" '(:dev :stage)))))

;;; mode definition

;;;###autoload
(define-derived-mode resty-mode
  emacs-lisp-mode "Resty"
  "Major mode for Resty"

  (setq-local mode-name
              '(:eval (propertize (format "Resty[%s]" resty--current-environment) 'face 'bold))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.resty\\'" . resty-mode))

(provide 'resty)
;;; resty.el ends here