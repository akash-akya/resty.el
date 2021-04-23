;;; resty.el --- Emacs REST Client  -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'request)
(require 'json)
(require 'pp)
(require 'curl-to-elisp)

(defvar resty-mode-hook nil)

(defvar resty-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in `elixir-mode'.")

;; helper utils

(defun resty-json-to-alist (begin end)
  (interactive "r")
  (resty-encode-json begin end 'alist))

(defun resty-json-to-plist (begin end)
  (interactive "r")
  (resty-encode-json begin end 'plist))

(defun resty-encode-json (begin end object-type)
  (atomic-change-group
    (let ((json-object-type object-type)
          (txt (delete-and-extract-region begin end)))
      (insert (prin1-to-string (json-read-from-string txt))))))

(defun resty-pget (plist keys &optional default-value)
  (unless default-value (setq default-value nil))
  (let ((key (pop keys)))
    (cond
     ((null key)
      plist)
     ((listp plist)
      (if (plist-member plist key)
          (resty-pget (plist-get plist key) keys default-value)
        default-value))
     (t
      (error "Invalid args" plist keys)))))

(defun resty-jget (json keys)
  (let* ((json-object-type 'plist)
         (body (json-read-from-string json)))
    (resty-pget body keys)))

;; request handler

(defvar resty--request-state-hash (make-hash-table :test 'equal))

(defun resty--request-lock (id)
  (unless (gethash id resty--request-state-hash)
    (puthash id 'lock resty--request-state-hash)
    ;; if buffer is killed for some reason before we get chance to cleanup
    (add-hook 'kill-buffer-hook (lambda () (resty--request-unlock id)) 0 :local)
    t))

(defun resty--request-unlock (id)
  (remhash id resty--request-state-hash))

;; ***DEBUG***
;; (defun resty-unlock-request ()
;;   (interactive)
;;   (resty--request-unlock "Response"))

(defun resty--log-request (request)
  (message "=============== HTTP-REQUEST-START ID:%s =============" (plist-get request :id))
  (message "%s %s" (plist-get request :method) (plist-get request :url))
  (message "HEADERS:")
  (let ((body (plist-get request :entity))
        (headers (plist-get request :headers)))
    (mapcar (lambda (header)
              (message "  %s: %s" (car header) (cdr header)))
            (plist-get request :headers))

    (when (> (length body) 0)
      (message "BODY:")
      (if (equal (assoc-string "Content-Type" headers)
                 '("Content-Type" . "application/json"))
          (message (resty--format-json body))
        (message body))))
  (message "=============== HTTP-REQUEST-END ID:%s ==============" (plist-get request :id)))

(defun resty--http-do (request timeout success-callbacks failure-callbacks context)
  "`Context' is need for supporting request chaining. since the request.el makes asynchronous call, the lexical bindings will be lost"
  (resty--log-request request)
  (if (null (resty--request-lock (plist-get request :id)))
      (message "[resty.el] A request is already in process... skipping")
    (let (status)
      (unwind-protect
          (progn
            (request (plist-get request :url)
                     :headers (plist-get request :headers)
                     :type (plist-get request :method)
                     :data (plist-get request :entity)
                     :parser 'buffer-string
                     ;; :sync t
                     :timeout timeout
                     :success (resty--create-response-handler
                               request
                               (append (list #'resty--unlock-request) success-callbacks)
                               context)
                     :error (resty--create-response-handler
                             request
                             (append (list #'resty--unlock-request) failure-callbacks)
                             context))
            (setq status 'ok)
            (message "[resty.el] Request started"))
        (unless status
          (resty--request-unlock (plist-get request :id))
          (message "[resty.el] Request failed due to an error"))))))

(defun resty--create-response-handler (request callbacks context)
  (let ((start-time (current-time)))
    (lambda (&rest rest)
      (let* ((duration (time-subtract (current-time) start-time))
             (response (plist-get rest :response))
             (data (resty--try-parsing-response response))
             (args (list data response request duration)))
        ;; Setting global dynamic variable temporarily
        ;; This works since Emacs is single threaded
        (resty--call-with-dynamic-binding callbacks args context)))))

(defun resty--try-parsing-response (response)
  (let ((data (request-response-data response))
        (content-type (request-response-header response "Content-Type")))
    (if (string-prefix-p "application/json" content-type)
        (json-read-from-string data)
      data)))

(defun resty--unlock-request (_data _response request _duration)
  (resty--request-unlock (plist-get request :id)))

(defun resty--call-with-dynamic-binding (callbacks args bindings)
  (let ((binding (pop bindings)))
    (if (null binding)
        (dolist (func callbacks)
          (apply func args))
      (let ((var-symbol (car binding))
            (var-value (cdr binding)))
        ;; make temp variable definition so `cl-letf' does not complain.
        ;; we define new global-variable if there isn't one
        (unless (boundp var-symbol)
          (set var-symbol nil))
        (cl-letf (((symbol-value var-symbol) var-value))
          (resty--call-with-dynamic-binding callbacks args bindings))))))

;; configuration

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
                     (resty--show-headers-window)))
            ("p" . (lambda ()
                     (interactive)
                     (resty--goto-request-pos)))
            ("g" . (lambda ()
                     (interactive)
                     (resty--re-run)))
            ("$" . (lambda ()
                     (interactive)
                     (resty--show-headers-window))))
  :group 'resty)

(defun resty--stringify (obj)
  (prin1-to-string obj t))

(defun resty--url-params (alist)
  (if (consp alist)
      (concat "?"
              (string-join
               (mapcar (lambda (pair)
                         (concat (url-hexify-string (resty--stringify (car pair)))
                                 "="
                                 (url-hexify-string (resty--stringify (cdr pair))))) alist)
               "&"))
    ""))

(defun resty--make-request (request pos)
  (let* ((success-callbacks (or (plist-get request :callbacks) '()))
         (timeout (or (plist-get request :timeout) resty-default-timeout))
         (id (plist-get request :id))
         (method (plist-get request :method))
         (url (plist-get request :url))
         (body (plist-get request :body))
         (display (resty-pget request '(:display) t))
         (headers (plist-get request :headers))
         (extra-context (resty-pget request '(:context) '()))
         (context `((resty--environments        .  ,resty--environments)
                    (resty--current-environment .  ,resty--current-environment)
                    (resty--buffer-name         .  ,resty--buffer-name))))

    (when display
      (resty--reset-response-buffer id method url)
      (push #'resty--response-handler success-callbacks))

    ;; log response before anything else
    (push #'resty--response-logger success-callbacks)

    (setq context (append context extra-context))

    (resty--http-do
     (list :id id :method method :url url :headers headers :entity body :pos pos)
     timeout
     success-callbacks
     (list #'resty--response-handler)
     context)))

(defun resty--run-request ()
  (interactive)
  (let ((sexp (read (resty--top-level-sexp))))
    (resty--make-request (eval sexp) (point-marker))))

(defun resty--build-request (method path body &rest rest)
  (let* ((env (or (plist-get rest :env) resty--current-environment))
         (resty--buffer-name (or resty--buffer-name (buffer-name)))
         (base-url (or (plist-get rest :base-url)
                       (resty--base-url resty--environments resty--buffer-name env)
                       (error "No base URL" env resty--buffer-name)))
         (params (resty--url-params (or (plist-get rest :params) '())))
         (url (concat base-url path params))
         (user-headers (or (plist-get rest :headers) '()))
         (headers (append user-headers resty-default-headers))
         (id (or (plist-get rest :id) resty-buffer-response-name)))
    (append rest (list :id id :method method :url url :headers headers :body (json-encode-plist body)))))

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
      (message "[resty.el] took %fs" (float-time duration))
      (setq-local resty-formated-headers (resty--formatted-headers response duration))
      (setq-local resty-request request)
      (resty--set-header-line status-code method url)
      (resty--display-buffer (current-buffer)))))

(defun resty--display-buffer (buffer)
  (let ((window (display-buffer buffer nil)))
    (let* ((old-frame (selected-frame))
           (new-frame (window-frame window)))
      (select-window window)
      (unless (eq old-frame new-frame)
        (select-frame-set-input-focus new-frame)))))

(defun resty--response-logger (data response request duration)
  (resty--log-response request response))

(defun resty--response-buffer-name (name)
  (format "*RESTY-%s*" name))

(defun resty--log-response (request response)
  (message "=============== HTTP-RESPONSE-START ID:%s =============" (plist-get request :id))
  (pcase (resty--response-content-type response)
    ("application/json" (message "%s" (resty--format-json (request-response-data response))))
    ("application/xml" (message "%s" (request-response-data response)))
    ("text/xml" (message "%s" (request-response-data response)))
    ((pred (string-prefix-p "text/")) (message "%s" (request-response-data response)))
    (_ (message "None Text Response")))
  (message "=============== HTTP-RESPONSE-END ID:%s ==============" (plist-get request :id)))

(defun resty--reset-response-buffer (id method url)
  (let ((request-buffer (current-buffer)))
    (with-current-buffer (get-buffer-create (resty--response-buffer-name id))
      (when buffer-read-only
        (setq-local buffer-read-only nil))
      (setq-local header-line-format
                  (concat
                   (propertize (format " START %s " method) 'face 'bold)
                   (propertize url)))
      (erase-buffer)
      (unless (eq request-buffer (current-buffer))
        (switch-to-buffer-other-window (current-buffer))
        (other-window -1)))))

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
  (pcase content-type
    ("application/json" #'resty--json-response-handler)
    ("application/xml" #'resty--xml-response-handler)
    ("text/xml" #'resty--xml-response-handler)
    ((pred (string-prefix-p "text/")) #'resty--text-response-handler)
    (_ #'resty--skip-response-handler)))

(defun resty--json-response-handler (_data response request duration)
  (js-mode)
  (hs-minor-mode)
  (insert (request-response-data response))
  (json-pretty-print-buffer)
  (setq-local buffer-read-only t))

(defun resty--xml-response-handler (_data response request duration)
  (nxml-mode)
  (hs-minor-mode)
  (insert (request-response-data response)))

(defun resty--text-response-handler (_data response request duration)
  (text-mode)
  (insert (request-response-data response)))

(defun resty--skip-response-handler (_data response request duration)
  (text-mode)
  (insert "Not showing response for the request"))


(defun resty--show-headers-window ()
  (let ((help-window-select t))
    (with-help-window (concat (buffer-name) " HEADERS")
      (princ resty-formated-headers))))

(defun resty--goto-request-pos ()
  (let ((pos (plist-get resty-request :pos)))
    (when (markerp pos)
      (with-current-buffer (marker-buffer pos)
        (goto-char pos)
        (switch-to-buffer-other-window (marker-buffer pos))))))

(defun resty--re-run ()
  (let ((pos (plist-get resty-request :pos)))
    (resty--make-request resty-request pos)))

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

(defun cond-eval (method path body rest)
  (apply #'resty--build-request method path body rest))

(defun POST (path body &rest rest)
  (cond-eval "POST" path body rest))

(defun GET (path &rest rest)
  (cond-eval "GET" path nil rest))

(defun PATCH (path body &rest rest)
  (cond-eval "PATCH" path body rest))

(defun PUT (path body &rest rest)
  (cond-eval "PUT" path body rest))

(defun DELETE (path body &rest rest)
  (cond-eval "DELETE" path body rest))

;; cURL utils

(defun resty--extract-curl (cmd)
  "Returns '(URL METHOD HEADERS DATA SILENT)"
  (curl-to-elisp--extract
   (curl-to-elisp--parse
    (curl-to-elisp--tokenize
     (curl-to-elisp--trim cmd)))))

(defun resty--get-header (key headers)
  (alist-get "content-type" headers nil nil #'string-equal))

(defun resty-import-request-from-curl (curl-string &optional insert)
  (interactive (list (read-string "cURL command: ") t))
  (cl-multiple-value-bind (url method headers data silent) (resty--extract-curl curl-string)
    (seq-do (lambda (el) (setf (car el) (downcase (car el)))) headers)
    (setq method (or method "GET"))
    (setq content-type (resty--get-header "content-type" headers))
    (if (and content-type (not (string= content-type "application/json")))
        (error "Only support application/json")
      (let ((json-object-type 'plist))
        (setq data (json-read-from-string (or data "{}")))))
    (setq exp-str (list (intern (upcase method)) url
                        (and data (list 'quote data))
                        :headers (and headers (list 'quote headers))))
    (when insert
      (save-excursion
        (insert (pp-to-string exp-str))))
    (pp-to-string exp-str)))

(defun resty--top-level-sexp ()
  (interactive)
  (save-excursion
    (let ((beg (progn (beginning-of-defun) (point)))
          (end (progn (end-of-defun) (point))))
      (buffer-substring-no-properties beg end))))

(defun resty-copy-current-request-as-curl ()
  (interactive)
  (let ((sexp (read (resty--top-level-sexp))))
    (resty--copy-request-as-curl (eval sexp))))

(defun resty--copy-request-as-curl (request)
  (let ((url (plist-get request :url))
        (body (plist-get request :body))
        (method (plist-get request :method))
        (headers (plist-get request :headers)))
    (let ((header-args
           (mapcar (lambda (header) (format "-H '%s: %s'" (car header) (cdr header)))
                   headers))
          (joiner " \\\n  "))
      (kill-new (string-join
                 (list (format "curl -X %s" method)
                       (format "'%s'" url)
                       (string-join header-args joiner)
                       (when (> (length body) 0)
                         (if (equal (assoc-string "Content-Type" headers)
                                    '("Content-Type" . "application/json"))
                             (format "-d '%s'" (resty--indent (resty--format-json body) "  "))
                           (format "-d '%s'" body))))
                 joiner)))
    (message "curl command copied to clipboard.")))

(defun resty--indent (json indent)
  (replace-regexp-in-string "\n" (concat "\n" indent) json))

(defun resty--run-command (text out-buffer)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "jq" nil out-buffer t "." "--raw-output")))

(defun resty--indent-json (text)
  (let (return-value)
    (with-temp-buffer
      (setq return-value (resty--run-command text (current-buffer)))
      (list return-value (buffer-string)))))

(defun resty--format-json (body)
  (if (executable-find "jq")
      (let* ((result (resty--indent-json body))
             (status (car result))
             (text (cadr result)))
        (if (eq status 0) (string-trim text) body))
    body))

;; config

;; buffer name captured when request starts, needed for request
;; chaining to preserve the context during consecutive requests.
;; *should* not be set manually, it will be set and used internally
(make-variable-buffer-local
 (defvar resty--buffer-name nil))

(defvar resty--environments (make-hash-table))
(defvar resty--current-environment :dev)

(defun resty-set-environments (plist)
  (map-put! resty--environments (buffer-name)
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


;; (global-set-key (kbd "C-c c") #'resty--run-request)

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
(add-hook 'resty-mode-hook #'resty--eval-buffer-init)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.resty\\'" . resty-mode))

(provide 'resty)

;;; resty.el ends here
