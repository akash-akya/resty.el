;; -*- lexical-binding: t -*-
(require 'request)
(require 'cl-lib)

(defvar resty--request-state-hash (make-hash-table :test 'equal))
(defun resty--request-lock (id)
  (unless (gethash id resty--request-state-hash)
    (puthash id 'lock resty--request-state-hash)
    t))

(defun resty--request-unlock (id)
  (remhash id resty--request-state-hash))

(defun resty--http-do (request timeout success-callbacks failure-callbacks context)
  "`Context' is need for supporting request chaining. since the request.el makes asynchronous call, the lexical bindings will be lost"
  (message "HTTP %s %s ID: %s Headers: %s Body: %s"
           (plist-get request :method)
           (plist-get request :url)
           (plist-get request :id)
           (plist-get request :headers)
           (plist-get request :entity))
  (if (null (resty--request-lock (plist-get request :id)))
      (message "[resty] A request is already in process... skipping")
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
            (message "[resty] Request started"))
        (unless status
          (resty--request-unlock (plist-get request :id))
          (message "[resty] Request failed due to an error"))))))

(defun resty--create-response-handler (request callbacks context)
  (let ((start-time (current-time)))
    (lambda (&rest rest)
      (let ((duration (time-subtract (current-time) start-time))
            (data (resty--try-parsing-response (plist-get rest :response))))
        ;; Setting global dynamic variable temporarily
        ;; This works since Emacs is single threaded
        (cl-letf (((symbol-value 'resty--environments) (seq-elt context 0))
                  ((symbol-value 'resty--current-environment) (seq-elt context 1))
                  ((symbol-value 'resty--buffer-name) (seq-elt context 2)))
          (dolist (c callbacks)
            (funcall c data (plist-get rest :response) request duration)))))))

(defun resty--try-parsing-response (response)
  (let ((data (request-response-data response))
        (content-type (request-response-header response "Content-Type")))
    (if (string-prefix-p "application/json" content-type)
        (json-read-from-string data)
      data)))

(defun resty--unlock-request (_data _response request _duration)
  (resty--request-unlock (plist-get request :id)))

(provide 'resty-request-helper)
