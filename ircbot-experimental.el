;; -*- lexical-scoping: t -*-
(setq channel-table (make-hash-table :test 'equal))
(setq async-instance 0)
(defun async-shell-command (callback name &rest args)
  "Runs program NAME with arguments ARGS. When output is received from program, 
CALLBACK will be called. Returns process identifier"
    (let ((proc (apply 'start-process (format "%s+%d" name async-instance) nil name args)))
      (set-process-filter proc callback)
      (setq async-instance (+ async-instance 1))
      proc))

(let ((p (async-shell-command (lambda (p o)
				(message "%s" o))
			      "fortune"
			      "-s")))
  (delete-process p))

(defun get-item (triplet n)
  (let ((list (split-string triplet "\n")))
    (replace-regexp-in-string 
     "\"" 
     ""
     (replace-regexp-in-string 
      (format "^%d) " (+ n 1)) 
      "" 
      (elt list n)))))
(defun get-origin (triplet)
  (get-item triplet 1))
(defun get-message (triplet)
  (get-item triplet 2))

(defun build-reply (irc-proc target process output)
  "async-reply to TARGET in IRC-PROC. OUTPUT
is the last stdout received from PROCESS as a single string"
  (let ((origin (get-origin output))
	(message (get-message output)))
    (async-reply irc-proc target
		 (message "%s: %s" origin message))))

(defun redis-subscribe (text process sender response target)
  "subscribes to a redis pubsub. ARGS are a space separated
list of redis-channels to subscribe to"
  (let ((channels (split-string text)))
    (message "%s->%s" target channels)  
    (mapcar (lambda (channel)
	      ;; need identifier to combine redis-channel AND target
	      (let ((id (format "%s:%s" channel target)))
		(unless (gethash id channel-table)
		  (let ((proc (async-shell-command
			       (apply-partially 'build-reply process target)
			       "redis-cli"
			       "subscribe"
			       channel)))
		    (puthash id proc channel-table)))))
	      channels)))

(puthash "subscribe" 'redis-subscribe command-table)

(defun redis-stop (text process sender response target)
  "Cancels all subscriptons"
  (let ((fn (lambda (key value)
	      (delete-process value))))
    (maphash fn channel-table))
  (clrhash channel-table)
  nil)
(puthash "unsubscribe" 'redis-stop command-table)
