;; -*- lexical-binding: t -*-

(require 'rcirc)
(require 'cl)

;; lookup in command-table the function for a command if it exists, call it and pass it the rest of the args.
(setq command-table (make-hash-table :test 'equal))
(setq async-command-table (make-hash-table :test 'equal))

;; hook for rcirc print    
(defun my-rcirc-highlight-hook (process sender response target text)
  (when (and (string-match (concat "^" (regexp-quote (rcirc-nick process))) text)
	     (not (string= (rcirc-server-name process) sender)))
    ;; do your stuff
    (let ((text
	   (replace-regexp-in-string (format "^%s[^a-zA-Z0-9]*" (regexp-quote (rcirc-nick process))) "" text)))
      (if (async-command-p text)
	  (handle-async-command process target text)
	(let ((reply (handle-command text)))
	  (when reply
	    (rcirc-send-message process target reply)))))))
;; add the hook
(add-hook 'rcirc-print-functions 'my-rcirc-highlight-hook)

(defun handle-command (text)
  (let ((words (split-string text)))
    (let ((command (gethash (car words) command-table)))
      (when command
	(funcall command (mapconcat 'identity (cdr words) " "))))))

(defun async-reply (process target reply)
  (rcirc-send-message process target reply))

(defun handle-async-command (process target text)
  (let ((words (split-string text)))
    (let ((command (gethash (car words) async-command-table)))
      (when command
	(funcall command (lambda (text)
			   (async-reply process target text))
		 (mapconcat 'identity (cdr words) " "))))))

(defun async-command-p (text)
  (let ((words (split-string text)))
    (gethash (car words) async-command-table)))

;; Let's write some commands.
(defun ping-command (text)
  (concat "pong " text))
(puthash "ping" 'ping-command command-table)

(defun commands-command (text)
  (let (commands)
    (let ((fn (lambda (key value)
		(setq commands (cons key commands)))))
    (maphash fn
	     command-table)
    (maphash fn async-command-table)
    (mapconcat 'identity (sort commands 'string<) " "))))
(puthash "commands" 'commands-command command-table)

(defun say-command (text)
  text)
(puthash "say" 'say-command command-table)

(defun async-say-command (fn text)
  (funcall fn text))
(puthash "async-say" 'async-say-command async-command-table)

(defun source-command (text)
  "https://github.com/sauerkrause/bot.emacs")
(puthash "source" 'source-command command-table)
