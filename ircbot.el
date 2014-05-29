;; -*- lexical-binding: t -*-

(require 'rcirc)
(require 'dom)
(require 'web)
(require 'cl)
(require 'list-utils)

;; lookup in command-table the function for a command if it exists, call it and pass it the rest of the args.
(setq command-table (make-hash-table :test 'equal))
(setq custom-command-table (make-hash-table :test 'equal))
(setq async-command-table (make-hash-table :test 'equal))
(setq command-needs-auth-table (make-hash-table :test 'equal))
(setq pending-funcalls (make-hash-table :test 'equal))

;; hook for rcirc print    
(defun my-rcirc-highlight-hook (process sender response target text)
  (when (and (string-match (concat "^" (regexp-quote (rcirc-nick process))) text)
	     (not (string= (rcirc-server-name process) sender)))
    ;; do your stuff
    (let ((text
	   (replace-regexp-in-string (format "^%s[^a-zA-Z0-9]*" (regexp-quote (rcirc-nick process))) "" text)))
      (handle-authable-command
       (gethash (car (split-string text)) command-needs-auth-table)
       process sender target text))))

(defun handle-authable-command (auth process sender target text)
  (flet ((handle-line (line)
		      (rcirc-send-message process target reply)))
	(if auth
	    nil
	  (if (async-command-p text)
	      (handle-async-command process sender nil target text)
	    (let* ((reply (handle-command text process sender nil target))
		   (reply-lines (if (listp reply)
				    reply
				  (cons (if (stringp reply)
					    reply
					  (format "%s" reply)) nil))))
	      (mapcar (apply-partially 'rcirc-send-message process target)
		      reply-lines))))))

;; add the hook
(defun my-rcirc-url-hook (process sender response target text)
  (when (not (string= (rcirc-server-name process) sender))
    (let ((urls (cl-remove-if (lambda (i)
				(let* ((url-struct (url-generic-parse-url i))
				       (type (url-type url-struct)))
				  (or (eq nil type)
				      (not (or (equal "http" type)
					       (equal "https" type)))
				      (string-match "tinyurl.com" i))))
			      (split-string text))))
      (if urls
	  (mapcar (lambda (url)
		    (funcall (apply-partially 'handle-async-command process sender nil target)
			     (format "tinyurl %s" url)))
		  urls)))))

(add-hook 'rcirc-print-functions 'my-rcirc-highlight-hook)
(add-hook 'rcirc-print-functions 'my-rcirc-url-hook)

(defun my-rcirc-authed-hook (process sender response target text)
  (when (string= sender "NickServ")
    (let* ((words (split-string text))
	   (nick (elt words 0))
	   (second (elt words 1))
	   (auth-status (when (string= second "ACC")
			  (string= (elt words 2) "3"))))
      (when auth-status
	  (mapcar 'funcall
		  (gethash nick pending-funcalls)))
      (remhash nick pending-funcalls))))
(add-hook 'rcirc-print-functions 'my-rcirc-authed-hook)

(defun append-hash (key value hash)
  (let ((pending-calls (gethash key hash)))
    (puthash key (append pending-calls (list value)) hash)))

(defun handle-command (text process sender response target)
  (let ((words (split-string text)))
    (let ((command (let ((builtin (gethash (downcase (car words)) command-table)))
		     (if builtin
			 builtin
		       (gethash (downcase (car words)) custom-command-table)))))
      (when command
	(apply command (list (mapconcat 'identity (cdr words) " ") 
			       process 
			       sender 
			       response 
			       target))))))

(defun async-reply (process target reply)
  (let ((reply-lines (if (listp reply)
				    reply
				  (cons (if (stringp reply)
					    reply
					  reply) nil))))
    (mapcar (apply-partially 'rcirc-send-message process target)
	    reply-lines)))

(defun join-strings (words)
  (mapconcat 'identity words " "))

(defun handle-async-command (process sender response target text)
  (let ((words (split-string text)))
    (let ((command (gethash (downcase (car words)) async-command-table)))
      (when command
	(funcall command (apply-partially
			  'async-reply process target)
		 (mapconcat 'identity (cdr words) " ")
		 process sender response target)))))

(defun authed-command-p (text)
  (let ((words (split-string text)))
    (gethash (downcase (car words)) command-needs-auth-table)))

(defun async-command-p (text)
  (let ((words (split-string text)))
    (gethash (downcase (car words)) async-command-table)))

;; Let's write some commands.
(defun commands-command (text process sender response target)
  (let (commands)
    (let ((fn (lambda (key value)
		(setq commands (cons key commands)))))
    (maphash fn
	     command-table)
    (maphash fn async-command-table)
    (mapconcat 'identity (sort commands 'string<) " "))))
(puthash "commands" 'commands-command command-table)

(defun custom-commands-command (text process sender response target)
  (mapconcat 
   'identity
   (sort
    (remove-if (lambda (k)
		 (or (gethash k command-table)
		     (gethash k async-command-table)))
	       (let
		   (commands)
		 (maphash (lambda (k v)
			    (setq commands (cons k commands)))
			  custom-command-table)
		 commands))
    'string<)
   " "))
(maphash (lambda (k v)
	   k)
	custom-command-table)
(puthash "custom-commands" 'custom-commands-command command-table)
(custom-commands-command nil nil nil nil nil)

(defun source-command (text process sender response target)
  "https://github.com/sauerkrause/bot.emacs")
(puthash "source" 'source-command command-table)

(defun spit-page (fn url)
  (web-http-get (lambda (httpc header page-data)
		  (funcall fn page-data))
		:url url))

(defun html-from-body (body)
  (with-temp-buffer
    (insert body)
    (libxml-parse-html-region (point-min)
			      (point-max))))
(defun get-elements-by-tag (dom tag)
  (let ((parsed dom))
    (let ((cur parsed)
	  (out nil))
      (while cur
	(if (eq (car cur) tag)
	      (setq out (cons tag (cdr cur))))
	(if (consp (car cur))
	    (let ((subout (get-elements-by-tag (car cur) tag)))
	      (if subout
		  (setq out (cons subout out)))))
	(setq cur (cdr cur)))
      (list-utils-flatten out))))
(defun parse-dom (dom)
  (if (stringp dom)
      dom
    (let ((tag (car dom))
	  (subtags (cddr dom)))
      (cons tag (mapcar 'parse-dom subtags)))))
(defun find-title (body)
  (let ((titles
	 (get-elements-by-tag
	  (parse-dom (html-from-body body)) 'title)))
    (when titles
      (elt titles 1))))

(defun put-title-tinyurl (fn url page-data)
  (web-http-get
   (lambda (httpc header body)
     (let ((content-type (gethash 'content-type header)))
       (if (equal "text/html" (car (split-string content-type "; ")))
	(funcall fn
		 (format "%s%s" page-data 
			 (let ((title (find-title body)))
			   (if title
			       (format " : (%s)" title)
			     ""))))
	(funcall fn page-data))))
   :url url))

(defun tinyurl (fn url)
  (let ((query-data (make-hash-table :test 'equal)))
    (puthash 'url url query-data)
    (web-http-post
     (lambda (httpc header page-data)
       (put-title-tinyurl fn url page-data))
     :url "http://tinyurl.com/api-create.php"
     :data query-data)))

(defun tinyurl-command (fn text process sender response target)
  (let* ((url (car (split-string text))))
    (tinyurl fn url)))
(puthash "tinyurl" 'tinyurl-command async-command-table)

(defun xml-from-body (body)
  (with-temp-buffer
    (insert body)
    (xml-parse-region (point-min) (point-max))))

(defun get-weather-description (xml)
  (format "%s and %s at %s"
	  (get-weather-attribute 'weather xml)
	  (get-weather-attribute 'temperature_string xml)
	  (get-weather-attribute 'location xml)))

(defun get-weather-attribute (sym xml)
  (let ((tmp-node
          (car (xml-get-children (car xml)
                                 sym))))
    (car (cddr tmp-node))))
(defun weather-command (fn text process sender response target)
  (let* ((station (car (split-string text)))
	 (url (format "http://w1.weather.gov/xml/current_obs/%s.xml" station)))
    (web-http-get (lambda (httpc headers body)
		    (funcall fn (get-weather-description (xml-from-body body))))
		  :url url)))
(puthash "weather" 'weather-command async-command-table)


(defun random-choice (choices)
  (if (and choices (listp choices))
	(let ((element 
	       (elt choices (random (length choices)))))
	  element)))

(defmacro define-reply (cmd choices)
  `(progn
     (defun ,cmd (fn text process sender response target)
       (let ((choices ,choices))
	 (funcall fn (random-choice choices))))
     (puthash (symbol-name ',cmd) ',cmd async-command-table)))


(defun define-command (text process sender response target)
  (let ((name (downcase (car (split-string text))))
	(replies (cdr (split-string text))))
    (puthash name (apply-partially
		    (lambda (r txt p s resp trgt)
		      (random-choice r))
		    replies)
	     custom-command-table)
    (format "%s defined!"
	    (if (gethash name custom-command-table)
		name
	      (format "%s could not be" name)))))
(puthash "define" 'define-command command-table)

(defun undefine-command (text process sender response target)
  (mapcar (lambda (i)
	    (remhash i custom-command-table))
	  (split-string text))
  (format "Deleted %s" text))
(puthash "undefine" 'undefine-command command-table)

(defun purge-customs-command (text process sender response target)
  (maphash (lambda (k v)
	     (remhash k custom-command-table))
	   custom-command-table)
  (setq custom-command-table (make-hash-table :test 'equalp)))
(puthash "purge" 'purge-customs-command command-table)
