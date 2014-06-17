;; -*- lexical-binding: t -*-
(require 'elnode)
(eval-when-compile
  (require 'cl))
(require 'cl)
(defun get-commands nil
  (let ((names nil))
    (maphash (lambda (k v)
	       (setq names (cons k names)))
	     command-table)
    (maphash (lambda (k v)
	       (setq names (cons k names)))
	     async-command-table)
    (sort names 'string<)))
(defun delistify (list)
  (with-temp-buffer
    (dolist (item list (buffer-string))
      (insert (format "%s<br>\n" item)))))

(defun* http-wrapify (str &key (title "GNUFO"))
  (lambda (httpcon)
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (elnode-http-return
     httpcon
     (format "<html><head><title>%s</title></head><body>%s</body></html>" title str))))

(defun get-command-documentation (cmd)
  (setq fn (gethash cmd command-table))
  (unless fn (setq fn (gethash cmd async-command-table)))
  (if fn
      (documentation fn)
    ""))

(defun app-routes nil
  (get-commands))
(app-routes)
(mapcar 'get-command-documentation (app-routes))
(defun linkify (cmds)
  (mapcar
   (lambda (c)
     (format "<a href=\"/%s/\">%s</a>" c c))
   cmds))
(defun routes nil
  (let ((alist (acons "/"
	   (list (http-wrapify (format "<h1>Commands</h1>%s" (delistify (linkify (get-commands)))) :title "Commands"))
	   nil)))
    (mapc (lambda (c)
	    (setq alist
		  (acons (format "/%s/" c)
			(list (http-wrapify (get-command-documentation c) :title c))
			 alist)))
	  (app-routes))
    alist))

(defun root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon (routes)))

(elnode-start 'root-handler :port 8009 :host "0.0.0.0")

