;;; -*- mode: Common-Lisp; coding: utf-8-unix -*-

(defpackage #:jlk.string-index
  (:use #:common-lisp)
  (:export #:make-index
	   #:search-index
	   #:flatten-index
	   #:human-search)
  (:documentation "A simple string indexing library.

Produces a tree of alists, indexed by character."))

(in-package #:jlk.string-index)

;;; Note the alist in the cdr of the final cons will display as an
;;; extension of the list rather than as an alist - don't be fooled!

;;; cannot use standard alist as we need to be able to return a value
;;; and further expansions, eg. for value-a and value-aa
;;; 
;;; therefore define a new type of list, ilist. assoc will still work
;;; unchanged.

(defun icons (key datum children ilist)
  (cons (list key datum children) ilist))

(defmacro ikey (iterm)
  `(car ,iterm))

(defmacro ivalue (iterm)
  `(cadr ,iterm))

(defmacro ichildren (iterm)
  `(caddr ,iterm))

(defun make-index (index-terms)
  "Provide an alist of (string . value) terms for indexing"
  (labels ((recur (chars ilist value)
	     (destructuring-bind (ch . chars)
		 chars
	       ;; (log:> :debug "~s ~s" ch alist)
	       ;(declare (type character ch))
	       (if chars
		   (if (assoc ch ilist)
		       (progn (setf (ichildren (assoc ch ilist))
				    (recur chars
					   (ichildren (assoc ch ilist))
					   value))
			      ilist)
		       (icons ch
			      nil
			      (recur chars nil value)
			      ilist))
		   (if (assoc ch ilist)
		       (progn (setf (ivalue (assoc ch ilist))
				    value)
			      ilist)
		       (icons ch value nil ilist))))))
    (loop
       with index = nil
       for ((term . value)) on index-terms
       do (setf index (recur (coerce term 'list)
			     index
			     value))
       finally (return index))))

(defun test-make-colour-index ()
  (make-index
   (loop
      with colours = '(:red :green :blue :grey :greyblack :black :white)
      for colour in colours
      collect (cons (string-downcase (symbol-name colour))
		    colour))))

(defun search-index (term index)
  "Return the value of the match and any remaining ichildren."
  (labels ((recur (chars ilist)
	     (if (not chars)
		 index
		 (destructuring-bind (ch . rest)
		     chars
		   (let ((match (assoc ch ilist)))
		     (when match
		       (if rest
			   (when (consp (cdr match))
			     (recur rest (ichildren match)))
			   (values (ivalue match)
				   (ichildren match)))))))))
    (let ((terms (coerce term 'list)))
      (if terms
	  (recur terms
		 index)
	  (values nil
		  index)))))

(defun flatten-index (index)
  "Turn an alist of alists into strings"
  (let ((rv nil))
    (labels ((recur (ilist current-match)
	       (loop
		  for ((ch val rest)) on ilist
		  when val
		  do (push (coerce (reverse (cons ch current-match)) 'string)
			   rv)
		  when (consp rest)
		  do (recur rest
			    (cons ch current-match)))))
      (recur index nil)
      (values rv))))

(defun human-search (term index &optional (min-length 0))
  "Search the index, providing a minimum length of terms before
  searching. Return either the matched value or nil. A list of the
  possible completions is returned as the second value."
  (when (>= (length term)
	    min-length)
    (multiple-value-bind (match rest)
	(search-index term index)
      (values match
	      (flatten-index rest)))))

(defun test-usage ()
  (let ((idx (test-make-colour-index)))
    (multiple-value-bind (match completions)
	(human-search "gre" idx)
      (format t "match: ~s~%completions: ~s~%" match completions))
    (multiple-value-bind (match completions)
	(human-search "green" idx)
      (format t "match: ~s~%completions: ~s~%" match completions))
    (multiple-value-bind (match completions)
	(human-search "greenx" idx)
      (format t "match: ~s~%completions: ~s~%" match completions))
    (multiple-value-bind (match completions)
	(human-search "ye" idx)
      (format t "match: ~s~%completions: ~s~%" match completions))))
