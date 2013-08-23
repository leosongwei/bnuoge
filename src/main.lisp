;;;; -*- Lisp -*-
;;; LeoBlog
;;; Author: Leo.Song
;;; Email: leo_songwei@126.com

;;; Package {{{
(in-package :cl-user)

(defpackage :leoblog
  (:use :common-lisp))

(in-package :leoblog)
(require :hunchentoot)
(require :html-template)
(require :postmodern)
;;; }}}

;;; Data Structures {{{

; Blog's name, string, <=50 chars
(defparameter *blog-name* "Unnamed Blog")
; Blog's subtitle, <= 200 chars
(defparameter *blog-subtitle* "NULL")
; Blog's describe, string,
(defparameter *blog-describe* "NULL")

(defstruct article
  ; Article id, int
  aid
  ; Title, string.
  title
  ; Body, string.
  body
  ; Tags, string."T1,T2,T3..."
  tags
  ; Date, string. Unix universal date. UTC
  date
  ; URL part, string. ONLY alphanumeric and "-"
  url
)

(defstruct reply
  ; Reply id, int
  rid
  ; Related aid
  aid
  ; Name, string, UTF-8 & alphanumeric chars & "-" and "@" and "_" and "."
  name
  ; E-mail, string
  email
  ; Website, string
  website
  ; Date, Unix universal date. UTC
  date
  ; body , string
  body
)
;;; }}}

;;; HTML Generaters {{{

; html configs {{{
(defparameter *recent-article-title-cut* 20)
(defparameter *recent-reply-name-cut* 10)
(defparameter *recent-reply-body-cut* 20)
(defparameter *article-list-body-cut* 200)
; }}}

(defun get-recent-articles ()
  (loop for recent-article in
        (mapcar #'fill-article-struct
                (get-article-reverse-sequence-db 0 5))
        collect
        (list
          :url (article-url recent-article)
          :title-cut (cut-string (article-title recent-article)
                                     *recent-article-title-cut*))))

(defun get-recent-replys ()
  (loop for recent-reply in
        (mapcar #'fill-reply-struct
                (get-global-reply-reverse-sequence-db 0 5))
        collect
        (list :name (cut-string (reply-name recent-reply)
                                *recent-reply-name-cut*)
              :body-cut (cut-string (reply-body recent-reply)
                                    *recent-reply-body-cut*))))

(defun get-article-list ()
  (loop for article-list in
        (mapcar #'fill-article-struct
                (get-article-reverse-sequence-db 0 50))
        collect
        (list :title (article-title article-list)
              :url (article-url article-list)
              :body-cut (cut-string (article-body article-list)
                                    *article-list-body-cut*)
              :date (get-yy-mm-dd-date (article-date article-list))
              :time (get-hh-mm-ss-time (article-date article-list)))))

(defun get-reply-list (aid)
  (loop for reply-list in
        (mapcar #'fill-reply-struct
                (get-reply-aid-db aid))
        collect
        (list :name (reply-name reply-list)
              :body (reply-body reply-list)
              :date (get-yy-mm-dd-date (reply-date reply-list))
              :time (get-hh-mm-ss-time (reply-date reply-list)))))

(defun generate-index-page ()
  (with-output-to-string (stream)
     (html-template:fill-and-print-template
       #P"index.tmpl"
       (list :blog-name *blog-name*
             :recent-article (get-recent-articles)
             :recent-reply (get-recent-replys)
             :article-list (get-article-list))
     :stream stream)))

(defun generate-post-page (url)
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
      #P"post.tmpl"
      (let ((article (fill-article-struct (get-article-url-db url))))
        (list :blog-name *blog-name*
              :recent-article (get-recent-articles)
              :recent-reply (get-recent-replys)
              :article-title (article-title article)
              :article-body (article-body article)
              :article-date (get-yy-mm-dd-date (article-date article))
              :article-time (get-hh-mm-ss-time (article-date article))
              :reply-list (get-reply-list (get-aid-with-url url))))
      :stream stream)))

;;; }}}

;;; Tools {{{

(defun get-utc-timestamp ()
  (let* ((local-universal-time (get-universal-time))
         (timezone (ninth (multiple-value-list (decode-universal-time local-universal-time)))))
    (+ local-universal-time (* 3600 timezone))))

(defun get-yy-mm-dd-date (universal-time)
  (let* ((decoded-time (multiple-value-list (decode-universal-time universal-time)))
    (y (format nil "~4,'0D" (sixth decoded-time)))
    (m (format nil "~2,'0D" (fifth decoded-time)))
    (d (format nil "~2,'0D" (fourth decoded-time))))
    (concatenate 'string y "-" m "-" d)))

(defun get-hh-mm-ss-time (universal-time)
  (let* ((decoded-time (multiple-value-list (decode-universal-time universal-time)))
    (h (format nil "~2,'0D" (third decoded-time)))
    (m (format nil "~2,'0D" (second decoded-time)))
    (s (format nil "~2,'0D" (first decoded-time))))
    (concatenate 'string h ":" m ":" s)))


(defun get-date ()
  "Local time and timezone. Example: 2013-08-23 11:16:53+08"
  (let ((universal-time (get-universal-time)))
    (concatenate 'string
                 (get-yy-mm-dd-date universal-time)
                 " "
                 (get-hh-mm-ss-time universal-time)
                 (let ((timezone (ninth (multiple-value-list (get-decoded-time)))))
                   (cond ((< timezone 0) (concatenate 'string "+"
                                                      (format nil "~2,'0D"
                                                              (abs timezone))))
                         ((> timezone 0) (concatenate 'string "-"
                                                      (format nil "~2,'0D"
                                                              (abs timezone)))))))))

(defun get-utc-date ()
  "UTC time, example: 2013-08-23 03:19:21 UTC"
  (let ((universal-utc-time (get-utc-timestamp)))
    (concatenate 'string
                 (get-yy-mm-dd-date universal-utc-time)
                 " "
                 (get-hh-mm-ss-time universal-utc-time)
                 " UTC")))

(defun make-url-part (title)
  (string-downcase
    (delete-if #'(lambda (x)
                   (not (or (alphanumericp x)
                            (char= #\- x))))
               (substitute #\- #\SPACE title))))

(defun fill-article-struct (list-article)
  (make-article
    :aid (first list-article)
    :title (second list-article)
    :body (third list-article)
    :tags (fourth list-article)
    :date (fifth list-article)
    :url (sixth list-article)))

(defun fill-reply-struct (list-reply)
  (make-reply
    :rid (first list-reply)
    :aid (second list-reply)
    :name (third list-reply)
    :email (fourth list-reply)
    :website (fifth list-reply)
    :date (sixth list-reply)
    :body (seventh list-reply)))

(defun articlep (article)
  (article-aid article))

(defun cut-string (string-to-cut preserve)
  (cond ((> preserve (length string-to-cut)) string-to-cut)
        (T
         (concatenate 'string
                      (subseq string-to-cut 0 preserve)
                      ; Any more elegant way instead of just add "..."?
                      "..."))))
;;; }}}

;;; Configure {{{
; User-name, string, alphanumeric & "_" & "-". <= 50 chars.
(defparameter *user-name* "Leo_Song")
; Passwd, string, <= 128 chars.
; Will be check with md5sum in the future.
(defparameter *passwd* "a")

; PostgreSQL database name
(defparameter *db-name* "leoblog")
; PostgreSQL database user
(defparameter *db-user* "leo")
; PostgreSQL database passwd
(defparameter *db-passwd* "d41d8cd")
; PostgreSQL database location
(defparameter *db-location* "localhost")
;;; }}}

;;; Database Operations {{{
(postmodern:connect-toplevel *db-name* *db-user* *db-passwd* *db-location*)

(defun save-article-db (&key title body tags url)
  (postmodern:query (:insert-into 'article :set
                     'title '$1     'body '$2
                     'tags  '$3     'date '$4
                     'url  '$5)
                    title body tags
                    (get-utc-date)
                    url))

(defun save-reply-db (&key aid name email website body)
  (postmodern:query (:insert-into 'reply :set
                     'aid   '$1     'name    '$2
                     'email '$3     'website '$4
                     'date  '$5     'body  '$6)
                    aid name email website
                    (get-utc-date)
                    body))

; Get article with url, list with 1 article
(defun get-article-url-db (url)
  (car
    (postmodern:query (:select '* :from 'article :where
                     (:= :url '$1)) url)))

; Get article with aid, list with 1 article
(defun get-article-aid-db (aid)
  (car
    (postmodern:query (:select '* :from 'article :where
                       (:= :aid '$1)) aid)))

; Get the last article's AID, number
(defun get-last-article-aid ()
  (caar (postmodern:query (:select (:max 'aid) :from 'article))))

; Get all reply of a AID, list with list
(defun get-reply-aid-db (aid)
  (postmodern:query (:select '* :from 'reply :where
                     (:= :aid '$1)) aid))

; Get reply with rid, list
(defun get-reply-rid-db (rid)
  (car
    (postmodern:query (:select '* :from 'reply :where
                       (:= :rid '$1)) rid)))

; Get article sequence reverse list
(defun get-article-reverse-sequence-db (start amount)
  (postmodern:query (:limit (:order-by
                              (:select '* :from 'article)
                              (:desc 'aid))
                     '$1 '$2)
                    amount start))

; Get global reply sequence list
(defun get-global-reply-reverse-sequence-db (start amount)
  (postmodern:query (:limit (:order-by
                              (:select '* :from 'reply)
                              (:desc 'rid))
                     '$1 '$2)
                    amount start))

(defun get-aid-with-url (url)
  (car (car
         (postmodern:query (:select 'aid :from 'article
                     :where (:= :url '$1)) url))))

; Delete Article with aid
(defun delete-article-aid-db (aid)
  (postmodern:query (:delete-from 'article :where
                     (:= :aid '$1)) aid))

; Delete Reply with rid
(defun delete-reply-rid-db (rid)
  (postmodern:query (:delete-from 'reply :where
                     (:= :rid '$1)) rid))

;;; }}}

;;; Hunchentoot Server {{{
(defvar *ht-server*
  (hunchentoot:start (make-instance
                       'hunchentoot:easy-acceptor
                       :port 8080)))

(hunchentoot:define-easy-handler (index-page :uri "/") ()
  (setf (hunchentoot:content-type*) "html")
  (generate-index-page))

(hunchentoot:define-easy-handler (post-page :uri "/p") (page)
  (setf (hunchentoot:content-type*) "html")
  (generate-post-page (make-url-part page)))
;;; }}}
