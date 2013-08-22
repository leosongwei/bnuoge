;;;; Test Cases

;(defun save-article-db (&key title body tags url)
(save-article-db
  :title "Timestamp test2"
  :body "For testing usage"
  :tags "timestamp"
  :url "timestamp-test2")

;(defun save-reply-db (&key aid name email website body)
(save-reply-db
  :aid 1
  :name "SUDOER"
  :email "ombama@whitehouse.gov"
  :website ""
  :body "LOL")

;Works
(loop for blog-post in (mapcar #'fill-article-struct (get-article-reverse-sequence-db 0 5))
      collect (list :title (article-title blog-post)
                    :body (cut-string  (article-body blog-post) 50)))


