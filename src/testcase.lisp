;;;; Test Cases

;(defun save-article-db (&key title body tags url)
(save-article-db
  :title "Test Case3"
  :body "For testing usage"
  :tags "Test"
  :url "test-case3")

;(defun save-reply-db (&key aid name email website body)
(save-reply-db
  :aid 1
  :name "SUDOER"
  :email "ombama@whitehouse.gov"
  :website ""
  :body "LOL")
