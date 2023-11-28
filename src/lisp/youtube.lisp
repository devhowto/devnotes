;;
;; Transforms Youtube URL into its RSS feed URL.
;;
(defun yt-channel-rss (channel-url)
  (with-temp-buffer
    (insert-buffer (url-retrieve-synchronously channel-url))
    (dom-attr (first (dom-search (libxml-parse-html-region (point-min)
                                                           (point-max))
                                 (lambda (node)
                                   (and (eq 'link (dom-tag node))
                                        (equal "RSS" (dom-attr node 'title))))))
              'href)))
