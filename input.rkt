#lang racket

(require net/url)

(provide input)

(define (input day)
  (let ([filename (~a "inputs/" day)])
    (if (file-exists? filename)
        (file->string filename)
        (let ([input (download day)])
          (unless (directory-exists? (path-only filename))
            (make-directory (path-only filename)))
          (call-with-output-file filename (curry display input))
          input))))

(define (session-cookie)
  (let ([filename "session.cookie"])
    (if (file-exists? filename)
        (file->string filename)
        (error "Missing `session.cookie`."))))

(define (download day)
  (let*-values ([(port headers)
                (get-pure-port/headers
                 (string->url(~a "https://adventofcode.com/2024/day/" day "/input"))
                 (list (~a "cookie: " (session-cookie)))
                 #:status? #t)]
                [(str) (port->string port)])
    (close-input-port port)
    (unless (string-contains? headers "200 OK")
      (error (~a "Error while downloading input " day ": " str)))
    str))
