(in-package :info.read-eval-print.web-server)

(named-readtables:in-readtable quek:|#"|)

(defparameter *root* "~/")

(defvar *httpd*)
(defvar *params*)

(alexandria:define-constant +crlf+ (format nil "~c~c" #\cr #\lf) :test #'equalp)

(deftype ubyte () '(unsigned-byte 8))
(deftype ubytes () '(simple-array ubyte))


(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; (defparameter *optimize* '(optimize (speed 0) (safety 3) (debug 3) (compilation-speed 0)))
  (defparameter *optimize* '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))))

(defun start (&key (port 8888))
  (declare #.*optimize*)
  (setf *httpd* (make-instance 'iolib.multiplex:event-base))
  (iolib.sockets::with-open-socket (socket :address-family :ipv4
                                           :type :stream
                                           :connect :passive
                                           :local-host "0.0.0.0"
                                           :local-port port
                                           :reuse-address t)
    (iolib.sockets::listen-on socket)
    (describe socket)
    (iolib.multiplex::set-io-handler
     *httpd*
     (iolib.streams::fd-of socket)
     :read (lambda (fd event exception)
             (declare (ignore fd event exception))
             (let* ((client-socket (iolib.sockets:accept-connection socket))
                    (fd (iolib.streams:fd-of client-socket)))
               (iolib.multiplex:set-io-handler
                *httpd* fd
                :read (lambda (_fd event exception)
                        (declare (ignore _fd event exception))
                        (unwind-protect
                             (handler client-socket)
                          (iomux:remove-fd-handlers *httpd* fd)
                          (close client-socket)))))))
    (iolib.multiplex:event-dispatch *httpd*)))

(alexandria:define-constant +url-decode-table+
    (let ((array (make-array 255 :element-type 'fixnum
                                 :initial-element -1)))
      (let ((i (scan-range :from (char-code #\0) :upto (char-code #\9)))
            (n (scan-range)))
        (collect-ignore (setf (aref array i) n)))
      (let ((i (scan-range :from (char-code #\a) :upto (char-code #\f)))
            (n (scan-range)))
        (collect-ignore (setf (aref array i) (+ n 10))))
      (let ((i (scan-range :from (char-code #\A) :upto (char-code #\F)))
            (n (scan-range)))
        (collect-ignore (setf (aref array i) (+ n 10))))
      (setf (aref array (char-code #\+)) (char-code #\space))
      array)
  :test #'equalp)

(declaim (inline nurl-decode))
(defun nurl-decode (buffer start end)
  (declare #.*optimize*
           (type ubytes buffer)
           (type fixnum start end))
  (loop with i = start
        for j from start
        do (when (= i end)
             (return-from nurl-decode
               (babel:octets-to-string buffer :encoding :utf-8
                                              :start start
                                              :end j)))
           (let ((code (aref buffer i)))
             (if (= code #.(char-code #\%))
                 (progn
                   (setf (aref buffer j)
                         (+ (the fixnum (* (aref +url-decode-table+ (aref buffer (1+ i))) 16))
                            (aref +url-decode-table+ (aref buffer (+ 2 i)))))
                   (incf i 3))
                 (progn (setf (aref buffer j) code)
                        (incf i))))))

(declaim (inline parse-query-string))
(defun parse-query-string (buffer start end)
  (declare #.*optimize*)
  (declare (type ubytes buffer))
  (loop with h = (make-hash-table :test #'eq)
        for key-start = start then (1+ val-end)
        for key-end = (position #.(char-code #\=) buffer :start key-start :end end)
        for val-start = (1+ key-end)
        for val-end = (or (position #.(char-code #\&) buffer :start val-start :end end) end)
        do (setf (gethash (intern (nstring-upcase (nurl-decode buffer key-start key-end))
                                  :keyword)
                          h)
                 (nurl-decode buffer val-start val-end))
        if (= val-end end)
          do (return-from parse-query-string h)))

(defparameter *handler-package* (find-package :info.read-eval-print.web-server)
  "package of handler functions.")

(declaim (inline parse-request))
(defun find-handler-symbol (path)
  (declare #.*optimize*)
  (let ((symbol-name (string-upcase path)))
    (or (and (string= "/index.html" symbol-name)
             (find-symbol "ROOT" *handler-package*))
        (find-symbol symbol-name *handler-package*)
        (prog1 '/404
                  (format t "~&~a not found in ~a." path *handler-package*)))))

(declaim (inline parse-request))
(defun parse-request (buffer nbytes)
  (declare #.*optimize*
           (type ubytes buffer))
  (let* ((start (position #x20 buffer :end nbytes))
         (end (position #x20 buffer :start (+ start 2) :end nbytes))
         (? (position #x3f buffer :start (+ start 2) :end end))
         (path (let ((x (babel:octets-to-string buffer
                                                :encoding :utf-8
                                                :start (1+ start)
                                                :end (or ? end))))
                 (if (string= "/" x)
                     "/index.html"
                     x)))
         (params (if ?
                     (parse-query-string buffer (1+ ?) end)
                     (make-hash-table :test #'eq)))
         (handler-symbol (find-handler-symbol path)))
    (values handler-symbol params path)))

(declaim (inline send-file))
(defun send-file (file)
  (declare #.*optimize*)
  (let* ((header (the ubytes (babel:string-to-octets #"""HTTP/1.0 200#,+crlf+,Content-Type: text/html; charset=utf-8;#,+crlf+,#,+crlf+"""
                                                     :encoding :utf-8)))
         (header-length (length header)))
    (with-open-file (in file :element-type 'ubyte)
      (let* ((length (the fixnum (+ (the fixnum (file-length in)) header-length)))
             (buffer (make-array length :element-type 'ubyte)))
        (dotimes (i header-length)
          (setf (aref buffer i) (aref header i)))
        (read-sequence buffer in :start header-length)
        (values buffer length)))))

(declaim (inline request))
(defun request (buffer nbytes)
  (declare #.*optimize*
           (type ubytes buffer))
  (multiple-value-bind (symbol *params* path) (parse-request buffer nbytes)
    (let ((file (probe-file (concatenate 'string *root* path))))
      (if file
          (send-file file)
          (funcall (symbol-function symbol))))))

(defun handler (client-socket)
  (declare #.*optimize*)
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer))
    (multiple-value-bind (buffer nbytes) (iolib.sockets:receive-from client-socket :buffer buffer)
      (declare (type fixnum nbytes))
      ;; イベントが2回発生し、2回目の receive-from は 0 バイトになる。そういうもの?
      (when (plusp nbytes)
          (multiple-value-bind (buffer nbytes) (request buffer nbytes)
            (iolib.sockets:send-to client-socket buffer :end nbytes))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-response (content)
    (declare #.*optimize*)
    (let* ((response #"""HTTP/1.0 200#,+crlf+,Content-Type: text/html; charset=utf-8;#,+crlf+,#,+crlf+,#,content""")
           (buffer (babel:string-to-octets response :encoding :utf-8)))
      (declare (type ubytes buffer))
      (values buffer (length buffer)))))

(define-compiler-macro make-response (&whole form content &environment env)
  (if (constantp content env)
      (multiple-value-bind (buffer nbytes) (make-response content)
        `(values ,buffer ,nbytes))
      form))

(defun params (key &optional default)
  (gethash key *params* default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sample pages
(defun root ()
  (declare #.*optimize*)
  (make-response #"""<html><body><h1>This is root.</h1></body></html>"""))

(defun /hello ()
  (declare #.*optimize*)
  (let ((name (gethash :name *params* "てめちゃん")))
   (make-response #"""<html><body><h1>hello #,name,!</h1></body></html>""")))

(defun /404 ()
  (declare #.*optimize*)
  (make-response "<html><body><h1>404</h1></body></html>"))

;;(sb-thread:make-thread #'start :name "info.read-eval-print.web-server")