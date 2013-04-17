;;;
;;; file.jpeg - read JPEG data stream
;;;
;;;   Copyright (c) 2001-2005 Shigenobu Kimura, All rights reserved.
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
;;;  $Id: jpeg.scm,v 1.1 2005/11/12 12:01:59 shirok Exp $
;;;

;; [SK] This module was originally a part of Shigenobu Kimura's library
;; collection.  I took the source, adjusted some names, and packaged it
;; as an extension.

(define-module file.jpeg
  (use gauche.uvector)
  (export JPEG_SOI JPEG_EOI JPEG_APP0 JPEG_APP1 JPEG_RSTT0 JPEG_RST7 JPEG_SOS
          <jpeg-format-error>
          jpeg-read-marker
          jpeg-read-stream)
  )
(select-module file.jpeg)

(define-condition-type <jpeg-format-error> <error> #f)

;;;
;;; JPEG markers
;;;
(define-constant JPEG_SOI  #xD8)        ;start of image
(define-constant JPEG_EOI  #xD9)        ;end of image
(define-constant JPEG_APP0 #xE0)        ;used by JFIF
(define-constant JPEG_APP1 #xE1)        ;used by EXIF
(define-constant JPEG_RST0 #xD0)
(define-constant JPEG_RST7 #xD7)
(define-constant JPEG_SOS  #xDA)

;; jpeg-read-marker &optional port
;;   Assuming PORT is at the point of beginning of JPEG marker.
;;   This funtion read one marker and its accompanied data, and
;;   returns a list of the marker (integer) and the data (u8vector).
;;   Throws <jpeg-format-error> if PORT doesn't point to a valid marker
;;   or it encounters unexpected EOF.
(define (jpeg-read-marker . maybe-port)
  (define p (get-optional maybe-port (current-input-port)))

  (define (fmt1? m)
    (or (= m JPEG_SOI)
        (= m JPEG_EOI)
        (<= JPEG_RST0 m JPEG_RST7)))

  (let ((ff (read-byte p)))
    (unless (eqv? ff #xFF)
      (errorf <jpeg-format-error>
              "JPEG marker does not start with FF (got ~a), reading from ~s"
              ff p)))
  (let ((m (read-byte p)))
    (if (fmt1? m) 
      (list m)
      (or (and-let* ((x (read-byte p))
                     ( (not (eof-object? x)) )
                     (y (read-byte p))
                     ( (not (eof-object? y)) )
                     (l (- (+ (* x 256) y) 2))
                     (v (make-u8vector l))
                     (d (read-block! v p))
                     ( (= l d) ))
            (list m v))
          (jpeg-eof-error p)))))

;; jpeg-read-stream &optional port
;;    Read an entire JPEG data stream from PORT, splitting it into the
;;    chunks of the data and leading markers.
(define (jpeg-read-stream . maybe-port)
  (define p (get-optional maybe-port (current-input-port)))
  (define (rst? m) (<= JPEG_RST0 m JPEG_RST7))
  (define (rdi l)
    (let1 d (read-byte p)
      (cond
       ((eof-object? d) (jpeg-eof-error d))
       ((eqv? d #xFF)
        (let1 e (read-byte p)
          (cond
           ((eof-object? d)   (jpeg-eof-error d))
           ((eqv? e JPEG_EOI) (reverse l))
           ((eqv? e #x00)     (rdi (cons d l)))
           ((rst? e)          (rdi (cons e (cons d l))))
           (else
            (errorf <jpeg-format-error>
                    "JPEG image data corrupted ~2'0X, reading from ~a"
                    e p)))))
       (else (rdi (cons d l))))))

  (let lp ((mk (jpeg-read-marker p))
           (l '()))
    (if (eq? (car mk) JPEG_SOS) 
      (reverse `((,JPEG_EOI) ,(rdi '()) ,mk . ,l))
      (lp (jpeg-read-marker p) (cons mk l)))))

;; util
(define (jpeg-eof-error port)
  (errorf <jpeg-format-error>
          "unexpected end-of-file during reading JPEG file from ~s" p))

(provide "file/jpeg")
