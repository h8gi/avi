(use coops srfi-4-utils)
(define file-name "data/sea.avi")

(define (u8vector->string u8v #!key (big? #f))
  (let ([func (if big?
                  (lambda (x acc) (conc acc (integer->char x)))
                  (lambda (x acc) (conc (integer->char x) acc)))])
    (u8vector-fold func "" u8v)))

(define (u8vector->number u8v #!key (big? #f))
  (if big?
      (u8vector-fold (lambda (x acc) (fx+ (arithmetic-shift acc 8) x)) 0 u8v)
      (u8vector-foldi (lambda (i x acc) (fx+ (arithmetic-shift x (* i 8)) acc)) 0 u8v)))

(define (read-avi-fourcc #!optional value)
  (let ([str (u8vector->string (read-u8vector 4) #:big? #t)])
    (when value (assert (string=? value str)))
    value))

(define (read-avi-size)
  (u8vector->number (read-u8vector 4)))

(define (read-avi-dword)
  (u8vector->number (read-u8vector 4)))

(define (consume-n-byte n)
  (read-u8vector n))


(define (read-avi #!optional (port (current-input-port)))
  (with-input-from-port port
    (lambda ()
      (let ([riff      (read-avi-fourcc "RIFF")]
            [file-size (read-avi-size)]
            [file-type (read-avi-fourcc "LIST")])
        (list riff file-size file-type
              (read-avi-header-list))))))

(define (avi-header-list-read)
  (let ([list-id   (read-avi-fourcc "LIST")]
        [list-size (read-avi-size)]
        [list-type (read-avi-fourcc "hdrl")])
    (list list-id list-size list-type
          (read-avi-main-header-chunk)
          (read-avi-stream-header-list)
          )))

(define (read-avi-main-header-chunk)
  (let ([fcc (read-avi-fourcc "avih")]         ; avih
        [cb  (read-avi-dword)]          ;56
        [micro-sec-per-frame (read-avi-dword)]
        [max-bytes-per-sec (read-avi-dword)]
        [padding-granularity (read-avi-dword)]
        [flags (read-avi-dword)]
        [total-frames (read-avi-dword)]
        [initial-frames (read-avi-dword)]
        [streams (read-avi-dword)]
        [suggested-buffer-size (read-avi-dword)]
        [width (read-avi-dword)]
        [height (read-avi-dword)])
    (consume-n-byte (- cb 40))
    (list fcc cb micro-sec-per-frame max-bytes-per-sec padding-granularity
          flags total-frames initial-frames streams suggested-buffer-size
          width height)))

(define (read-avi-stream-header-list)
  (let ([list-id (read-avi-fourcc "LIST")]
        [list-size (read-avi-size)]
        [list-type (read-avi-fourcc "strl")])
    (list list-id list-size list-type
          ())))

;; (define (avi-read-switch #!optional (port (current-input-port)))
;;   (with-input-from-port port
;;     (lambda ()
;;       (let ([four-cc (u8vector->number (read-u8vector 4) #:big? #t)])
;;         (cond [(string=? four-cc "LIST")
;;                (avi-read-list)]
;;               [else
;;                (avi-read-chunk)])))))

