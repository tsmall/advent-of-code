(context 'crypto)

(import "/usr/local/opt/openssl/lib/libcrypto.dylib" "MD5")

;; @syntax (crypto:md5 <string> <bool-raw>)
;; @param <string> The string buffer for which to calculate a MD5 hash
;; @param <bool-raw> Return the raw binay buffer when 'true'.
;; @return The 16 Byte MD5 hash as a 32 Byte long hex string or as a 16 byte binary buffer.
;; @example
;; (crypto:md5 "ABC") => "902fbdd2b1df0c4f70b4a5d23525e932"
;;
;; (crypto:md5 (read-file "newlisp-9.1.0.tgz")) => "46c79c93e904df35c6a8474ace406c92"

(define (md5 str raw-flag)
  (if raw-flag
      (let (buff (dup "\000" 16))
        (cpymem (MD5 str (length str) 0) buff 16)
        buff)
    (join
     (map (lambda (x) (format "%02x" (& x 0xff))) 
          (unpack (dup "c" 16) (MD5 str (length str) 0))))))
