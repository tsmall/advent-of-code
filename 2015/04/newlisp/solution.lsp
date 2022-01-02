(load "md5.lsp")

(set 'key "ckczppom")
(set 'num 0)

(define (next-hash)
  (crypto:md5 (string key (inc num))))

(set 'hash (next-hash))
(until (starts-with hash "00000")
       (set 'hash (next-hash)))

(println "Part 1: " num)

(until (starts-with hash "000000")
       (set 'hash (next-hash)))

(println "Part 2: " num)

(exit)
