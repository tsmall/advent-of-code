(defmacro swap
  [x y]
  (def $tmp (gensym))
  ~(let [,$tmp ,x]
     (set ,x ,y)
     (set ,y ,$tmp)))

(defn- dragon-curve-step
  [data buf]
  (buffer/clear buf)
  (buffer/push-string buf data)
  (buffer/push-byte buf (chr "0"))
  (each c (string/reverse data)
    (buffer/push-byte
     buf
     (if (= c (chr "0")) (chr "1") (chr "0"))))
  buf)

(defn dragon-curve
  [data desired-length]
  (var buf @"")
  (var result @"")
  (buffer/push-string result data)
  (while (< (length result) desired-length)
    (dragon-curve-step result buf)
    (swap buf result))
  (string/slice result 0 desired-length))

(defn- checksum-step
  [data buf]
  (buffer/clear buf)
  (var i 0)
  (while (< i (dec (length data)))
    (if (= (get data i) (get data (inc i)))
      (buffer/push-byte buf (chr "1"))
      (buffer/push-byte buf (chr "0")))
    (+= i 2))
  buf)

(defn checksum
  [data]
  (var buf @"")
  (var result @"")
  (buffer/push-string result data)
  (while (even? (length result))
    (checksum-step result buf)
    (swap buf result))
  result)

(defn dragon-checksum
  [input desired-length]
  (checksum (dragon-curve input desired-length)))

(def input
  "01000100010010111")

(defn part1
  []
  (dragon-checksum input 272))

(defn part2
  []
  (dragon-checksum input 35651584))

(defn main
  [& args]
  (printf "Part 1: %s" (part1))
  (printf "Part 2: %s" (part2)))
