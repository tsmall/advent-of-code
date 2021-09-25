### Advent of Code 2019 - Day 01

# Given the mass of the spacecraft's modules on stdin, one number per line,
# this program calculates and then reports the fuel required for the modules,
# for the added fuel, and the total.

# See problem.txt for the full problem description.


(defn module-fuel
  "Calculates the fuel required for a module."
  [module]
  (- (math/floor (/ module 3)) 2))


(defn added-fuel
  "Calculates the added fuel needed for the fuel itself."
  [fuel]
  (let [extra (module-fuel fuel)]
    (if (neg? extra)
      0
      (+ extra (added-fuel extra)))))


(def masses
  "Generates the masses of the modules, one at a time."
  (generate
   [line :iterate (file/read stdin :line)
    :let [trimmed (string/trim line)]]
   (scan-number trimmed)))


(var total-module-fuel 0)
(var total-fuel-fuel 0)

(loop [mass :in masses]
  (let [fuel  (module-fuel mass)
        extra (added-fuel fuel)]
    (+= total-module-fuel fuel)
    (+= total-fuel-fuel extra)))

(printf "Part 1: %d" total-module-fuel)
(printf "Part 2: %d" (+ total-module-fuel total-fuel-fuel))
