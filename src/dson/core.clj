(ns dson.core
  (:require [instaparse.core :as insta]))

(def parse-dson
  (insta/parser
    "value = string | number | object | array | true | false | null
     object = 'such wow' | 'such' members 'wow'
     members = pair | pair ',' members | pair '.' members | pair '!' members | pair '?' members
     pair = string 'is' value
     array = 'so many' | 'so' elements 'many'
     elements = value | value 'and' elements | value 'also' elements
     string = '\"\"' | '\"' chars '\"'
     chars = char | char chars
     char = #'[^\"\\/\b\f\n\r\t]'
     number = int | int frac | int exp | int frac exp
     int = digit | digit1-9 digits | '-' digit | '-' digit1-9 digits
     digit1-9 = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
     digit = digit1-9 | '0'
     frac = '.' digits
     exp = very digits
     digits = digit | digit digits
     very = 'very' | 'very+' | 'very-' | 'VERY' | 'VERY+' | 'VERY-'
     true = 'yes'
     false = 'no'
     null = 'empty'"
     :auto-whitespace :standard))

(defn transform-dson
  [parse-tree]
  (insta/transform
    {:value identity
     :object (fn [_ members _] members)
     :members (fn ([[k v]] {k v})
                  ([[k v] _ members] (assoc members k v)))
     :pair (fn [k _ v] [k v])
     :array (fn ([_] [])
                ([_ elements _] (vec elements)))
     :elements (fn ([element] `(~element))
                   ([v _ elements] (cons v elements)))
     :string (fn ([_] "")
                 ([_ chars _] chars))
     :chars (fn ([char] (str char))
                ([char chars] (str char chars)))
     :char (fn [s] (str s)),
     :int (fn ([digit] digit)
              ([digit digits] (if (= digit "-") (- digit) (Float. (str digit digits)))))
     :digit1-9 identity
     :digit identity
     :digits (fn ([digit] (Float. digit))
                 ([digit digits] (Float. (str digit digits)))),
     :true (fn [_] true),
     :false (fn [_] false),
     :null (fn [_] nil)}
    parse-tree))

(defn dson
  [input]
  (->> input parse-dson transform-dson))

(defn -main
  [& args]
  (prn (dson "so many"))
  (prn (dson "so yes also no also empty many"))
  (prn (dson "such \"foo\" is \"bar\". \"doge\" is \"shibe\" wow"))
  (prn (dson "such \"foo\" is such \"shiba\" is \"inu\", \"doge\" is yes wow wow"))
  (prn (dson "such \"foo\" is so \"bar\" also \"baz\" and \"fizzbuzz\" many wow"))
  (prn (dson "such \"foo\" is 42very3 wow")))
