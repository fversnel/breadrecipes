(ns org.fversnel.breadrecipe.core
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def recipes
  (edn/read-string
   (slurp
    (io/resource "recipes.edn"))))

(def proportions->formula
  {:flour '(* :flour :total-flour-in-grams)
   :water '(- (* :water :total-flour-in-grams) :preferment/water)
   :yeast '(* :yeast :total-flour-in-grams)
   :salt '(* :salt :total-flour-in-grams)

   :preferment/flour '(* :preferment/flour :total-flour-in-grams)
   :preferment/water '(* :preferment/water :preferment/flour)
   :preferment/yeast '(* :preferment/yeast :preferment/flour)})

(defn resolve-amount [recipe ingredient x]
  (cond
    (number? x)
    x

    (keyword? x)
    (if-let [proportion (proportions->formula x)]
      (resolve-amount recipe x proportion)
      (get recipe x 0))

    (list? x)
    (let [operator ({'+ +
                     '- -
                     '* *
                     '/ /}
                    (first x))
          
          arguments (map
                     #(if (= % ingredient)
                        (get recipe % 0)
                        (resolve-amount recipe ingredient %))
                     (rest x))]
      (apply operator arguments))

    :else
    x))

(defn ->percentage [amount]
  (str (format "%.1f" (* (float amount) 100)) "%"))

(defn grams->str [amount]
  (str (format "%.1f" (float amount)) "g"))

(defn format-recipe [recipe]
  (transduce

   (comp
    (filter 
     (fn [[type _ _]]
       (contains? recipe type)))
    (map
     (fn [[type description convert-amount]]
       (let [{:keys [proportion amount]} (type recipe)]
         (str description
              ": "
              (convert-amount amount)
              (when proportion
                (str " (" (->percentage proportion) ")"))))))
    (interpose \newline))

   str

   [[:name "Naam" str]

    [:flour "Meel" grams->str]
    [:water "Water" grams->str]
    [:yeast "Gist" grams->str]
    [:salt "Zout" grams->str]

    [:preferment/type "Preferment type" name]
    [:preferment/flour "Preferment meel" grams->str]
    [:preferment/water "Preferment water" grams->str]
    [:preferment/yeast "Preferment gist" grams->str]]))

(defn resolve-recipe [recipe]
  (into
   {}
   (map (fn [[ingredient proportion]]
          [ingredient
           (let [amount (resolve-amount recipe ingredient ingredient)]
             (if (number? proportion)
               {:proportion proportion
                :amount amount}
               {:amount amount}))]))
   recipe))

(defn run [opts]
  (println "Beschikbare recepten met"
           (grams->str (:total-flour-in-grams opts))
           "meel:"
           \newline)
  (let [resolve-recipe (fn [recipe]
                         (resolve-recipe (merge opts recipe)))

        converted-recipes
        (transduce
         (comp 
          (map resolve-recipe)
          (map format-recipe)
          (interpose (str \newline \newline)))
          str
          (:org.fversnel.breadrecipes recipes))]
    (println converted-recipes)))