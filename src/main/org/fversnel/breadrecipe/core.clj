(ns org.fversnel.breadrecipe.core
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def source
  (edn/read-string
   (slurp
    (io/resource "recipes.edn"))))

(def proportions->formula 
  (:org.fversnel.breadrecipes/proportions source))

(def recipes
  (:org.fversnel.breadrecipes/recipes source))

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
  (let [converted-recipes
        (transduce
         (comp
          (map (fn [recipe] (resolve-recipe (merge opts recipe))))
          (map format-recipe)
          (interpose (str \newline \newline)))
         str
         recipes)]
    (println "Beschikbare recepten met"
             (grams->str (:total-flour-in-grams opts))
             "meel:"
             \newline)
    (println converted-recipes)))