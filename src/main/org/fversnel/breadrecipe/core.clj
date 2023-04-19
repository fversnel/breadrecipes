(ns org.fversnel.breadrecipe.core
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.walk :as walk :refer [postwalk]]))

(def source
  (edn/read-string
   (slurp
    (io/resource "recipes.edn"))))

(def recipe-metadata
  (into
   {}
   (map-indexed
    (fn [index [ingredient metadata]]
      [ingredient
       (assoc
        metadata
        :order index
        :ingredient/type ingredient)]))
   (:org.fversnel.breadrecipes/metadata source)))

(let [resolve-operator {'+ +
                        '- -
                        '* *
                        '/ /}]
  (defn resolve-amount [recipe {:keys [proportion-formula] :as ingredient}]
    (letfn [(assoc-amount
              [amount]
              (assoc ingredient :amount amount))

            (keyword->proportion
              [x]
              (if (keyword? x)
                (:proportion (recipe x))
                x))

            (expand-formula [ingredient-type x]
              (cond
                (= ingredient-type x)
                x

                (keyword? x)
                (let [ingredient (recipe x)
                      proportion-formula (:proportion-formula ingredient)]
                  (if proportion-formula
                    (expand-formula (:ingredient/type ingredient) proportion-formula)
                    x))

                :else
                x))

            (resolve-formula [x]
              (if (sequential? x)
                (let [[operator & arguments] x
                      ;; Replace nil arguments with default values
                      arguments (map (fn [argument]
                                       (if (nil? argument)
                                         (cond
                                           (#{'+ '-} operator) 0
                                           (#{'* '/} operator) 1)
                                         argument))
                                     arguments)]
                  (apply (resolve-operator operator) arguments))
                x))

            (print-form [x] (println (:ingredient/type ingredient) "form:" x) x)]
      (if proportion-formula
        (->>
         proportion-formula
         (postwalk (partial expand-formula (:ingredient/type ingredient)))
         (postwalk keyword->proportion)
        ;;  print-form
         (postwalk resolve-formula)
         assoc-amount)

        ingredient))))

(defn expand-recipe [recipe]
  (let [expanded-recipe
        (into
         {}
         (map
          (fn [[ingredient proportion-or-value]]
            (let [metadata (recipe-metadata ingredient)]
              [ingredient
               (assoc
                metadata
                (if (contains? metadata :proportion-formula)
                  :proportion
                  :amount)
                proportion-or-value)])))
         recipe)]
    (->> (vals expanded-recipe)
         (map (partial resolve-amount expanded-recipe))
         (sort-by :order))))

(defn format-amount [{:keys [unit amount]}]
  (condp = unit
    :grams
    (str (format "%.1f" (float amount)) "g")

    :celcius
    (str amount "°C")

    :hours
    (str amount "h")

    :percentage
    (str (format "%.1f" (float (* amount 100))) "%")

    (str amount)))

(defn format-recipe [recipe]
  (transduce

   (comp
    (map
     (fn [{:keys [name percentage? proportion] :as ingredient}]
       (str name
            ": "
            (format-amount ingredient)
            (when percentage?
              (str
               " ("
               (format-amount {:unit :percentage :amount proportion})
               ")")))))

    (interpose \newline))

   str

   recipe))

(defn run [opts]
  (let [converted-recipes
        (transduce
         (comp
          (map (fn [recipe] (expand-recipe (merge opts recipe))))
          (map format-recipe)
          (interpose (str \newline \newline "---------------------------------" \newline)))
         str
         (:org.fversnel.breadrecipes/recipes source))]
    (println "Beschikbare recepten:")
    (println)
    (println converted-recipes)))