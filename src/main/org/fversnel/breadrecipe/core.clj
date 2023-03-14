(ns org.fversnel.breadrecipe.core
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def source
  (edn/read-string
   (slurp
    (io/resource "recipes.edn"))))

(def recipe-metadata
  (:org.fversnel.breadrecipes/metadata source))

(def recipes
  (:org.fversnel.breadrecipes/recipes source))

(defn resolve-amount
  [recipe {:keys [ingredient proportion proportion-formula] :as part}]
  (cond
    (some? proportion-formula)
    (comment
      (cond
        keyword? (recursive resolving formula)
        number? identity
        list? (loop)
        ))


    
    
    )




  (assoc
   part
   :amount
   (if proportion-formula
     (letfn [(resolve-formula [ingredient x]
               (println "resolving formula" x)
               (cond
                 (= x ingredient)
                 (:proportion (recipe ingredient))

                 (list? x)
                 (let [operator ({'+ +
                                  '- -
                                  '* *
                                  '/ /}
                                 (first x))
                       arguments (map resolve-formula (rest x))]
                   (apply operator arguments))

                 (number? x)
                 x))]
       (resolve-formula ingredient proportion-formula))

     proportion)))

(defn format-amount [unit amount]
  (condp = unit
    :grams
    (str (format "%.1f" (float amount)) "g")

    :percentage
    (str (format "%.1f" (float (* amount 100))) "%")

    (str amount)))

(defn format-recipe [recipe]
  (transduce

   (comp
    (filter
     (fn [[type _]]
       (contains? recipe type)))
    (map
     (fn [[_ {:keys [name proportion amount unit]}]]
       (str name
            ": "
            (format-amount unit amount)
            (when proportion
              (str " (" (format-amount :percentage proportion) ")")))))
    (interpose \newline))

   str

   recipe-metadata))

(defn resolve-recipe [recipe]
  (let [recipe-metadata (into {} recipe-metadata)
        recipe (into
                {}
                (map
                 (fn [[ingredient proportion]]
                   (let [metadata (ingredient recipe-metadata)]
                     [ingredient
                      (assoc
                       metadata
                       :ingredient ingredient
                       :proportion proportion)])))
                recipe)]
    (println recipe)

    (update-vals recipe (partial resolve-amount recipe))))

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
             (format-amount :grams (:total-flour-in-grams opts))
             "meel:"
             \newline)
    (println converted-recipes)))