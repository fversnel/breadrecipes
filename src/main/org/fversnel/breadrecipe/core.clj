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

(defn resolve-amount [recipe x]
  (let [resolve-operator
        {'+ +
         '- -
         '* *
         '/ /}]

    (cond
      (list? x)
      (let [[operator base-value rest-formula] x

            operator (resolve-operator operator)
            ;; If base-value is a formula, process it recursively
            base-value (if (list? base-value)
                         (resolve-amount recipe base-value)
                         (get recipe base-value 0))
            rest-formula (resolve-amount recipe rest-formula)]
        (operator base-value (resolve-amount recipe rest-formula)))

      (number? x)
      x

      (keyword? x)
      (if-let [proportion (get proportions->formula x)]
        (resolve-amount recipe proportion)
        (get recipe x 0)))))

(defn ->percentage
  [amount]
  (str (format "%.1f" (* (float amount) 100)) "%"))

(defn grams->str [amount]
  (str (format "%.1f" (float amount)) "g"))

(defn format-recipe [recipe]
  (map
   (fn [[type description convert-amount]]
     (let [{:keys [proportion amount]} (type recipe)]
       (str description
            ": "
            (convert-amount amount)
            (when proportion
              (str " (" (->percentage proportion) ")"))
            \newline)))
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
           (let [amount (resolve-amount recipe ingredient)]
             (if (number? proportion)
               {:proportion proportion
                :amount amount}
               {:amount amount}))]))
   recipe))

;; TODO Format entire recipe


(defn run [opts]
  (println "Beschikbare recepten met" 
           (grams->str (:total-flour-in-grams opts)) 
           "meel:"
           \newline)
  (let [resolve-recipe (fn [recipe]
                         (resolve-recipe (merge opts recipe)))
        
        converted-recipes
        (map
         (comp format-recipe resolve-recipe)
         (:org.fversnel.breadrecipes recipes))]
    (run! println converted-recipes)))