(ns rakk.core
  (:require [loom.graph :as graph]
            [loom.attr :as attr]
            [loom.dataflow :as df]))


(defn value [g node]
  (attr/attr g node :value))


(defn values [g]
  (let [nodes (graph/nodes g)]
    (zipmap nodes (map #(value g %) nodes))))


(defn inputs [g]
  (remove #(attr/attr g % :function) (graph/nodes g)))


(defn flow [g starts]
  ;; df/dataflow-analysis will return a map of the new values that
  ;; were produced. What this map contains (and which parts of the
  ;; graph were analysed) depends on what start nodes are passed,
  ;; which is great, because you can use that property to avoid
  ;; recalculating values that are not affected by changes.
  ;;
  ;; This is a bit of a naive implementation because it does not
  ;; guarantee any order for the arguments
  (->> (df/dataflow-analysis
        {:start    starts
         :graph    g
         :join     identity
         :transfer (fn [node args]
                     (prn node args)
                     (if-let [f (attr/attr g node :function)]
                       {:node  node
                        :value (f (zipmap (map :node args)
                                          (map :value args)))}
                       {:node  node
                        :value (value g node)}))})
       (reduce (fn [m [_ {:keys [node value] :as v}]]
                 (assoc m node value)) {})))


(defn set-attrs
  "Set attributes of multiple nodes"
  [g attr new-attrs]
  (reduce (fn [gr [node value]]
            (attr/add-attr gr node attr value)) g new-attrs))


(defn set-values
  "Set :value attribute of multiple nodes"
  [g new-values]
  (set-attrs g :value new-values))


(defn set-value
  [g node value]
  (let [g (if (graph/has-node? g node)
            g
            (graph/add-nodes g node))]
    (attr/add-attr g node :value value)))


(defn set-function
  [g node value]
  (let [g (if (graph/has-node? g node)
            g
            (graph/add-nodes g node))]
    (-> g
        (attr/add-attr node :function value)
        (attr/remove-attr node :value))))


(defn clear-function
  [g node]
  (if (graph/has-node? g node)
    (attr/remove-attr g node :function)
    g))


(defn flow-starts
  "This function is necessary to compute the correct starts for
  dataflow analysis. If only one predecessor of a node is changed, it
  is necessary to pass all predecessors of the node as starts,
  otherwise you get null values passed to the transfer function."
  [g changed]
  (->> changed
       (mapcat (partial graph/successors g))
       (mapcat (partial graph/predecessors g))
       (set)))


(defn advance
  [g new-inputs]
  (let [new-graph  (set-values g new-inputs)
        out-values (flow new-graph (flow-starts g (keys new-inputs)))]
    (set-values new-graph out-values)))


(defn init [g]
  (set-values g (flow g (inputs g))))


(defn mutate! [graph-atom new-inputs]
  (swap! graph-atom advance new-inputs))


(comment
  (def gr
    (-> (graph/digraph [:a :c]
                       [:b :c]
                       [:c :d]
                       [:f :g])
        (attr/add-attr :a :value 10)
        (attr/add-attr :b :value 20)
        (attr/add-attr :c :function (fn [{:keys [a b]}] (+ a b)))
        (attr/add-attr :d :function (fn [{:keys [c]}] (* 2 c)))

        (attr/add-attr :f :value 33)
        (attr/add-attr :g :function (fn [{:keys [f]}] (* 100 f)))))

  (-> gr init (advance {:a 100})))
