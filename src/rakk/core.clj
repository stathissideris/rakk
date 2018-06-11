(ns rakk.core
  (:require [loom.graph :as graph]
            [loom.attr :as attr]
            [loom.dataflow :as df]))


(defn value [g node]
  (try ;;in case it doesn't exist
    (attr/attr g node :value)
    (catch Exception _ nil)))


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
  (if-not (seq starts)
    {}
    (->> (df/dataflow-analysis
          {:start    starts
           :graph    g
           :join     identity
           :transfer (fn [node args]
                       (if-let [f (attr/attr g node :function)]
                         (if (every? nil? args) ;;function node is passed empty args, means it's a start node itself
                           {:node  node
                            :value (attr/attr g node :value)}
                           {:node  node
                            :value (f (zipmap (map :node args)
                                              (map :value args)))})
                         {:node  node
                          :value (value g node)}))})
         (reduce (fn [m [_ {:keys [node value] :as v}]]
                   (assoc m node value)) {}))))


(defn set-attrs
  "Set attributes of multiple nodes"
  [g attr new-attrs]
  (reduce (fn [gr [node value]]
            (attr/add-attr gr node attr value)) g new-attrs))


(defn ensure-node [g node]
  (if (graph/has-node? g node)
    g
    (graph/add-nodes g node)))


(defn set-value
  [g node value]
  (-> g
      (ensure-node node)
      (attr/add-attr node :value value)))


(defn set-values
  "Set :value attribute of multiple nodes"
  [g new-values]
  (reduce (fn [g [node value]]
            (set-value g node value))
          g new-values))


(defn set-function
  [g node value]
  (-> g
      (ensure-node node)
      (attr/add-attr node :function value)
      (attr/remove-attr node :value)))


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
  [g new-inputs extra-starts]
  (let [new-graph  (set-values g new-inputs)
        out-values (flow new-graph (into (flow-starts g (keys new-inputs))
                                         extra-starts))]
    (set-values new-graph out-values)))


(defn init [g]
  (set-values g (flow g (inputs g))))


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
