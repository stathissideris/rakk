(ns rakk.core
  (:refer-clojure :exclude [remove])
  (:require [loom.graph :as graph]
            [loom.attr :as attr]
            [loom.dataflow :as df]))


(defn value [g node]
  (try ;;in case it doesn't exist
    (attr/attr g node :value)
    (catch Exception _ nil)))


(defn error [g node]
  (try ;;in case it doesn't exist
    (attr/attr g node :error)
    (catch Exception _ nil)))


(defn error? [g node]
  (some? (error g node)))


(defn error-info [g node]
  (let [error? (error? g node)]
    {:rakk/error?     error?
     :rakk/error      (when error? (error g node))
     :rakk/error-type (cond (not error?)                                                :none
                            (some-> g (error node) ex-data :rakk/secondary-error some?) :secondary
                            :else                                                       :primary)}))


(defn values [g]
  (let [nodes (graph/nodes g)]
    (zipmap nodes (map #(value g %) nodes))))


(defn inputs [g]
  (clojure.core/remove #(attr/attr g % :function) (graph/nodes g)))


(defn- apply-fn
  "Calls f by passing a map of node name to node value. Catch all
  exceptions and return them wrapped in a map under the ::error key."
  [f args]
  (try (f (zipmap (map :node args)
                  (map :value args)))
       (catch Exception e
         {::error e})))


(defn- transfer
  "Transfer function for dataflow analysis. "
  [g node args]
  (if-let [f (attr/attr g node :function)]
    (if (and (not (empty? args)) (every? nil? args))
      ;; function node is passed empty args, means it's a start node
      ;; itself (but not an input node). In that case we just use the
      ;; cached value
      (if (error? g node)
        {:node  node
         :value (value g node)
         :error (error g node)}
        {:node  node
         :value (value g node)})
      (if (some :error args)
        {:node  node
         :value :rakk/error
         :error (ex-info "Some upstream cells contain errors"
                         {:rakk/secondary-error true
                          :rakk/upstream-errors (filter :error args)})}
        (let [v (apply-fn f args)]
          (if (::error v)
            {:node node :value :rakk/error :error (::error v)}
            {:node node :value v}))))
    {:node  node
     :value (value g node)}))


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
           :transfer (partial transfer g)})
         vals)))


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


(defn set-error
  [g node error]
  (let [g (ensure-node g node)]
    (if error
      (attr/add-attr g node :error error)
      (attr/remove-attr g node :error))))


(defn set-errors
  [g new-errors]
  (reduce (fn [g [node error]]
            (set-error g node error))
          g new-errors))


(defn set-function
  [g node function]
  (-> g
      (ensure-node node)
      (attr/add-attr node :function function)
      (attr/remove-attr node :value)))


(defn clear-function
  [g node]
  (if (graph/has-node? g node)
    (attr/remove-attr g node :function)
    g))


(defn clear-functions
  [g nodes]
  (reduce clear-function g nodes))


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
  (let [new-graph (-> g
                      (set-values new-inputs)
                      (clear-functions (keys new-inputs))) ;;cleanup in case it used to be a function
        outcomes  (flow new-graph (into (flow-starts g (keys new-inputs))
                                        extra-starts))]
    (-> new-graph
        (set-values (into {} (map (juxt :node :value) outcomes)))
        (set-errors (into {} (map (juxt :node :error) outcomes))))))


(defn kill
  "Remove a set of nodes so that downstream nodes enter an error state because
  they are referring to dead nodes."
  [g nodes]
  (let [downstream (mapcat (partial graph/successors g) nodes)]
    (as-> g _
        (apply graph/remove-nodes _ nodes)
        (update _ :attrs #(apply dissoc % nodes))
        (set-errors _ (zipmap downstream (repeat ::ref)))
        (advance _ nil downstream))))

(defn remove
  "Remove nodes in an orderly manner, by removing them from the graph and also
  removing their downstream edges. Used for when downstream nodes are tolerant
  of removals of upstream deps."
  [g nodes]
  (let [downstream (mapcat (partial graph/successors g) nodes)]
    (as-> g _
      (apply graph/remove-nodes _ nodes)
      (update _ :attrs #(apply dissoc % nodes))
      (advance _ nil (mapcat (partial graph/predecessors _) downstream)))))


(defn recalc [g]
  (let [outcomes (flow g (inputs g))]
    (-> g
        (set-values (into {} (map (juxt :node :value) outcomes)))
        (set-errors (into {} (map (juxt :node :error) outcomes))))))


(defn make [] (graph/digraph))


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

  (-> gr recalc (advance {:a 100})))
