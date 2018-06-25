(ns rakk.core-test
  (:require [rakk.core :as sut]
            [loom.graph :as graph]
            [clojure.test :refer :all]))

(deftest basic-test
  (let [g (-> (sut/make)
              (sut/set-value :a 10)
              (sut/set-value :b 20)
              (sut/set-function :c (fn [{:keys [a b]}] (+ a b)))
              (graph/add-edges [:a :c] [:b :c]))]

    (is (= #{:a :b} (set (sut/inputs g))))

    (is (= [{:node :b :value 20}
            {:node :a :value 10}
            {:node :c :value 30}]
           (sut/flow g (sut/inputs g))))

    (is (= {:b 20 :a 10 :c 30} (sut/values (sut/init g))))

    (is (= {:c 120 :b 20 :a 100} (-> g (sut/advance {:a 100} []) sut/values)))

    (is (= {:c 300 :b 200 :a 100}
           (-> g
               (sut/advance {:a 100} [])
               (sut/advance {:b 200} [])
               sut/values)))

    (is (= (-> g
               (sut/advance {:a 100 :b 200} [])
               sut/values)
           (-> g
               (sut/advance {:a 100} [])
               (sut/advance {:b 200} [])
               sut/values)))

    (is (= {:c 200, :b 20, :a 10}
           (-> g
               (sut/set-function :c (fn [{:keys [a b]}] (* a b)))
               sut/init
               sut/values)))))

(deftest error-handling-test
  (let [g (-> (sut/make)
              (sut/set-value :a 10)
              (sut/set-value :b 20)
              (sut/set-function :c (fn [{:keys [a b]}] (+ a b)))
              (sut/set-function :d (fn [{:keys [c]}] (/ 55 c)))
              (sut/set-function :e (fn [{:keys [d]}] (+ d 5)))
              (graph/add-edges [:a :c] [:b :c] [:c :d] [:d :e]))]

    (is (= {:c 30 :b 20 :d 11/6 :a 10 :e 41/6} (sut/values (sut/init g))))
    (is (= {:a 0 :b 0 :c 0 :d :rakk/error :e :rakk/error} (-> g (sut/advance {:a 0 :b 0} []) sut/values)))
    (is (instance? Exception (-> g (sut/advance {:a 0 :b 0} []) (rakk.core/error :d))))
    (is (= :d (-> g (sut/advance {:a 0 :b 0} []) (rakk.core/error :e) ex-data :upstream-errors first :node)))))
