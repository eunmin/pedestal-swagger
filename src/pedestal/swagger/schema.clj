(ns pedestal.swagger.schema
  (:require [schema.coerce :as c]
            [schema.core :as s]
            [schema.utils :as u]
            [io.pedestal.impl.interceptor :refer [terminate]]))

(defn explain
  "Tries to give a meaningful explanation for each error key. It
   works better if each schema is a named schema, otherwise defaults
   to the technical validation-error explanation."
  [e]
  (cond
    (instance? schema.utils.NamedError e)
    (let [error (.-error e)
          name (.-name e)]
      (if (map? error)
        (explain error)
        name))

    (map? e)
    (into {} (for [[k v] e]
               [k (explain v)]))

    :default
    (pr-str e)))

(defn- throw-if-error [schema value result]
  (if (u/error? result)
    (throw (ex-info
            (format "Value does not match schema: %s" (pr-str result))
            {:schema schema :value value :error result}))
    result))

(defn- loosen [schema]
  (assoc schema s/Any s/Any))

(def ^:private schema->param
  {:body     :body-params
   :formData :form-params
   :path     :path-params
   :query    :query-params
   :header   :headers})

(def ^:private loose-schema?
  #{:query :header})

(defn- ->request-schema [schema]
  (->> (for [[k v] schema]
         [(schema->param k)
          (if (loose-schema? k)
            (loosen v)
            v)])
       (into {})
       (loosen)))

(defn- with-request-defaults [r]
  (merge
   {:body-params  nil
    :form-params  {}
    :path-params  {}
    :query-params {}
    :headers      {}} r))

(defn make-coerce-request
  "Builds a coerce-request fn used in a swagger interceptor.
  Custom coercions can be passed as a parameter."
  ([] (make-coerce-request c/string-coercion-matcher))
  ([coercions]
   (fn [schema request]
     (let [result ((c/coercer (->request-schema schema) coercions)
                   (with-request-defaults request))]
       (throw-if-error schema request result)))))

(defn- ->response-schema [{:keys [headers schema]}]
  (loosen
   (merge
    (when schema
      {:body schema})
    (when headers
      {:headers (loosen headers)}))))

(defn- with-response-defaults [r]
  (merge
   {:headers {}
    :body nil}
   r))

(defn make-validate-response
  "Builds a validate-response fn used in a swagger interceptor.
  Custom coercions can be passed as a parameter."
  ([] (make-validate-response {}))
  ([coercions]
   (fn [schema response] 
     (let [result ((c/coercer (->response-schema schema) coercions)
                   (with-response-defaults response))]
       (throw-if-error schema response result)))))
